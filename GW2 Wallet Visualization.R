library(shiny)
library(shinydashboard)
library(httr2)
library(jsonlite)
library(dplyr)
library(highcharter)
library(glue)

#API
api_base <- "https://api.guildwars2.com/v2"

gel_token_info <- function(token) {
  resp <- request(glue("{api_base}/tokeninfo")) %>%
    req_headers(Authorization = paste("Bearer", token)) %>%
    req_perform()
  if (resp_status(resp) != 200) return(NULL)
  resp_body_json(resp, simplifyVector = TRUE)
}

gel_get_wallet <- function(token) {
  resp <- request(glue("{api_base}/account/wallet")) %>%
    req_headers(Authorization = paste("Bearer", token)) %>%
    req_perform()
  if (resp_status(resp) != 200) return(NULL)
  df <- resp_body_json(resp, simplifyVector = TRUE) %>% as_tibble()
  ids <- df$id
  meta <- fromJSON(glue("{api_base}/currencies?ids={paste(ids, collapse=',')}"), simplifyDataFrame = TRUE)
  df %>% left_join(meta %>% select(id, name), by = "id")
}

gel_get_materials <- function(token) {
  resp <- request(glue("{api_base}/account/materials")) %>%
    req_headers(Authorization = paste("Bearer", token)) %>%
    req_perform()
  if (resp_status(resp) != 200) return(NULL)
  mat <- resp_body_json(resp, simplifyVector = TRUE) %>% as_tibble()
  ids <- unique(mat$id)
  chunks <- split(ids, ceiling(seq_along(ids)/200))
  meta <- bind_rows(lapply(chunks, function(ids_chunk) {
    fromJSON(glue("{api_base}/items?ids={paste(ids_chunk, collapse=',')}"), simplifyDataFrame = TRUE)
  }))
  mat %>% left_join(meta %>% select(id, name, type), by = "id")
}
#Material Values
gel_compute_material_value <- function(mat_df) {
  ids <- unique(mat_df$id)
  chunks <- split(ids, ceiling(seq_along(ids)/200))
  price_df <- bind_rows(lapply(chunks, function(chunk_ids) {
    url <- glue("{api_base}/commerce/prices?ids={paste(chunk_ids, collapse=',')}")
    fromJSON(url, flatten = TRUE) %>% as_tibble() %>%
      select(id, sell_price = sells.unit_price)
  }))
  joined <- mat_df %>% left_join(price_df, by = "id")
  sum(joined$count * joined$sell_price, na.rm = TRUE)
}

compute_wallet_copper <- function(wallet_df) {
  wallet_df %>%
    mutate(copper = case_when(
      id == 1 ~ value,
      id == 2 ~ value * 100,
      id == 3 ~ value * 10000,
      TRUE    ~ 0
    )) %>%
    pull(copper) %>% sum(na.rm = TRUE)
}

#UI
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = span(icon("gem"), strong("GW2 Wealth Tracker"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt"))
    ),
    hr(),
    textInput("api_key", "GW2 API Key", width = "100%"),
    actionButton("load", "Load Data", icon = icon("sync")),
    br(), br(),
    verbatimTextOutput("status")
  ),
  dashboardBody(
    tags$style(HTML("box.box-solid.box-primary>.box-header {
      color:#fff;
        background:#c0392b}")),
    
    tags$head(tags$style(HTML(
      ".skin-red .main-header .logo { background: #c0392b; color: #fff; }
       .skin-red .main-header .navbar { background: #a93226; }
       .sidebar { background: #fff; }
       .sidebar .sidebar-menu > li.active > a,
       .sidebar .sidebar-menu > li:hover > a { background: #c0392b; color: #fff; }
       .content-wrapper, .right-side { background: #ffffff; }"
    ))),
    tabItems(
      tabItem(tabName = "overview",
              # Info boxes
              fluidRow(
                infoBoxOutput("wallet_box",    width = 4),
                infoBoxOutput("materials_box", width = 4),
                infoBoxOutput("networth_box",  width = 4)
              ),
              # Wallet & Materials charts side by side
              fluidRow(
                box(
                  title = "Wallet Balances", status = "danger", solidHeader = TRUE,
                  width = 6, height = "420px",
                  highchartOutput("wallet_plot", height = "380px")
                ),
                box(
                  title = "Material Inventory", status = "danger", solidHeader = TRUE,
                  width = 6, height = "520px",
                  highchartOutput("material_plot", height = "480px")
                )
              )
      )
    )
  )
)

#Server Stuff
server <- function(input, output, session) {
  data <- reactiveValues(
    wallet     = NULL,
    materials  = NULL,
    wallet_cu  = 0,
    mat_cu     = 0,
    net_cu     = 0
  )
  
  observeEvent(input$load, {
    output$status <- renderText("🔍 Validating API Key...")
    if (is.null(gel_token_info(input$api_key))) {
      output$status <- renderText("❌ Invalid API Key.")
      return()
    }
    output$status <- renderText("⏳ Loading data...")
    
    data$wallet    <- gel_get_wallet(input$api_key)
    data$materials <- gel_get_materials(input$api_key)
    
    data$wallet_cu <- compute_wallet_copper(data$wallet)
    data$mat_cu    <- gel_compute_material_value(data$materials)
    data$net_cu    <- data$wallet_cu + data$mat_cu
    
    output$status <- renderText("✅ Data ready.")
  })
  
  output$wallet_box <- renderInfoBox({
    gold <- data$wallet_cu / 10000
    infoBox("Wallet (g)", round(gold, 2), icon = icon("coins"), color = "red", fill = TRUE)
  })
  output$materials_box <- renderInfoBox({
    gold <- data$mat_cu / 10000
    infoBox("Materials (g)", round(gold, 2), icon = icon("boxes"), color = "red", fill = TRUE)
  })
  output$networth_box <- renderInfoBox({
    gold <- data$net_cu / 10000
    infoBox("Total Net Worth (g)", round(gold, 2), icon = icon("gem"), color = "red", fill = TRUE)
  })
  
  output$wallet_plot <- renderHighchart({
    req(data$wallet)
    df <- data$wallet %>% arrange(desc(value))
    hchart(df, "column", hcaes(x = name, y = value)) %>%
      hc_colors("#c0392b") %>%
      hc_title(text = "Wallet Balances") %>%
      hc_xAxis(labels = list(rotation = -45)) %>%
      hc_plotOptions(column = list(borderColor = "#ffffff", borderWidth = 1))
  })
  
  output$material_plot <- renderHighchart({
    req(data$materials)
    df <- data$materials %>% arrange(desc(count))
    
#Treemap
    highchart() %>%
      hc_add_series(
        type = "treemap",
        layoutAlgorithm = "squarified",
        data = purrr::transpose(list(
          name       = df$name,
          value      = df$count,
          colorValue = df$count
        )),
        borderColor = "#ffffff",
        borderWidth = 1,
        dataLabels = list(
          enabled       = TRUE,
          align         = "center",
          verticalAlign = "middle",
          style         = list(fontSize = "11px", color = "#000", textOutline = "none")
        )
      ) %>%
      hc_colorAxis(
        minColor = "#ffffff",
        maxColor = "#c0392b"
      ) %>%
      hc_title(text = "Material Inventory") %>%
      hc_tooltip(pointFormat = "<b>{point.name}</b><br>Count: {point.value}")
  })
}

shinyApp(ui, server)
