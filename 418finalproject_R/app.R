library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinyWidgets)
library(scales)  # For comma formatting
library(usmap)   # For US heatmap
library(shinycssloaders)  # For loading spinners
library(DBI)
library(RSQLite)

# Establish database connection
con <- dbConnect(RSQLite::SQLite(), "zip_c.db")

# Function to load filtered data from the database
load_data <- function(housing_type, states, start_date, end_date) {
  start_year <- as.numeric(format(start_date, "%Y"))
  start_month <- as.numeric(format(start_date, "%m"))
  end_year <- as.numeric(format(end_date, "%Y"))
  end_month <- as.numeric(format(end_date, "%m"))
  
  query <- paste0("SELECT Zipcode, StateName, Metro, Year, Month, Avg_Pricing_Month 
                   FROM ", housing_type, 
                  " WHERE (Year > ", start_year, " 
                    OR (Year = ", start_year, " AND Month >= ", start_month, ")) 
                    AND (Year < ", end_year, " 
                    OR (Year = ", end_year, " AND Month <= ", end_month, "))")
  if (length(states) > 0) {
    states_str <- paste(shQuote(states, type = "sh"), collapse = ", ")
    query <- paste0(query, " AND StateName IN (", states_str, ")")
  }
  dbGetQuery(con, query)
}

# Function to get unique states from the selected table
load_unique_states <- function(housing_type) {
  dbGetQuery(con, paste("SELECT DISTINCT StateName FROM", housing_type))
}

# Function to get unique years from the selected table
load_unique_years <- function(housing_type) {
  dbGetQuery(con, paste("SELECT DISTINCT Year FROM", housing_type))
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
      .dataTable tbody tr:nth-child(odd) {
        background-color: #d3e9f9;
      }
      .dataTable tbody tr:nth-child(even) {
        background-color: white;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #007bff !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
        color: #888 !important;
      }
      .dataTables_wrapper .dataTables_length select, 
      .dataTables_wrapper .dataTables_filter input {
        color: #007bff !important;
      }
      .dataTables_wrapper .dataTables_info {
        color: black !important;
      }
      .irs-single, .irs-bar, .irs-bar-edge, .irs-slider, .irs-from, .irs-to, .irs-min, .irs-max {
        background: #007bff;
        border-color: #007bff;
      }
      .slider { width: 100%; margin-top: 20px; }
      .irs {
        height: 50px; /* Increase slider height */
      }
      .irs-line {
        height: 14px; /* Increase the line height */
      }
      .irs-handle {
        width: 30px; /* Increase handle width */
        height: 30px; /* Increase handle height */
        top: 18px; /* Center handle */
      }
    "))
  ),
  titlePanel("Housing Price Dashboard"),
  fluidRow(
    column(3, 
           selectInput("housingType", "Select Housing Type:",
                       choices = c("1 Bedroom" = "zip_1b", "2 Bedrooms" = "zip_2b", "3 Bedrooms" = "zip_3b",
                                   "4 Bedrooms" = "zip_4b", "5 Bedrooms" = "zip_5b", "Condo" = "zip_condo", "Family" = "zip_family"))),
    column(3, 
           pickerInput("stateInput", "Select State:", 
                       choices = NULL, 
                       options = list(`actions-box` = TRUE), 
                       multiple = TRUE)),
    column(4, 
           dateRangeInput("dateRange", "Select Date Range:",
                          start = as.Date("2019-01-01"), end = as.Date("2024-04-30"),
                          min = as.Date("2000-01-01"),
                          max = as.Date("2024-04-30"))),
    column(2, 
           div(style = "margin-top: 25px;",
               actionButton("filterButton", "Apply Filters", style = "background-color: #007bff; color: white; border: none;"))
    )
  ),
  hr(),
  tabsetPanel(
    tabPanel("Table", DTOutput("tableOutput")),
    tabPanel("Plot", 
             withSpinner(plotlyOutput("topPricePlotOutput"), type = 6, color = "#007bff", size = 1),
             withSpinner(plotlyOutput("lowPricePlotOutput"), type = 6, color = "#007bff", size = 1),
             withSpinner(plotlyOutput("histogramByStateOutput"), type = 6, color = "#007bff", size = 1),
             withSpinner(plotlyOutput("avgPricePlotOutput"), type = 6, color = "#007bff", size = 1),
             div(class = "slider",
                 uiOutput("monthSlider")),
             withSpinner(plotlyOutput("heatmapOutput"), type = 6, color = "#007bff", size = 1)
    ),
    tabPanel("Summary", 
             verbatimTextOutput("summaryOutput"))
  )
)

server <- function(input, output, session) {
  observeEvent(input$housingType, {
    # Load unique states from the selected table and update pickerInput
    df <- load_unique_states(input$housingType)
    updatePickerInput(session, "stateInput", choices = df$StateName, selected = c("CA", "NY"))
  })
  
  observeEvent(input$filterButton, {
    # Adjust slider based on selected date range
    start_date <- input$dateRange[1]
    end_date <- input$dateRange[2]
    date_seq <- seq(start_date, end_date, by = "month")
    
    output$monthSlider <- renderUI({
      sliderInput("yearInput", "Select Month and Year for Heatmap:", 
                  min = min(date_seq), max = max(date_seq), value = min(date_seq), 
                  timeFormat = "%b %Y", animate = animationOptions(interval = 1000, loop = TRUE), step = 1)
    })
  })
  
  observeEvent(input$dateRange, {
    start_date <- input$dateRange[1]
    end_date <- input$dateRange[2]
    date_seq <- seq(start_date, end_date, by = "month")
    
    output$monthSlider <- renderUI({
      sliderInput("yearInput", "Select Month and Year for Heatmap:", 
                  min = min(date_seq), max = max(date_seq), value = min(date_seq), 
                  timeFormat = "%b %Y", animate = animationOptions(interval = 1000, loop = TRUE), step = 1)
    })
  })
  
  filteredData <- reactive({
    req(input$filterButton)
    isolate({
      start_date <- as.Date(input$dateRange[1])
      end_date <- as.Date(input$dateRange[2])
      df <- load_data(input$housingType, input$stateInput, start_date, end_date)
      df <- df %>%
        mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), "%Y-%m-%d")) %>%
        filter(!is.na(Avg_Pricing_Month)) %>%
        filter(!is.na(Date))  # Remove rows with NA in Date column
      df
    })
  })
  
  output$tableOutput <- renderDT({
    datatable(filteredData() %>% select(-Date), options = list(serverSide = TRUE))
  })
  
  output$topPricePlotOutput <- renderPlotly({
    df <- filteredData()
    top_zipcodes <- df %>% 
      group_by(Zipcode) %>% 
      summarise(MaxPrice = max(Avg_Pricing_Month, na.rm = TRUE)) %>% 
      arrange(desc(MaxPrice)) %>% 
      slice(1:10) %>% 
      pull(Zipcode)
    df_top <- df %>% filter(Zipcode %in% top_zipcodes)
    p <- ggplot(df_top, aes(x = Date, y = Avg_Pricing_Month, color = as.factor(Zipcode), text = Metro)) + 
      geom_line() + 
      labs(title = "Top 10 Price Zipcodes Over Time", x = "Date", y = "Price", color = "Zipcode") +
      theme_minimal() +
      scale_y_continuous(labels = comma)  # Format prices with commas
    ggplotly(p, tooltip = c("x", "y", "color", "text"))
  })
  
  output$lowPricePlotOutput <- renderPlotly({
    df <- filteredData()
    low_zipcodes <- df %>% 
      group_by(Zipcode) %>% 
      summarise(MinPrice = min(Avg_Pricing_Month, na.rm = TRUE)) %>% 
      arrange(MinPrice) %>% 
      slice(1:10) %>% 
      pull(Zipcode)
    df_low <- df %>% filter(Zipcode %in% low_zipcodes)
    p <- ggplot(df_low, aes(x = Date, y = Avg_Pricing_Month, color = as.factor(Zipcode), text = Metro)) + 
      geom_line() + 
      labs(title = "Lowest 10 Price Zipcodes Over Time", x = "Date", y = "Price", color = "Zipcode") +
      theme_minimal() +
      scale_y_continuous(labels = comma)  # Format prices with commas
    ggplotly(p, tooltip = c("x", "y", "color", "text"))
  })
  
  output$histogramByStateOutput <- renderPlotly({
    df <- filteredData()
    p <- ggplot(df, aes(x = Avg_Pricing_Month, fill = StateName)) + 
      geom_histogram(bins = 30, alpha = 0.6, position = "identity") + 
      labs(title = "Distribution of Prices by State", x = "Price", y = "Count") +
      theme_minimal() +
      scale_x_continuous(labels = comma) +  # Format numbers with commas
      facet_wrap(~ StateName)
    ggplotly(p)
  })
  
  output$avgPricePlotOutput <- renderPlotly({
    df <- filteredData()
    df_avg <- df %>%
      group_by(StateName, Date) %>%
      summarise(AvgPrice = mean(Avg_Pricing_Month, na.rm = TRUE), .groups = 'drop')
    p <- ggplot(df_avg, aes(x = Date, y = AvgPrice, color = StateName)) + 
      geom_line() + 
      labs(title = "Average Price by State Over Time", x = "Date", y = "Average Price") +
      theme_minimal() +
      scale_y_continuous(labels = comma)  # Format numbers with commas
    ggplotly(p)
  })
  
  output$heatmapOutput <- renderPlotly({
    df <- filteredData()
    selected_date <- input$yearInput
    selected_year <- as.numeric(format(selected_date, "%Y"))
    selected_month <- as.numeric(format(selected_date, "%m"))
    df_year <- df %>%
      filter(Year == selected_year & Month == selected_month) %>%
      group_by(StateName) %>%
      summarise(AvgPrice = mean(Avg_Pricing_Month, na.rm = TRUE), .groups = 'drop') %>%
      filter(!is.na(AvgPrice))  # Remove rows with NA in AvgPrice
    
    names(df_year)[names(df_year) == "StateName"] <- "state"
    
    plot_usmap(data = df_year, values = "AvgPrice", labels = TRUE) + 
      scale_fill_continuous(name = "Average Price", label = comma) + 
      theme(legend.position = "right") +
      labs(title = paste("Average Price by State in", format(selected_date, "%B %Y")))
  })
  
  output$summaryOutput <- renderPrint({
    df <- filteredData()
    summary(df)
  })
}

shinyApp(ui, server)
