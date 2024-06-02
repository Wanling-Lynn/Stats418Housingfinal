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
library(data.table)  # For faster data manipulation
library(jsonlite)

# Helper function to convert named vectors to named lists
convert_to_named_list <- function(x) {
  if (is.vector(x) && !is.null(names(x))) {
    return(as.list(x))
  }
  return(x)
}

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

# Caching data to avoid repeated database queries
data_cache <- reactiveValues()

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
      .align-bottom {
        display: flex;
        align-items: flex-end;
        height: 100%; 
      }
      .vertical-align {
        margin-bottom: 1px;
      }
      .full-width {
        width: 100%;
      }
      .date-range-container {
        width: 100%;
      }
    "))
  ),
  titlePanel("Price Data Dashboard"),
  fluidRow(
    column(12,
           h4("Filters", style = "color: #63C5DA;"),
           hr(),
           fluidRow(
             column(3,
                    selectInput("housingType", "Select Housing Type:",
                                choices = c("1 Bedroom" = "zip_1b", "2 Bedrooms" = "zip_2b", "3 Bedrooms" = "zip_3b",
                                            "4 Bedrooms" = "zip_4b", "5 Bedrooms" = "zip_5b", "Condo" = "zip_condo", "Family" = "zip_family"))
             ),
             column(3,
                    pickerInput("stateInput", "Select State:", 
                                choices = NULL, 
                                options = list(`actions-box` = TRUE), 
                                multiple = TRUE)
             ),
             column(3,
                    div(class = "date-range-container",
                        dateRangeInput("dateRange", "Select Date Range:",
                                       start = as.Date("2019-04-01"), end = as.Date("2024-04-30"),
                                       min = as.Date("2000-01-01"), max = as.Date("2024-04-30"),
                                       format = "yyyy-mm",
                                       width = '100%')
                    )
             ),
             column(3,
                    div(style = "margin-top: 25px; width: 100%;",
                        actionButton("filterButton", "Apply Filters", style = "background-color: #63C5DA; color: white; border: none; width: 100%;")
                    )
             )
           ),
           hr(),
           p("It may take some time to process if you select more states ❤️")
    )
  ),
  fluidRow(
    column(12,
           DTOutput("tableOutput")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$housingType, {
    # Load unique states from the selected table and update pickerInput
    df <- load_unique_states(input$housingType)
    updatePickerInput(session, "stateInput", choices = df$StateName, selected = c("CA", "NY"))
  })
  
  filteredData <- reactive({
    req(input$filterButton)
    isolate({
      # Cache data to avoid repeated database queries
      key <- paste(input$housingType, paste(input$stateInput, collapse = ","), input$dateRange[1], input$dateRange[2], sep = "_")
      if (!is.null(data_cache[[key]])) {
        df <- data_cache[[key]]
      } else {
        start_date <- as.Date(paste0(format(input$dateRange[1], "%Y-%m"), "-01"))
        end_date <- as.Date(paste0(format(input$dateRange[2], "%Y-%m"), "-01"))
        df <- load_data(input$housingType, input$stateInput, start_date, end_date)
        df <- df %>%
          mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
          filter(!is.na(Avg_Pricing_Month))  # Remove rows with non-finite Avg_Pricing_Month
        setDT(df)  # Convert to data.table for faster manipulation
        data_cache[[key]] <- df
      }
      df
    })
  })
  
  output$tableOutput <- renderDT({
    datatable(filteredData(), options = list(serverSide = TRUE, processing = TRUE, scrollX = TRUE))
  })
}

shinyApp(ui, server)
