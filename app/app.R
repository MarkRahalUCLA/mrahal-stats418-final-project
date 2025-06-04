# Required packages
library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)  # Add back for local plotting
library(lubridate)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(base64enc)  # Add base64enc for decoding

# API Configuration
api_base_url <- "https://co2-prediction-api-577479811788.us-west1.run.app"

# Helper function for API requests
make_api_request <- function(endpoint, method = "GET", query = NULL, body = NULL) {
  url <- paste0(api_base_url, endpoint)
  
  response <- if (method == "GET") {
    GET(url, query = query)
  } else {
    POST(url, body = body, encode = "json")
  }
  
  if (status_code(response) == 200) {
    return(fromJSON(rawToChar(response$content)))
  } else {
    stop(paste("API request failed with status:", status_code(response)))
  }
}

# Helper function to render base64 plot
render_base64_plot <- function(base64_str, width = 10, height = 6) {
  # Create a temporary file
  temp_file <- tempfile(fileext = ".png")
  
  # Decode and write the base64 string to the temp file
  writeBin(base64decode(base64_str), temp_file)
  
  # Return the path to be used by renderImage
  list(
    src = temp_file,
    contentType = "image/png",
    width = width * 96,  # Convert inches to pixels
    height = height * 96,
    alt = "CO2 Plot"
  )
}

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  titlePanel("CO2 Concentration Analysis"),
  
  # Status panel
  div(
    id = "connection_status",
    style = "padding: 10px; margin-bottom: 10px; border-radius: 5px;",
    textOutput("api_status")
  ),
  
  # Main layout
  sidebarLayout(
    sidebarPanel(
      dateInput("prediction_date", 
                "Select Future Date:",
                value = Sys.Date() + days(30),
                min = Sys.Date() + days(1)),
      actionButton("update_btn", "Update Analysis", 
                  class = "btn-primary",
                  style = "width: 100%"),
      hr(),
      # Single prediction panel
      div(
        id = "prediction_panel",
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
        conditionalPanel(
          condition = "output.has_prediction == false",
          div(
            style = "color: #6c757d; text-align: center; padding: 20px;",
            "Click 'Update Analysis' to see prediction results"
          )
        ),
        conditionalPanel(
          condition = "output.has_prediction == true",
          div(
            style = "line-height: 1.6;",
            h4(style = "margin: 0 0 15px 0; color: #495057;", "CO2 Prediction"),
            div(
              style = "color:rgb(28, 28, 28); font-size: 1em; margin-bottom: 10px;",
              textOutput("selected_date")
            ),
            div(
              style = "font-size: 24px; font-weight: bold; color: #0056b3; margin: 15px 0;",
              textOutput("predicted_value")
            ),
            div(
              style = "color: #6c757d; font-size: 0.9em;",
              "95% Confidence Interval:",
              div(
                style = "margin-top: 5px;", 
                textOutput("confidence_interval")
              )
            )
          )
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Prediction Analysis",
                 fluidRow(
                   column(12,
                          h4("CO2 Trend and Prediction"),
                          div(
                            id = "trend_plot_container",
                            conditionalPanel(
                              condition = "!output.has_trend_plot",
                              div(
                                style = "text-align: center; color: #6c757d; padding: 40px; background-color: #f8f9fa; border-radius: 5px; margin: 20px 0;",
                                "Click 'Update Analysis' to view predictions"
                              )
                            ),
                            conditionalPanel(
                              condition = "output.has_trend_plot",
                              imageOutput("trend_plot", height = "600px") %>% 
                                withSpinner(type = 8, color = "#0dc5c1")
                            )
                          ),
                          div(
                            style = "border-top: 2px solid #dee2e6; margin: 40px 0;",
                            class = "separator"
                          ),
                          div(
                            style = "margin-top: 40px;",
                            h4("Predicted CO2 Values"),
                            conditionalPanel(
                              condition = "!output.has_trend_plot",
                              div(
                                style = "text-align: center; color: #6c757d; padding: 20px; background-color: #f8f9fa; border-radius: 5px;",
                                "Waiting for date selection..."
                              )
                            ),
                            DTOutput("prediction_table") %>% 
                              withSpinner(type = 8, color = "#0dc5c1")
                          )
                   )
                 )),
        tabPanel("Historical Data",
                 fluidRow(
                   column(12,
                          h4("Historical CO2 Measurements"),
                          div(
                            id = "raw_plot_container",
                            conditionalPanel(
                              condition = "!output.has_raw_plot",
                              div(
                                style = "text-align: center; color: #6c757d; padding: 20px; background-color: #f8f9fa; border-radius: 5px;",
                                "Waiting for data..."
                              )
                            ),
                            imageOutput("raw_data_plot", height = "600px") %>% 
                              withSpinner(type = 8, color = "#0dc5c1")
                          ),
                          div(
                            style = "border-top: 2px solid #dee2e6; margin: 40px 0;",
                            class = "separator"
                          ),
                          div(
                            style = "margin-top: 40px;",
                            DTOutput("raw_data_table") %>% 
                              withSpinner(type = 8, color = "#0dc5c1")
                          )
                   )
                 ))
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    raw_data = NULL,
    trend_data = NULL,
    prediction = NULL,
    api_status = "Checking connection...",
    has_raw_plot = FALSE,
    has_trend_plot = FALSE,
    has_prediction = FALSE,
    raw_plot_data = NULL
  )
  
  # Output reactive values to control conditional panels
  output$has_raw_plot <- reactive({ rv$has_raw_plot })
  outputOptions(output, "has_raw_plot", suspendWhenHidden = FALSE)
  
  output$has_trend_plot <- reactive({ rv$has_trend_plot })
  outputOptions(output, "has_trend_plot", suspendWhenHidden = FALSE)
  
  output$has_prediction <- reactive({ rv$has_prediction })
  outputOptions(output, "has_prediction", suspendWhenHidden = FALSE)
  
  # Check API connection on startup and load initial data
  observe({
    tryCatch({
      make_api_request("/health")
      rv$api_status <- "✓ Connected to API"
      shinyjs::addClass(selector = "#connection_status", class = "bg-success")
      
      # Get initial data range
      current_data <- make_api_request("/co2/current")
      dates <- as.Date(paste(current_data$co2$year, 
                            current_data$co2$month, 
                            current_data$co2$day, sep = "-"))
      max_date <- max(dates)
      
      # Update date input constraints
      updateDateInput(session, "prediction_date",
                     min = Sys.Date() + days(1),
                     max = Sys.Date() + years(10),
                     value = min(Sys.Date() + days(30), max_date))
      
      # Load initial raw data
      raw_response <- make_api_request("/co2/raw")
      print("Raw data response received")
      print("Raw data structure:")
      print(str(raw_response))
      print("Raw data column names:")
      print(names(raw_response))
      rv$raw_data <- raw_response
      rv$has_raw_plot <- TRUE
      
      # Get initial raw plot
      raw_plot_response <- make_api_request("/co2/raw/plot")
      rv$raw_plot_data <- raw_plot_response$plot_data
      
    }, error = function(e) {
      rv$api_status <- paste("✗ API Connection Failed:", e$message)
      shinyjs::addClass(selector = "#connection_status", class = "bg-danger")
    })
  })
  
  # Update data when button is clicked
  observeEvent(input$update_btn, {
    # Disable button while processing
    shinyjs::disable("update_btn")
    rv$has_prediction <- FALSE
    rv$has_trend_plot <- FALSE
    
    tryCatch({
      # Get raw data
      raw_response <- make_api_request("/co2/raw")
      rv$raw_data <- raw_response
      rv$has_raw_plot <- TRUE
      
      # Get trend data and prediction for selected date
      trend_response <- make_api_request(
        "/co2/trend",
        query = list(prediction_date = format(input$prediction_date, "%Y-%m-%d"))
      )
      rv$trend_data <- trend_response
      
      # Get prediction details for selected date only
      pred_response <- make_api_request(
        "/co2/predict",
        method = "POST",
        body = list(prediction_date = format(input$prediction_date, "%Y-%m-%d"))
      )
      
      # Calculate days from latest data to prediction date
      latest_date <- as.Date(trend_response$metadata$latest_data_date)
      pred_date <- input$prediction_date
      days_ahead <- as.numeric(difftime(pred_date, latest_date, units = "days"))
      
      # Store prediction with confidence intervals for the target date
      rv$prediction <- list(
        predicted_value = pred_response$predicted_value,
        confidence_intervals = list(
          lower_95 = pred_response$confidence_intervals$lower_95[days_ahead],
          upper_95 = pred_response$confidence_intervals$upper_95[days_ahead]
        )
      )
      
      rv$has_prediction <- TRUE
      rv$has_trend_plot <- TRUE
      
    }, error = function(e) {
      showNotification(
        paste("Error updating data:", e$message),
        type = "error"
      )
    })
    
    # Re-enable button
    shinyjs::enable("update_btn")
  })
  
  # Render API status
  output$api_status <- renderText({
    rv$api_status
  })
  
  # Render raw data table
  output$raw_data_table <- renderDT({
    req(rv$raw_data)
    
    # Ensure we have the correct data structure
    if (!is.null(rv$raw_data$co2)) {
      historical_data <- data.frame(
        Year = rv$raw_data$co2$year,
        Month = rv$raw_data$co2$month,
        Day = rv$raw_data$co2$day,
        "CO2 (ppm)" = rv$raw_data$co2$co2,
        check.names = FALSE
      )
    } else {
      # If data structure is different, try direct approach
      historical_data <- as.data.frame(rv$raw_data)
      # Get the actual column names from the data
      print("Available columns:")
      print(names(historical_data))
      
      if ("co2" %in% names(historical_data)) {
        names(historical_data) <- c("Date", "CO2 (ppm)")
      } else {
        # Keep original column names but format them nicely
        names(historical_data) <- tools::toTitleCase(names(historical_data))
      }
    }
    
    datatable(
      historical_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        order = list(list(0, 'desc'))
      ),
      rownames = FALSE
    )
  })
  
  # Render raw data plot
  output$raw_data_plot <- renderImage({
    req(rv$raw_plot_data)
    
    # Render the plot
    render_base64_plot(rv$raw_plot_data)
  }, deleteFile = TRUE)  # Clean up temp file
  
  # Render trend plot
  output$trend_plot <- renderImage({
    req(rv$prediction)  # Require prediction data to be available
    req(rv$has_trend_plot)  # Require explicit flag that we've updated
    req(input$prediction_date)
    
    # Make API request
    response <- make_api_request(
      "/co2/trend/plot",
      query = list(prediction_date = format(input$prediction_date, "%Y-%m-%d"))
    )
    
    # Render the plot
    render_base64_plot(response$plot_data)
  }, deleteFile = TRUE)  # Clean up temp file
  
  # Render prediction details
  output$selected_date <- renderText({
    req(input$prediction_date)
    format(input$prediction_date, "%B %d, %Y")
  })
  
  output$predicted_value <- renderText({
    req(rv$prediction)
    # Only show prediction for the selected date
    sprintf("%.2f ppm", rv$prediction$predicted_value)
  })
  
  output$confidence_interval <- renderText({
    # Require both the prediction data and that the date matches
    req(rv$prediction)
    req(input$prediction_date)
    
    # Only show confidence interval if we have a prediction for the selected date
    if (!is.null(rv$prediction$predicted_value)) {
      sprintf("%.2f - %.2f ppm", 
              rv$prediction$confidence_intervals$lower_95,
              rv$prediction$confidence_intervals$upper_95)
    } else {
      return(NULL)  # Don't show anything if no prediction for this date
    }
  })
  
  # Render prediction table
  output$prediction_table <- renderDT({
    req(rv$prediction)
    
    # Calculate time spans
    today <- Sys.Date()
    selected_date <- input$prediction_date
    days_out <- as.numeric(difftime(selected_date, today, units = "days"))
    
    # Generate adaptive date sequence
    if (days_out <= 30) {
      # Daily for first month
      date_seq <- seq.Date(from = today, to = selected_date, by = "day")
    } else if (days_out <= 180) {
      # First month daily, then weekly
      daily_dates <- seq.Date(from = today, to = today + days(30), by = "day")
      weekly_dates <- seq.Date(from = today + days(31), to = selected_date, by = "week")
      date_seq <- c(daily_dates, weekly_dates)
    } else {
      # First month daily, next 5 months weekly, then monthly
      daily_dates <- seq.Date(from = today, to = today + days(30), by = "day")
      weekly_dates <- seq.Date(from = today + days(31), to = today + days(180), by = "week")
      monthly_dates <- seq.Date(from = today + days(181), to = selected_date, by = "month")
      date_seq <- c(daily_dates, weekly_dates, monthly_dates)
    }
    
    # Ensure selected date is included
    if (!selected_date %in% date_seq) {
      date_seq <- c(date_seq, selected_date)
      date_seq <- sort(unique(date_seq))
    }
    
    # Get predictions for dates
    predictions <- lapply(date_seq, function(date) {
      pred <- tryCatch({
        make_api_request(
          "/co2/predict",
          method = "POST",
          body = list(prediction_date = format(date, "%Y-%m-%d"))
        )
      }, error = function(e) NULL)
      
      if (!is.null(pred)) {
        data.frame(
          Date = format(date, "%m/%d/%Y"),
          "Sort_Date" = as.numeric(date),  # Hidden column for sorting
          "Predicted CO2" = sprintf("%.2f", pred$predicted_value),
          check.names = FALSE
        )
      }
    })
    
    # Combine all predictions
    predictions_df <- do.call(rbind, predictions)
    
    # Add units to column names
    names(predictions_df)[3] <- paste(names(predictions_df)[3], "(ppm)")
    
    # Create the table
    datatable(
      predictions_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = 1, visible = FALSE)  # Hide the Sort_Date column
        ),
        order = list(list(2, 'asc'))  # Sort by hidden date column
      ),
      rownames = FALSE
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server) 