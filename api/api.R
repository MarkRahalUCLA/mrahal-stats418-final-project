# Load required packages
library(plumber)
library(httr)
library(jsonlite)
library(forecast)
library(tidyverse)
library(lubridate)
library(memoise)
library(base64enc)  # Add base64enc package for image encoding

# Configure cache directory for Cloud Run
cache_dir <- Sys.getenv("CACHE_DIR", "/workspace/cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# Configure logging
log_info <- function(...) {
  cat(format(Sys.time()), "-", "INFO", "-", ..., "\n")
}

log_error <- function(...) {
  cat(format(Sys.time()), "-", "ERROR", "-", ..., "\n", file = stderr())
}

# Source the model functions
source("model.R")

#* @apiTitle CO2 Prediction API
#* @apiDescription API for CO2 concentration predictions and trend analysis

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  plumber::forward()
}

#* @get /health
#* @tag Health
#* @description Health check endpoint for Cloud Run
#* @serializer unboxedJSON
function() {
  log_info("Health check requested")
  list(
    status = "healthy",
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    version = "1.0.0"
  )
}

#* @get /co2/current
#* @tag CO2 Data
#* @description Get current CO2 concentration data
#* @serializer unboxedJSON
function() {
  log_info("Fetching current CO2 data")
  data <- get_co2_data()
  if (is.null(data)) {
    log_error("Failed to fetch CO2 data")
    stop("Failed to fetch CO2 data", status = 500)
  }
  return(data)
}

#* @get /co2/raw
#* @tag CO2 Data
#* @description Get processed raw CO2 data for plotting
#* @serializer unboxedJSON
function() {
  log_info("Processing raw CO2 data")
  data <- get_co2_data()
  if (is.null(data)) {
    log_error("Failed to fetch CO2 data")
    stop("Failed to fetch CO2 data", status = 500)
  }
  
  plot_data <- prepare_plot_data(data)
  if (is.null(plot_data)) {
    log_error("Failed to process plot data")
    stop("Failed to process plot data", status = 500)
  }
  
  # Ensure date is in correct format
  plot_data$date <- as.character(as.Date(plot_data$date))
  return(plot_data)
}

#* @get /co2/raw/plot
#* @tag Plots
#* @description Get raw CO2 data plot as base64 encoded PNG
#* @serializer unboxedJSON
function() {
  log_info("Generating raw CO2 plot")
  
  # Get data and generate plot
  data <- get_co2_data()
  if (is.null(data)) {
    log_error("Failed to fetch CO2 data")
    stop("Failed to fetch CO2 data", status = 500)
  }
  
  # Generate plot
  plot_results <- create_raw_plot(data)
  if (is.null(plot_results)) {
    log_error("Failed to generate plot")
    stop("Failed to generate plot", status = 500)
  }
  
  # Save plot to a temporary file
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot_results$plot, width = 10, height = 6, dpi = 300)
  
  # Read and encode the plot
  plot_data <- base64enc::base64encode(temp_file)
  
  # Clean up
  unlink(temp_file)
  
  # Return encoded plot
  list(
    plot_data = plot_data,
    content_type = "image/png"
  )
}

#* @post /co2/predict
#* @tag Predictions
#* @param prediction_date:string Date to predict CO2 concentration for (YYYY-MM-DD)
#* @description Generate CO2 concentration prediction for a specific date
#* @serializer unboxedJSON
function(prediction_date) {
  log_info("Generating prediction for date:", prediction_date)
  
  # Validate date
  tryCatch({
    pred_date <- as.Date(prediction_date)
    if (pred_date <= Sys.Date()) {
      log_error("Invalid prediction date (not in future):", prediction_date)
      stop("Prediction date must be in the future", status = 400)
    }
  }, error = function(e) {
    log_error("Invalid date format:", prediction_date)
    stop("Invalid date format. Use YYYY-MM-DD", status = 400)
  })
  
  # Get data and generate prediction
  data <- get_co2_data()
  if (is.null(data)) {
    log_error("Failed to fetch CO2 data for prediction")
    stop("Failed to fetch CO2 data", status = 500)
  }
  
  prediction <- generate_prediction(data, pred_date)
  if (is.null(prediction)) {
    log_error("Failed to generate prediction")
    stop("Failed to generate prediction", status = 500)
  }
  
  # Return prediction result
  list(
    prediction_date = as.character(pred_date),
    predicted_value = prediction$predicted_value,
    confidence_intervals = list(
      lower_80 = as.numeric(prediction$forecast_model$lower[, 1]),
      upper_80 = as.numeric(prediction$forecast_model$upper[, 1]),
      lower_95 = as.numeric(prediction$forecast_model$lower[, 2]),
      upper_95 = as.numeric(prediction$forecast_model$upper[, 2])
    )
  )
}

#* @get /co2/trend
#* @tag Analysis
#* @param prediction_date:string Date to include in trend analysis (YYYY-MM-DD)
#* @description Get historical trend and prediction data for plotting
#* @serializer unboxedJSON
function(prediction_date) {
  log_info("Generating trend analysis for date:", prediction_date)
  
  # Validate date
  tryCatch({
    pred_date <- as.Date(prediction_date)
    if (is.na(pred_date)) {
      log_error("Invalid date format:", prediction_date)
      stop("Invalid date format. Use YYYY-MM-DD")
    }
  }, error = function(e) {
    log_error("Invalid date format:", prediction_date)
    stop("Invalid date format. Use YYYY-MM-DD", status = 400)
  })
  
  # Get data and generate trend
  data <- get_co2_data()
  if (is.null(data)) {
    log_error("Failed to fetch CO2 data for trend analysis")
    stop("Failed to fetch CO2 data", status = 500)
  }
  
  # Generate trend data with error handling
  trend_results <- tryCatch({
    results <- create_trend_plot(data, pred_date)
    if (is.null(results)) {
      log_error("Failed to generate trend analysis")
      stop("Failed to generate trend analysis")
    }
    
    # Add metadata to response
    list(
      data = results$data,
      metadata = list(
        latest_data_date = format(results$latest_data_date, "%Y-%m-%d"),
        prediction_date = format(pred_date, "%Y-%m-%d")
      )
    )
  }, error = function(e) {
    # Extract error message and return appropriate status
    msg <- conditionMessage(e)
    if (grepl("Prediction date must be after", msg)) {
      log_error("Invalid prediction date:", msg)
      stop(msg, status = 400)
    } else {
      log_error("Error generating trend analysis:", msg)
      stop(paste("Error generating trend analysis:", msg), status = 500)
    }
  })
  
  # Return trend data
  trend_results
}

#* @get /co2/trend/plot
#* @tag Plots
#* @param prediction_date:string Date to include in trend analysis (YYYY-MM-DD)
#* @description Get trend plot as base64 encoded PNG
#* @serializer unboxedJSON
function(prediction_date) {
  log_info("Generating trend plot for date:", prediction_date)
  
  # Validate date
  tryCatch({
    pred_date <- as.Date(prediction_date)
    if (is.na(pred_date)) {
      log_error("Invalid date format:", prediction_date)
      stop("Invalid date format. Use YYYY-MM-DD")
    }
  }, error = function(e) {
    log_error("Invalid date format:", prediction_date)
    stop("Invalid date format. Use YYYY-MM-DD", status = 400)
  })
  
  # Get data and generate plot
  data <- get_co2_data()
  if (is.null(data)) {
    log_error("Failed to fetch CO2 data")
    stop("Failed to fetch CO2 data", status = 500)
  }
  
  # Generate trend plot
  plot_results <- create_trend_plot(data, pred_date)
  if (is.null(plot_results)) {
    log_error("Failed to generate trend plot")
    stop("Failed to generate trend plot", status = 500)
  }
  
  # Save plot to a temporary file
  temp_file <- tempfile(fileext = ".png")
  ggsave(temp_file, plot_results$plot, width = 10, height = 6, dpi = 300)
  
  # Read and encode the plot
  plot_data <- base64enc::base64encode(temp_file)
  
  # Clean up
  unlink(temp_file)
  
  # Return encoded plot
  list(
    plot_data = plot_data,
    content_type = "image/png"
  )
} 