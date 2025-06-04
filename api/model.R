# Install and load required packages
# if (!require("shiny")) install.packages("shiny")
# if (!require("httr")) install.packages("httr")
# if (!require("jsonlite")) install.packages("jsonlite")
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("forecast")) install.packages("forecast")
# if (!require("tidyverse")) install.packages("tidyverse")
# if (!require("lubridate")) install.packages("lubridate")
# if (!require("memoise")) install.packages("memoise")

# library(shiny)
# library(httr)
# library(jsonlite)
# library(ggplot2)
# library(forecast)
# library(tidyverse)
# library(lubridate)
# library(memoise)

# Cache for storing computed values
cache <- new.env()

# Fetch API CO2 data with caching (24 hour expiry)
get_co2_data <- memoise::memoise(
  function() {
    url <- "https://daily-atmosphere-carbon-dioxide-concentration.p.rapidapi.com/api/co2-api"
    
    print("Making request to RapidAPI...")
    
    # Set proper timeout configuration
    timeout_config <- httr::config(timeout = 10)
    
    # Make request with proper error handling
    response <- tryCatch({
      VERB("GET", url,
           add_headers(
             "X-RapidAPI-Key" = "c2bac24923mshc8dbd41bdf11eeap1514bejsnf164d705bdf4",
             "X-RapidAPI-Host" = "daily-atmosphere-carbon-dioxide-concentration.p.rapidapi.com"
           ),
           config = timeout_config)
    }, error = function(e) {
      print(paste("Request error:", e$message))
      return(NULL)
    })
    
    if (is.null(response)) {
      return(NULL)
    }
    
    print(paste("Response status:", httr::status_code(response)))
    
    if (httr::status_code(response) == 200) {
      # Try to parse the response content
      tryCatch({
        raw_content <- rawToChar(response$content)
        print("Raw response content (first 500 chars):")
        print(substr(raw_content, 1, 500))
        
        data <- jsonlite::fromJSON(raw_content)
        
        # Validate data structure
        if (!is.null(data$co2) && 
            all(c("year", "month", "day", "cycle", "trend") %in% names(data$co2))) {
          
          # Convert year, month, day to numeric and create proper dates
          year <- as.numeric(data$co2$year)
          month <- as.numeric(data$co2$month)
          day <- as.numeric(data$co2$day)
          
          # Validate numeric conversion
          if (any(is.na(year)) || any(is.na(month)) || any(is.na(day))) {
            print("Error: Invalid date values")
            return(NULL)
          }
          
          dates <- lubridate::make_date(year = year, month = month, day = day)
          
          # Print data range
          print(paste("Data range:", format(min(dates), "%Y-%m-%d"), "to", format(max(dates), "%Y-%m-%d")))
          
          # Convert trend and cycle to numeric
          trend <- as.numeric(data$co2$trend)
          cycle <- as.numeric(data$co2$cycle)
          
          # Validate numeric conversion
          if (any(is.na(trend)) || any(is.na(cycle))) {
            print("Error: Invalid numeric values in trend or cycle data")
            return(NULL)
          }
          
          # Store pre-computed values in cache
          cache$dates <- dates
          cache$trend <- trend
          cache$trend_ts <- ts(trend, frequency = 365.25)
          
          # Pre-compute ARIMA model with error handling
          tryCatch({
            cache$arima_model <- forecast::auto.arima(cache$trend_ts)
          }, error = function(e) {
            print(paste("Warning: Failed to pre-compute ARIMA model:", e$message))
            # Don't return NULL here, we can still use the data
          })
          
          # Return data with properly typed values
          data$co2$year <- year
          data$co2$month <- month
          data$co2$day <- day
          data$co2$trend <- trend
          data$co2$cycle <- cycle
          
          return(data)
        } else {
          print("Invalid data structure received:")
          print(str(data))
          return(NULL)
        }
      }, error = function(e) {
        print(paste("Error parsing response:", e$message))
        return(NULL)
      })
    } else {
      print(paste("Error response content:", rawToChar(response$content)))
      return(NULL)
    }
  },
  ~ memoise::timeout(24 * 60 * 60) # 24 hour cache
)

# Function to create the raw data table
prepare_plot_data <- function(data, end_date = NULL) {
  if (is.null(data)) return(NULL)
  
  # Use pre-computed dates if available
  dates <- if (!is.null(cache$dates)) cache$dates else 
    as.Date(paste0(data$co2$year,"-", data$co2$month,"-", data$co2$day))
  
  df <- data.frame(
    date = dates,
    cycle = as.numeric(data$co2$cycle),
    trend = as.numeric(data$co2$trend)
  )
  
  # Filter data up to end_date if specified
  if (!is.null(end_date)) {
    df <- df %>% dplyr::filter(date <= end_date)
  }
  
  df_long <- df %>%
    tidyr::pivot_longer(cols = c(cycle, trend),
                names_to = "type",
                values_to = "value")
  
  return(df_long)
}

# Function to create raw data plot
create_raw_plot <- function(data, end_date = NULL) {
  if (is.null(data)) return(NULL)
  
  raw_data <- prepare_plot_data(data, end_date)
  
  # If end_date is provided, ensure we only plot within the range
  if (!is.null(end_date)) {
    raw_data <- raw_data %>%
      dplyr::filter(date <= end_date)
  }
  
  # Calculate plot limits
  x_min <- min(raw_data$date)
  x_max <- if (!is.null(end_date)) end_date else max(raw_data$date)
  
  p <- ggplot2::ggplot(raw_data, ggplot2::aes(x = date, y = value, color = type)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Raw CO2 Concentration Data",
                  x = "Date",
                  y = "CO2 Concentration (ppm)",
                  color = "Measurement Type") +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%y",
      limits = c(x_min, x_max)
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10)  # Add more breaks
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_line(color = "gray90"),  # Lighter minor gridlines
      panel.grid.major = ggplot2::element_line(color = "gray80")   # Slightly darker major gridlines
    )
  
  return(list(
    plot = p,
    data = raw_data
  ))
}

# Function to generate CO2 trend prediction
generate_prediction <- function(data, prediction_date) {
  if (is.null(data)) return(NULL)
  
  # Get the latest date in our data
  dates <- if (!is.null(cache$dates)) cache$dates else 
    as.Date(paste0(data$co2$year,"-", data$co2$month,"-", data$co2$day))
  latest_date <- max(dates)
  
  # Calculate prediction span from the latest data point
  days_ahead <- as.numeric(difftime(prediction_date, latest_date, units = "days"))
  
  if (days_ahead <= 0) {
    stop(paste("Prediction date must be after the latest available data point:", format(latest_date, "%Y-%m-%d")))
  }
  
  # Use cached ARIMA model if available
  if (!is.null(cache$arima_model)) {
    tryCatch({
      forecast_model <- forecast::forecast(cache$arima_model, h = days_ahead)
    }, error = function(e) {
      stop(paste("Error generating forecast:", e$message))
    })
  } else {
    # Fallback to computing new model
    trend_data <- if (!is.null(cache$trend_ts)) cache$trend_ts else 
      ts(as.numeric(data$co2$trend), frequency = 365.25)
    
    tryCatch({
      arima_model <- forecast::auto.arima(trend_data)
      forecast_model <- forecast::forecast(arima_model, h = days_ahead)
    }, error = function(e) {
      stop(paste("Error fitting ARIMA model:", e$message))
    })
  }
  
  # Get the exact prediction for the target date
  predicted_value <- forecast_model$mean[days_ahead]
  
  # Trim the forecast to exactly match the days needed
  forecast_model$mean <- forecast_model$mean[1:days_ahead]
  forecast_model$lower <- forecast_model$lower[1:days_ahead, ]
  forecast_model$upper <- forecast_model$upper[1:days_ahead, ]
  
  return(list(
    forecast_model = forecast_model,
    predicted_value = predicted_value,
    latest_data_date = latest_date
  ))
}

# Function to create combined historical and prediction plot
create_trend_plot <- function(data, prediction_date) {
  if (is.null(data)) return(NULL)
  
  # Get prediction data
  pred_results <- tryCatch({
    generate_prediction(data, prediction_date)
  }, error = function(e) {
    stop(paste("Error in prediction generation:", e$message))
  })
  
  if (is.null(pred_results)) {
    stop("Failed to generate prediction results")
  }
  
  # Calculate start date (10 years before prediction date)
  start_date <- prediction_date - years(10)
  
  # Use cached dates and trend if available
  dates <- if (!is.null(cache$dates)) cache$dates else 
    as.Date(paste0(data$co2$year,"-", data$co2$month,"-", data$co2$day))
  trend <- if (!is.null(cache$trend)) cache$trend else 
    as.numeric(data$co2$trend)
  
  # Prepare historical data
  historical_df <- data.frame(
    date = dates,
    value = trend,
    type = "Historical"
  ) %>%
    # Filter to only include data between start_date and latest_data_date
    dplyr::filter(date >= start_date, date <= pred_results$latest_data_date)
  
  # Create sequence of dates from day after last historical date to prediction date
  date_seq <- seq.Date(from = pred_results$latest_data_date + 1,
                       to = prediction_date,
                       by = "day")
  
  # Get corresponding predictions
  pred_indices <- 1:length(date_seq)
  predictions <- pred_results$forecast_model$mean[pred_indices]
  lower_95 <- pred_results$forecast_model$lower[pred_indices, 2]
  upper_95 <- pred_results$forecast_model$upper[pred_indices, 2]
  
  prediction_df <- data.frame(
    date = date_seq,
    value = predictions,
    type = "Predicted",
    lower_95 = lower_95,
    upper_95 = upper_95
  )
  
  # Combine historical and prediction data
  combined_df <- rbind(
    historical_df,
    prediction_df[, c("date", "value", "type")]
  )
  
  # Create the plot
  p <- ggplot2::ggplot() +
    # Historical data line
    ggplot2::geom_line(data = historical_df,
                       ggplot2::aes(x = date, y = value, color = type),
                       size = 1) +
    # Prediction line
    ggplot2::geom_line(data = prediction_df,
                       ggplot2::aes(x = date, y = value, color = type),
                       size = 1) +
    # Confidence interval ribbon
    ggplot2::geom_ribbon(data = prediction_df,
                         ggplot2::aes(x = date,
                                     ymin = lower_95,
                                     ymax = upper_95),
                         fill = "lightblue",
                         alpha = 0.3) +
    # Theme and labels
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "CO2 Concentration Trend and Prediction",
      subtitle = paste("Historical data from", format(start_date, "%Y-%m-%d"), 
                      "to", format(pred_results$latest_data_date, "%Y-%m-%d"),
                      "\nPrediction until", format(prediction_date, "%Y-%m-%d")),
      x = "Year",
      y = "CO2 Concentration (ppm)",
      color = "Data Type"
    ) +
    ggplot2::scale_color_manual(
      values = c("Historical" = "darkblue", "Predicted" = "red")
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%y",
      expand = c(0.02, 0.02)
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10)
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_line(color = "gray90"),
      panel.grid.major = ggplot2::element_line(color = "gray80"),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )
  
  return(list(
    plot = p,
    data = combined_df,
    latest_data_date = pred_results$latest_data_date,
    prediction = list(
      data = prediction_df,
      value = pred_results$predicted_value
    )
  ))
}
