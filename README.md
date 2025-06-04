# CO2 Concentration Prediction App

This Shiny application provides an interactive interface for analyzing and predicting atmospheric CO2 concentrations using historical data and time series modeling.

## Table of Contents
- [Exploratory Data Analysis](#exploratory-data-analysis)
- [Methodology](#methodology)
- [Results and Features](#results-and-features)

You can access the application here: [https://mrahalucla.shinyapps.io/co2-prediction-app/](https://mrahalucla.shinyapps.io/co2-prediction-app/)

## Exploratory Data Analysis

The application performs a predictive analysis of CO2 concentration data.

### Data Sources
- Historical CO2 concentration measurements, from January 2015 to today, from [Rene R's API](https://rapidapi.com/rene-mdd/api/daily-atmosphere-carbon-dioxide-concentration) hosted on RapidAPI.com. This API is updated daily.

### Key Insights
- Clear upward trend in CO2 concentrations over time
- Annual seasonal patterns with peaks typically in May
- Accelerating rate of increase in recent decades

### Data Characteristics
- Unit of measurement: Parts per million (ppm)
- `cycle`: The concentration value read as-is for that day
- `trend`: The concentration value after accounting for seasonal variation. This is the value used in the predictive model.

## Methodology

### API Architecture

The application interfaces with a dedicated prediction API hosted at [`co2-prediction-api-577479811788.us-west1.run.app`](https://co2-prediction-api-577479811788.us-west1.run.app). The API provides the following endpoints:

1. `/health`
   - Method: GET
   - Purpose: Check API connectivity
   - Response: Service health status

2. `/co2/current`
   - Method: GET
   - Purpose: Retrieve latest CO2 measurements
   - Response: Current CO2 levels with timestamp

3. `/co2/raw`
   - Method: GET
   - Purpose: Access historical raw data
   - Response: Complete historical CO2 measurements

4. `/co2/raw/plot`
   - Method: GET
   - Purpose: Generate visualization of historical raw data
   - Response: Base64 encoded plot image

5. `/co2/trend`
   - Method: GET
   - Parameters: prediction_date (YYYY-MM-DD)
   - Purpose: Get trend analysis and predictions
   - Response: Trend components and forecasts

6. `/co2/trend/plot`
   - Method: GET
   - Parameters: prediction_date (YYYY-MM-DD)
   - Purpose: Visualize trends and predictions
   - Response: Base64 encoded trend plot

7. `/co2/predict`
   - Method: POST
   - Parameters: prediction_date (YYYY-MM-DD)
   - Purpose: Generate specific date predictions
   - Response: Predicted value with confidence intervals

### Prediction Model

The underlying model is a relatively simple approach. It includes:

1. **ARIMA Modeling**
   - Utilizing `auto.arima()` function to select the best time series model
   - Using the `forecast` R package to perform the predictive analysis on the ARIMA model 

2. **Confidence Intervals**
   - 95% prediction confidence intervals
   - 80% prediction confidence intervals also available, but not as relevant

3. **Plotting**
   - The model generates the plot, which the app will pull and post as an image file

## Results and Features

### Interactive Analysis
- Day-by-day CO2 prediction for future dates far into the future
- Visualization of trends and patterns
- Confidence interval estimation

### Visualization Components
- Historical trend plots
- Prediction confidence bands
- Seasonal pattern visualization
- Raw data exploration via tables

### Data Tables
- Historical measurements
- Predicted values
- Confidence intervals available but not shown in app

### Performance Metrics
- High accuracy for short-term predictions
- Decently fast API responses