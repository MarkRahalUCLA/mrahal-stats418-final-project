# Using R/Rstudio to run the application

The application script actually references the API URL from the Google Cloud Run server. To change this:
1. Open app.R in some IDE
2. Locate the `api_base_url` declaration on line 13
3. Change the value passed in to `httop://localhost:8080`
4. Run the app (make sure the API is running first)
