# Loss Development Shiny App

- Live example: https://jimbrig.shinyapps.io/lossdevt
- Code: https://github.com/jimbrig/loss_development_app

## Roadmap

A simple shiny web app with the following features:

1. Loss development triangles:
  - Types:
    - Paid
    - Reported
    - Case Reserves
    - Reported Claim Counts
  - Age-to-Age Factors Triangle
  - Averages
2. Actual vs. Expected
3. Ultimate Selections

## Installation

- Clone repo and run `shiny::runApp("shiny_app")` or,
- Pull docker image and run locally on port 8080:

```bash
docker run -p 8080:8080 -it ghcr.io/jimbrig/lossdevt:latest
```

### Related Projects

- Online Rater 
  - Live example: https://tychobra.shinyapps.io/online_rater
  - Code: https://github.com/Tychobra/online_rater


***

### File and Folder Structure

Use deploy.R script to deploy the app

Only files and folders that need to be deployed with the Shiny app go in the "shiny_app/" folder.

##### Project root level folders:

  - "data_prep/" - for data preparation.  Most projects need data to be prepped from some raw format to a format ready for the app.  This folder is most heavily used in the early days of the project as we are exploring the data and cleaning up initial data issues.
      - "provided/" - raw data provided by the client goes here.  This data is not tracked on GitHub
      - "prepped/" - cleaned versions of the data in "provided" goes here.  This data is also not tracked on GitHub.
  - "docs/" - additional documentation goes here
  - "analysis/" - additional analysis e.g. EDA goes here.  Also if you are training models that may or may not end up in your Shiny app, do that here.
  - You can make other folders as is appropriate for your project

##### "shiny_app/" level folders:

  - "www/" - contains all front end assets (i.e. css, js, and images)
  - "data/" - contains any flat files that need to be deployed with the app
  - "R/" - contains all R functions and modules.  Files containing modules should end with '_module.R'
