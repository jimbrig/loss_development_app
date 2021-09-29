Sys.setenv(R_CONFIG_ACTIVE = "production")

config <- config::get(file = "shiny_app/config.yml")

rsconnect::deployApp(
  appDir = "shiny_app",
  account = "jimbrig",
  appName = config$app_name
)
