
#  ------------------------------------------------------------------------
#
# Title : Global Script
#    By : Jimmy Briggs
#  Date : 2020-11-06
#
#  ------------------------------------------------------------------------

# Library Packages --------------------------------------------------------
library(devtri) # remotes::install_github(ractuary/devtri)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(purrr)
library(lubridate)
library(tibble)
library(dplyr)
library(tidyr)
library(DT)
library(config)
library(qs)
library(polished)

# library(tychobratools)
#

# polished::set_config_env()

app_config <- config::get()

polished::global_sessions_config(
  app_name = app_config$app_name,
  api_key = app_config$api_key#,
  # admin_mode = TRUE
)

# Source Functions --------------------------------------------------------
purrr::walk(fs::dir_ls("R"), source)

# Global Options ----------------------------------------------------------
options(shiny.autoload.r = TRUE)
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

# Data --------------------------------------------------------------------
# trans <- qs::qread("./data/trans.qs")
loss_data_all <- qs::qread("./data/loss_data_all.qs")

maturity_choices <- c(1:12) %>% set_names(month.name)
