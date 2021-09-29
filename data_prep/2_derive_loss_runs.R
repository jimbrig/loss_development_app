
#  ------------------------------------------------------------------------
#
# Title : Derive Individual Claim Level Lossruns by Eval
#    By :
#  Date : 2020-11-10
#
#  ------------------------------------------------------------------------

library(lubridate)
library(dplyr)
library(randomNames)
library(devtri)
library(tidyr)

trans <- qs::qread("data_prep/cache/trans.qs")

source("shiny_app/R/loss_run.R")

loss_data <- loss_run_all_evals(trans, merge = TRUE)

qs::qsave(loss_data, "data_prep/cache/loss_data.qs")
file.copy("data_prep/cache/loss_data.qs", "shiny_app/data/loss_data_all.qs", overwrite = TRUE)
