library(tidyverse)
library(cmdstanr)
library(rethinking)

# Copy trait percentiles from Prediction_Intervals_MS

basefile <- "Female_ans_menton.Rds"
f <- file.path("..", "Craniofacial_Growth", "Growth_Models",
               "Generated_Data",
               basefile)

fm <- read_rds(f)$DL_growth_set
coef(fm)
