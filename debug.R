library(shiny)
library(tibble)
library(dplyr)
library(tidyr)
library(lubridate)
library(png)
library(markdown)
library(stringr)
library(ggpubr)
library(ggrepel)
library(DT)

source("includes.R")

# Single trait, multiple time points

sex_time <- "Male"
trait_time <- "Sella-Basion"

age_yrs_time_1 <- 9
age_yrs_time_2 <- 12
age_yrs_time_3 <- 16

age_months_time_1 <- 0
age_months_time_2 <- 0
age_months_time_3 <- 0

measure_time_1 <- 33.4
measure_time_2 <- 35.8
measure_time_3 <- 36.0

age_time_1 <- yrmo_to_years(age_yrs_time_1, age_months_time_1)
age_time_2 <- yrmo_to_years(age_yrs_time_2, age_months_time_2)
age_time_3 <- yrmo_to_years(age_yrs_time_3, age_months_time_3)


# data
fname <- file.path(
  "Trait_Percentiles",
  paste0(if_else(sex_time == "Female", "Female_", "Male_"),
         trait_to_abbrev(trait_time),
         "_percentiles.Rds"))
qs_time <- readRDS(fname)


# multi_time_plot
q_multi_time_to_plot <- qs_time |> 
  slice(seq(1, nrow(qs_time), by = 10)) |> 
  dplyr::select(age, all_of(percentiles_to_plot))

# Find percentiles
measure_1 <- as.numeric(measure_time_1)
age_1_rnd_time <- round(age_time_1, 2)
q_obs_1_time <- qs_time |>
  filter(age == age_1_rnd_time) |>
  select(-age) |>
  t()
pct_1_time <- which.min(abs(q_obs_1_time - measure_1))

measure_2 <- as.numeric(input$measure_time_2)
age_2_rnd_time <- round(age_time_2(), 2)
q_obs_2_time <- qs_time() |>
  filter(age == age_2_rnd_time) |>
  select(-age) |>
  t()
pct_2_time <- which.min(abs(q_obs_2_time - measure_2))

measure_3 <- as.numeric(input$measure_time_3)
age_3_rnd_time <- round(age_time_3(), 2)
q_obs_3_time <- qs_time() |>
  filter(age == age_3_rnd_time) |>
  select(-age) |>
  t()
pct_3_time <- which.min(abs(q_obs_3_time - measure_3))

# Format data for plotting
pcts_time <- tibble(
  Age = c(yrmo_to_years(input$age_yrs_time_1, input$age_months_time_1),
          yrmo_to_years(input$age_yrs_time_2, input$age_months_time_2),
          yrmo_to_years(input$age_yrs_time_3, input$age_months_time_3)),
  Percentile = c(measure_1, measure_2, measure_3)
)

plot_percentile(q = q_multi_time_to_plot,
                trait_string = input$trait_time) +
  geom_line(data = pcts_time,
            aes(x = Age, y = Percentile),
            color = red_color, linewidth = 1) +
  geom_point(data = pcts_time,
             aes(x = Age, y = Percentile),
             color = red_color, size = 2.5)


# multi_time_table
# Find percentiles
measure_1 <- as.numeric(input$measure_time_1)
age_1_rnd_time <- round(age_time_1(), 2)
q_obs_1_time <- qs_time() |>
  filter(age == age_1_rnd_time) |>
  select(-age) |>
  t()
pct_1_time <- which.min(abs(q_obs_1_time - measure_1))

measure_2 <- as.numeric(input$measure_time_2)
age_2_rnd_time <- round(age_time_2(), 2)
q_obs_2_time <- qs_time() |>
  filter(age == age_2_rnd_time) |>
  select(-age) |>
  t()
pct_2_time <- which.min(abs(q_obs_2_time - measure_2))

measure_3 <- as.numeric(input$measure_time_3)
age_3_rnd_time <- round(age_time_3(), 2)
q_obs_3_time <- qs_time() |>
  filter(age == age_3_rnd_time) |>
  select(-age) |>
  t()
pct_3_time <- which.min(abs(q_obs_3_time - measure_3))


out_time <- tibble(
  Sex = rep(input$sex_time, times = 3),
  Measurement = rep(input$trait_time, times = 3),
  `Age (yr)` =
    c(yrmo_to_years(input$age_yrs_time_1, input$age_months_time_1),
      yrmo_to_years(input$age_yrs_time_2, input$age_months_time_2),
      yrmo_to_years(input$age_yrs_time_3, input$age_months_time_3)) |>
    round(2),
  `Measurement (mm)` = c(input$measure_time_1,
                         input$measure_time_2,
                         input$measure_time_3),
  Percentile = c(pct_1_time, pct_2_time, pct_3_time))

datatable(out_time,
          filter = "none",
          rownames = FALSE,
          class = 'cell-border stripe',
          options = list(paging = FALSE,
                         searching = FALSE,
                         info = FALSE)) |>
  formatStyle('Percentile',  color = 'firebrick4',
              fontWeight = 'bold')
