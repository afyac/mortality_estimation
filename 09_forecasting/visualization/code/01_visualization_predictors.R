library(rio)
library(dplyr)
library(tidyr)
colours_f <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")

palette_cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")

# Initialise random numbers
set.seed(123)

## FIrst define the date to FORECAST 
DATE_FORECAST <- '2023-08-15'

## Source functions
source("09_forecasting/code/00_source_function.R")


###Import worst, best, middle scenarios predictors 
middle_cases <- import('09_forecasting/output/som_statict_forecasting_data.csv')

middle_cases <- aggregate(middle_cases[, c("pop_average", "pop_average_u5", "cdi_lag5_scn", 
                                         "measles_cases_rate","sam_admissions_rate_lag1_scn",
                                         "malaria_cases_rate")], 
                          by=list(time_unit = middle_cases$time_unit, 
                                  date = middle_cases$date, district = middle_cases$district), FUN=mean)

###Merge all data and add a type
ts_all <- list(middle_cases)
ts_all[[1]]$scen <- 'middle_cases'


ts_comb <- do.call(rbind, ts_all) %>% mutate(date = as.Date(date), district=district)

name_pred <- 'measles_cases_rate'
## Plot the different predictors per district time 
ts_comb %>%
  pivot_longer(measles_cases_rate) %>%
  dplyr::filter(date > as.Date(as.Date("2015-01-01"))) %>%
  dplyr::filter(name != "pop_average") %>%
  dplyr::filter(name != "pop_average_u5") %>%
  ggplot2::ggplot(aes(date, value, color = scen)) +
  geom_line() +
  geom_line(data = . %>% dplyr::filter(date > as.Date(DATE_FORECAST)), lwd = 1.2) +
  geom_line(color = "black", data = . %>% dplyr::filter(date < as.Date(DATE_FORECAST))) +
  facet_wrap(. ~ district, scales = "free_y") +
  theme_bw() +
  scale_color_viridis_d(labels = c("Optimum  ", "Central  ", "Worst  "), name = "Scenario", end = 0.8) +
  xlab("Date") +
  ylab("Value")

ggsave(paste('09_forecasting/visualization/output/som_pred_forecast_district_', name_pred, '.png', sep=""), 
       dpi = "print", width = 50, height = 50, units = "cm")

## Plot at a global level
###Import worst, best, middle scenarios predictors 
middle_cases <- import('09_forecasting/output/som_statict_forecasting_data.csv')

middle_cases <- aggregate(middle_cases[, c("pop_average", "cdi_lag5_scn", 
                                           "measles_cases_rate","sam_admissions_rate_lag1_scn",
                                           "malaria_cases_rate")], 
                          by=list(date = middle_cases$date), FUN=mean)


bnlearn_cases <- rio::import("06_prepare_predicting_data/output/som_predicting_data.rds") |>
  dplyr::filter(date >= "2015-01-01") |>
  dplyr::filter(date <= DATE_FORECAST)

ts_expand <- aggregate(bnlearn_cases[, c("pop_average", "cdi_lag5_scn", 
                                           "measles_cases_rate","sam_admissions_rate_lag1_scn",
                                           "malaria_cases_rate")], 
                          by=list(date = bnlearn_cases$date), FUN=mean)

###Merge all data and add a type
ts_all <- list(middle_cases, ts_expand)
ts_all[[1]]$scen <- 'middle_cases'
ts_all[[2]]$scen <- 'bnlearn_cases'

ts_comb <- do.call(rbind, ts_all) %>% mutate(date = as.Date(date))

name_pred <- 'measles_cases_rate'
## Plot the different predictors per global time 
ts_comb %>%
  pivot_longer(measles_cases_rate) %>%
  dplyr::filter(date > as.Date(as.Date("2015-01-01"))) %>%
  # dplyr::filter(name != "pop_average") %>%
  # dplyr::filter(name != "pop_average_u5") %>%
  ggplot2::ggplot(aes(date, value, color = scen)) +
  geom_line() +
  geom_line(data = . %>% dplyr::filter(date > as.Date(DATE_FORECAST)), lwd = 1.2) +
  geom_line(color = "black", data = . %>% dplyr::filter(date < as.Date(DATE_FORECAST))) +
  theme_bw() +
  #scale_color_viridis_d(labels = c("Optimum  ", "Central  ", "Worst  "), name = "Scenario", end = 0.8) +
  xlab("Date") +
  ylab("Value")

ggsave(paste('09_forecasting/visualization/output/som_pred_forecast_', name_pred, '.png', sep=""), 
       dpi = "print", width = 50, height = 50, units = "cm")

