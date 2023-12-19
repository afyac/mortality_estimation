# get libraries
pacman::p_load(
  tidyverse,   # Data manipulation, transformation, visualisation  etc.,
  rio,         # For importing data without issues
  lubridate,   # Makes it easier to work with dates and times
  broom,       # For wrangling tables of models
  reshape,     # For reshaping data
  zoo,         # For stats functions
  scales)      # For sclaing things

source("03_update_predictors/code/00_source_functions.R")
# 1 - Visualizations of each predictor raw per time unit -----------------------

# get predictor datasets
markets_data <- import("03_update_predictors/output/som_markets_tot_updated.rds")
sam_data <- import("03_update_predictors/output/som_sam_admissions.rds")
climate_data <- import("03_update_predictors/output/som_climate_data.rds")
attacks_aid_workers <- import("03_update_predictors/output/som_attacks_aid_workers.rds")
acled_data <- import("03_update_predictors/output/som_conflict_acled.rds")
malaria_data <- import("03_update_predictors/output/som_malaria_data.rds")
measles_data <- import("03_update_predictors/output/som_measles_data.rds") 
cholera_data <- import("03_update_predictors/output/som_malaria_measles_cholera.rds") |> 
  dplyr::select(-measles_cases, -malaria_cases)

# Generate time-unit -----------------------------------------------------------

# make time unit (starts at 2014 to get 2013)
time_unit <- f_generate_time_units(
  y_analysis_start = 2010, y_analysis_end = 2024,
  m_analysis_start = 1, m_analysis_end = 12
)

# variables to join by
var_by <- c(
  "district", "year", "month", 'region', 'pcode'
)

# join predictors
predictor_data <-  acled_data |>
  left_join(time_unit, by = c('year', 'month')) |>
  left_join(attacks_aid_workers, by = var_by) |>
  left_join(cholera_data, by = var_by) |>
  left_join(malaria_data, by = var_by) |>
  left_join(measles_data, by = var_by) |>
  left_join(climate_data, by = var_by) |>
  left_join(sam_data, by = var_by) |>
  left_join(markets_data, by = var_by) |> 
  dplyr::mutate(date = as.Date(paste0(year, "-", month, "-01")))


predictor_data |> 
  dplyr::select(-district, -region, -month, -pcode, -year, -time_unit, -reg_code) |> 
  group_by(date) |> 
  mutate(across(
    where(is.numeric),
    .fns = mean
  )) |> 
  # Convert data to long format
  pivot_longer(
    cols = -date,  
    names_to = "predictor_name",  
    values_to = "value" 
  ) |> 
  mutate(variable = case_when(
    str_detect(predictor_name, "acled_event") ~ "acled_event",
    str_detect(predictor_name, "acled_fatalities") ~ "acled_fatalities",
    str_detect(predictor_name, "aidworkers_killed") ~ "aidworkers_killed",
    str_detect(predictor_name, "aidworkers_wounded") ~ "aidworkers_wounded",
    str_detect(predictor_name, "malaria_cases") ~ "malaria_cases",
    str_detect(predictor_name, "measles_cases") ~ "measles_cases",
    str_detect(predictor_name, "cholera_cases") ~ "cholera_cases",
    str_detect(predictor_name, "rainfall") ~ "rainfall",
    str_detect(predictor_name, "cdi") ~ "CDI",
    str_detect(predictor_name, "sam_admissions") ~ "sam_admissions",
    str_detect(predictor_name, "tot_goat_cereal") ~ "tot_goat_cereal",
    str_detect(predictor_name, "tot_wage_cereal") ~ "tot_wage_cereal",
    str_detect(predictor_name, "water_price") ~ "water_price",
    str_detect(predictor_name, "pop_average") ~ "pop_average",
    str_detect(predictor_name, "prop_idp") ~ "prop_idp",
    str_detect(predictor_name, "dep_rate") ~ "dep_rate"),
    variable = factor(variable)) |> 
  ggplot(aes(x = date, y = value)) +
  geom_point(alpha = 0.5, size = 0.4) +
  geom_line(color = "steelblue") +
  geom_area(aes(fill = variable)) +
  facet_wrap(~predictor_name, scales = "free_y", nrow = 3, ncol = 7) +
  labs(x = "", y = "Value", 
       title = "Visualisations of each predictor raw per time unit") +
  theme_minimal() + guides(fill = 'none')

# save plot
ggsave("03_update_predictors/visualisation/output/som_predictors_time.png", bg = 'white',
       width = 16, height = 12);  dev.off()
