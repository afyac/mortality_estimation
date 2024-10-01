# 1 - Visualizations of each predictor raw per time unit -----------------------

# get predictor datasets
markets_data <- rio::import("03_update_predictors/output/som_markets_data.rds")
sam_data <- rio::import("03_update_predictors/output/som_sam_data.rds")
climate_data <- rio::import("03_update_predictors/output/som_climate_data.rds")
attacks_aid_workers <- rio::import("03_update_predictors/output/som_aid_workers_data.rds")
acled_data <- rio::import("03_update_predictors/output/som_acled_data.rds")
malaria_data <- rio::import("03_update_predictors/output/som_malaria_fsnau.rds")
measles_data <- rio::import("03_update_predictors/output/som_measles_fsnau.rds") 
cholera_data <- rio::import("03_update_predictors/output/som_cholera_fsnau.rds")

# Generate time-unit -----------------------------------------------------------

# variables to join by
var_by <- c(
  "district", "year", "month", 'region', 'pcode', 'time_unit', 'reg_code', 'date'
)

# join predictors
predictor_data <-  acled_data |>
  dplyr::left_join(attacks_aid_workers, by = var_by) |>
  dplyr::left_join(cholera_data, by = var_by) |>
  dplyr::left_join(malaria_data, by = var_by) |>
  dplyr::left_join(measles_data, by = var_by) |>
  dplyr::left_join(climate_data, by = var_by) |>
  dplyr::left_join(sam_data, by = var_by) |>
  dplyr::left_join(markets_data, by = var_by)


predictor_data |> 
  dplyr::select(-district, -region, -month, -pcode, -year, -time_unit, -reg_code) |> 
  dplyr::group_by(date) |> 
  dplyr::mutate(dplyr::across(
    where(is.numeric),
    .fns = mean
  )) |> 
  # Convert data to long format
  tidyr::pivot_longer(
    cols = -date,  
    names_to = "predictor_name",  
    values_to = "value" 
  ) |> 
  dplyr::mutate(variable = dplyr::case_when(
    stringr::str_detect(predictor_name, "acled_event") ~ "acled_event",
    stringr::str_detect(predictor_name, "acled_fatalities") ~ "acled_fatalities",
    stringr::str_detect(predictor_name, "aidworkers_killed") ~ "aidworkers_killed",
    stringr::str_detect(predictor_name, "aidworkers_wounded") ~ "aidworkers_wounded",
    stringr::str_detect(predictor_name, "malaria_cases") ~ "malaria_cases",
    stringr::str_detect(predictor_name, "measles_cases") ~ "measles_cases",
    stringr::str_detect(predictor_name, "cholera_cases") ~ "cholera_cases",
    stringr::str_detect(predictor_name, "rainfall") ~ "rainfall",
    stringr::str_detect(predictor_name, "cdi") ~ "CDI",
    stringr::str_detect(predictor_name, "sam_admissions") ~ "sam_admissions",
    stringr::str_detect(predictor_name, "tot_goat_cereal") ~ "tot_goat_cereal",
    stringr::str_detect(predictor_name, "tot_wage_cereal") ~ "tot_wage_cereal",
    stringr::str_detect(predictor_name, "water_price") ~ "water_price",
    stringr::str_detect(predictor_name, "pop_average") ~ "pop_average",
    stringr::str_detect(predictor_name, "prop_idp") ~ "prop_idp",
    stringr::str_detect(predictor_name, "dep_rate") ~ "dep_rate"),
    variable = factor(variable)) |> 
  ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +
  ggplot2::geom_point(alpha = 0.5, size = 0.4) +
  ggplot2::geom_line(color = "steelblue") +
  ggplot2::geom_area(ggplot2::aes(fill = variable)) +
  ggplot2::facet_wrap(~predictor_name, scales = "free_y", nrow = 3, ncol = 7) +
  ggplot2::labs(x = "", y = "Value", 
       title = "Visualisations of each predictor raw per time unit") +
  ggplot2::theme_minimal() + ggplot2::theme_bw() + ggplot2::guides(fill = 'none')

# save plot
ggplot2::ggsave("03_update_predictors/visualisation/output/som_predictors_time.png", bg = 'white',
       width = 16, height = 12)
