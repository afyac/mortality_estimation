# 1 - Visualizations of each predictor raw per time unit -----------------------

df <- rio::import("05_prepare_training_data/output/som_training_data.rds") |> 
  dplyr::group_by(survey_id) |> 
  dplyr::filter(dplyr::row_number()==1) |> dplyr::ungroup()

df |> 
  dplyr::select(-district, -region, -month, -pcode, -year, -time_unit, -ends_with("_cat"), -survey_id, 
                -n, -admin0, -n_u5, -n_died, -n_died_u5, -p_time, -p_time_u5, -qualityScore, -lhz_type, -hh_weights,
                -recall_days) |> 
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
  ggplot2::facet_wrap(~predictor_name, scales = "free_y", nrow = 10, ncol = 10) +
  ggplot2::labs(x = "", y = "Value", 
       title = "Visualisations of each predictor raw per time unit") +
  ggplot2::theme_minimal() + ggplot2::guides(fill = 'none')

# save plot
ggplot2::ggsave("05_prepare_training_data/visualisation/output/som_training_data_time.png", bg = 'white',
       width = 16, height = 12)

# 2 - Visualizations of each predictor raw per time unit per district ----------

data_long <- rio::import("05_prepare_training_data/output/som_training_data.rds") |> 
  dplyr::select(
    date, region, district, acled_event, acled_fatalities, 
    aidworkers_killed, aidworkers_wounded, dep_rate, pop_average, 
    prop_idp, cholera_cases, malaria_cases, measles_cases, 
    rainfall_rollmean_lag5, cdi_lag5, sam_admissions_rate_lag1, 
    tot_goat_cereal_smooth_lag4, tot_wage_cereal_smooth_lag4, 
    tot_wage_cereal_smooth_lag4, water_price_smooth_lag4) |> 
  dplyr::group_by(region, district, date) |> 
  dplyr::mutate(dplyr::across(
    where(is.numeric),
    .fns = mean
  )) |> 
  # Convert data to long format
  tidyr::pivot_longer(
    cols = -c(region, district, date),  
    names_to = "predictor_name",  
    values_to = "value" 
  )

# Get the unique predictor names
predictor_names <- unique(data_long$predictor_name)

# Iterate over each predictor
for (predictor in predictor_names) {
  print(paste("working on predictor", predictor))
  
  # Filter the data for the current predictor
  data_filtered <- data_long[data_long$predictor_name == predictor,]
  
  # Generate plot
  plot <- ggplot2::ggplot(data = data_filtered, 
                          ggplot2::aes(x = date, y = value, color = region)) +
    ggplot2::geom_point(alpha = 0.5, size = 1) +
    ggplot2::geom_line() +
    ggplot2::geom_area(ggplot2::aes(fill = region)) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_hue(l = 60, c = 45) + 
    ggplot2::guides(color = 'none', fill = 'none') +
    # scale_color_manual(values = ) +
    ggplot2::facet_wrap(. ~ region + district, scales = "free_y") +
    ggplot2::ggtitle(predictor) 
  
  # Save plot with the predictor name
  ggplot2::ggsave(paste("05_prepare_training_data/visualisation/output/", 
               "som_training_district_dist_", predictor, ".png", sep = ""), 
         dpi = "print", height = 30, width = 40, units= "cm")
}

# 3 - Check co-correlations between predictors ---------------------------------

pred_data <- rio::import("05_prepare_training_data/output/som_training_data.rds") |> 
  dplyr::select(
    `Insecurity event rate` = acled_event_rate, 
    `Insecurity fatalities rate` = acled_fatalities_rate, 
    `Aid workers killed rate` =  aidworkers_killed_rate, 
    `Aid workers wounded rate` = aidworkers_wounded_rate,
    `IDP departure rate` = dep_rate, 
    `Proportion of IDPs` = prop_idp, 
    `Cholera incidence rate` =  cholera_cases_rate, 
    `Malaria incidence rate` = malaria_cases_rate, 
    `measles incidence rate` = measles_cases_rate, 
    `Rainfall (lag 5)` = rainfall_rollmean_lag5, 
    `Combined Drought Index (lag 5)` = cdi_lag5, 
    `SAM admissions rate (lag 1)` = sam_admissions_rate_lag1, 
    `Terms of trade (goat, lag 4)` = tot_goat_cereal_smooth_lag4, 
    `Terms of trade (wage, lag 4)` = tot_wage_cereal_smooth_lag4, 
    `Water price (lag 4)` = water_price_smooth_lag4, 
    `Number of deaths` = n_died, 
    `Number of deaths under 5` = n_died_u5) 

# run correlation plot
pred_data |> 
  cor(use = "pairwise.complete.obs")  |> 
  ggcorrplot::ggcorrplot(show.diag = TRUE, 
                         type = "lower", lab = TRUE, 
                         lab_size = 3) +
  ggplot2::theme(legend.position = "none")

# save plot
ggplot2::ggsave("05_prepare_training_data/visualisation/output/som_training_corcorrelation.png", bg = 'white',
       width = 10, height = 10);  dev.off()

# 5 - Make a plot to identify missing data -------------------------------------

# Get unprocessed predictor and rename vars
pred_data_pp <- 
  rio::import("05_prepare_training_data/output/som_training_data.rds") |> 
  dplyr::filter(year > 2014) |> 
  dplyr::select(
    Year = year, month, district,
    `Insecurity event rate` = acled_event, 
    `Insecurity fatalities rate` = acled_fatalities, 
    `Aid workers killed rate` =  aidworkers_killed, 
    `Aid workers wounded rate` = aidworkers_wounded,
    `IDP departure rate` = dep_rate, 
    `Proportion of IDPs` = prop_idp, 
    `Cholera incidence rate` = cholera_cases, 
    `Malaria incidence rate` = malaria_cases, 
    `Measles incidence rate` = measles_cases, 
    `Rainfall` = rainfall_rollmean_lag5, 
    `Combined Drought Index` = cdi, 
    `SAM admissions rate` = sam_admissions, 
    `Terms of trade (goat)` = tot_goat_cereal_smooth, 
    `Terms of trade (wage)` = tot_wage_cereal_smooth, 
    `Water price` = water_price, 
    `Number of deaths` = n_died, 
    `Number of deaths under 5` = n_died_u5, 
    `Person Time` = p_time, 
    `Person Time under 5` = p_time_u5) |> dplyr::select(-month, -district)

# Get unprocessed predictor and run through the plot function
mast::f_plot_missing_data(pred_data_pp)

# save plot
ggplot2::ggsave("05_prepare_training_data/visualisation/output/som_training_completeness.pdf",
       width = 10, height = 8, dpi = 320)
