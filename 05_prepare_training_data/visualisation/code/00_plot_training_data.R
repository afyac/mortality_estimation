# get libraries
pacman::p_load(
  tidyverse,   # Data manipulation, transformation, visualisation  etc.,
  rio,         # For importing data without issues
  lubridate,   # Makes it easier to work with dates and times
  broom,       # For wrangling tables of models
  reshape,     # For reshaping data
  zoo,         # For stats functions
  scales)      # For sclaing things

source("04_process_predictors/code/00_functions_predictors.R")
# 1 - Visualizations of each predictor raw per time unit -----------------------

df <- import("05_prepare_training_data/output/som_training_data.rds") |> 
  group_by(survey_id) |> 
  filter(row_number()==1) |> ungroup()

df |> 
  dplyr::select(-district, -region, -month, -pcode, -year, -time_unit, -ends_with("_cat"), -survey_id, 
                -n, -admin0, -n_u5, -n_died, -n_died_u5, -p_time, -p_time_u5, -qualityScore, -lhz_type, -hh_weights,
                -Recall_Days) |> 
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
  facet_wrap(~predictor_name, scales = "free_y", nrow = 10, ncol = 10) +
  labs(x = "", y = "Value", 
       title = "Visualisations of each predictor raw per time unit") +
  theme_minimal() + guides(fill = 'none')

# save plot
ggsave("05_prepare_training_data/visualisation/output/som_training_data_time.png", bg = 'white',
       width = 16, height = 12);  dev.off()

# 2 - Visualizations of each predictor raw per time unit per district ----------

data_long <- import("05_prepare_training_data/output/som_training_data.rds") |> 
  dplyr::select(
    date, region, district, acled_event, acled_fatalities, 
    aidworkers_killed, aidworkers_wounded, dep_rate, pop_average, 
    prop_idp, cholera_cases, malaria_cases, measles_cases, 
    rainfall_lag5, cdi_lag5, sam_admissions_rate_lag1, 
    tot_goat_cereal_smooth_lag4, tot_wage_cereal_smooth_lag4, 
    tot_wage_cereal_smooth_lag4, water_price_smooth_lag4) |> 
  group_by(region, district, date) |> 
  mutate(across(
    where(is.numeric),
    .fns = mean
  )) |> 
  # Convert data to long format
  pivot_longer(
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
  plot <- ggplot(data = data_filtered, 
                 aes(x = date, y = value, color = region)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_line() +
    geom_area(aes(fill = region)) +
    theme_bw() +
    scale_color_hue(l = 60, c = 45) + 
    guides(color = 'none', fill = 'none') +
    # scale_color_manual(values = ) +
    facet_wrap(. ~ region + district, scales = "free_y") +
    ggtitle(predictor) 
  
  # Save plot with the predictor name
  ggsave(paste("05_prepare_training_data/visualisation/output/", 
               "som_training_district_dist_", predictor, ".png", sep = ""), 
         dpi = "print", height = 30, width = 40, units= "cm")
}

# 3 - Check co-correlations between predictors ---------------------------------

pred_data <- import("05_prepare_training_data/output/som_training_data.rds") |> 
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
    `Rainfall (lag 5)` = rainfall_lag5, 
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
  theme(legend.position = "none")

# save plot
ggsave("05_prepare_training_data/visualisation/output/som_training_corcorrelation.png", bg = 'white',
       width = 10, height = 10);  dev.off()

# 5 - Make a plot to identify missing data -------------------------------------

# Get unprocessed predictor and rename vars
pred_data_pp <- 
  import("05_prepare_training_data/output/som_training_data.rds") |> 
  filter(year > 2014) |> 
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
    `Rainfall` = rainfall, 
    `Combined Drought Index` = cdi, 
    `SAM admissions rate` = sam_admissions, 
    `Terms of trade (goat)` = tot_goat_cereal_smooth, 
    `Terms of trade (wage)` = tot_wage_cereal_smooth, 
    `Water price` = water_price, 
    `Number of deaths` = n_died, 
    `Number of deaths under 5` = n_died_u5, 
    `Person Time` = p_time, 
    `Person Time under 5` = p_time_u5) |> select(-month, -district)

# Get unprocessed predictor and run through the plot function
f_plot_missing_data(pred_data_pp)

# save plot
ggsave("05_prepare_training_data/visualisation/output/som_training_completeness.pdf",
       width = 10, height = 8, dpi = 320)