# 1 - Visualizations of each predictor raw per time unit -----------------------

# Define the crisis periods in a data frame
# Define the crisis periods with different colors for each
crisis_periods <- data.frame(
  xmin = as.Date(c("2017-01-01", "2022-01-01")),
  xmax = as.Date(c("2018-12-31", "2023-12-31")),
  ymin = rep(-Inf, 2),  # Start from the bottom of the plot
  ymax = rep(Inf, 2),   # Go to the top of the plot
  period = c("2017-2018", "2022-2023"),  # Label for the periods
  color =  c("#e2dede", "#8e8b8b")  
)

data_long <-  rio::import("04_process_predictors/output/som_predictors_data_complete.rds") |>
  dplyr::select(acled_fatalities_rate, acled_event_rate, malaria_cases_rate, 
                measles_cases_rate, cholera_cases_rate, cdi_lag5_scn, 
                rainfall_rollmean_lag5, tot_goat_cereal_smooth_lag2_scn,
                tot_wage_cereal_smooth_lag2_scn, water_price_smooth_lag2_scn, 
                sam_admissions_rate_lag1_scn, date)|> 
  dplyr::group_by(date) |> 
  dplyr::mutate(across(
    dplyr::where(is.numeric),
    .fns = mean
  )) |> 
  # Convert data to long format
  tidyr::pivot_longer(
    cols = -date,  
    names_to = "predictor_name",  
    values_to = "value" 
  ) |> 
  dplyr::mutate(variable = dplyr::case_when(
    stringr::str_detect(predictor_name, 'acled_fatalities') ~ 'acled_fatalities',
    stringr::str_detect(predictor_name, "acled_event") ~ "acled_event",
    stringr::str_detect(predictor_name, "malaria_cases") ~ "malaria_cases",
    stringr::str_detect(predictor_name, "measles_cases") ~ "measles_cases",
    stringr::str_detect(predictor_name, "cholera_cases") ~ "cholera_cases",
    stringr::str_detect(predictor_name, "rainfall") ~ "rainfall",
    stringr::str_detect(predictor_name, "cdi") ~ "CDI",
    stringr::str_detect(predictor_name, "sam_admissions") ~ "sam_admissions",
    stringr::str_detect(predictor_name, "tot_wage_cereal") ~ "tot_wage_cereal",
    stringr::str_detect(predictor_name, "tot_goat_cereal") ~ "tot_goat_cereal",
    stringr::str_detect(predictor_name, "water_price") ~ "water_price",
  ),
  variable = factor(variable))

label_predictor <- c("acled_event_rate" = "Incidence of insecurity events",
                     "acled_fatalities_rate" = "Incidence of insecurity fatalities",
                     "tot_wage_cereal_smooth_lag2_scn" = "Terms of trade (wage/cereal)",
                     "tot_goat_cereal_smooth_lag2_scn" = "Terms of trade (goat/cereal)",
                     "water_price_smooth_lag2_scn" = "Water price",
                     "malaria_cases_rate" = "Malaria Incidence",
                     "measles_cases_rate" = "Measles Incidence",
                     "cholera_cases_rate" = "Cholera Incidence",
                     "sam_admissions_rate_lag1_scn" = "Malnutrition incidence",
                     "cdi_lag5_scn" = "Extent of drought conditions",
                     "rainfall_rollmean_lag5" = "Rainfall")

plot <- data_long |> 
  ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +

  
  # Main plot aesthetics
  ggplot2::geom_point(alpha = 0.5, size = 0.4) +
  ggplot2::geom_line(color = "steelblue") +
  
  # Facet by predictor name
  ggplot2::facet_wrap(~predictor_name, scales = "free_y", nrow = 4, ncol = 3,
                      labeller = ggplot2::labeller(predictor_name = label_predictor)) +
  ggplot2::labs(x = "", y = ggplot2::element_blank(), 
                title = ggplot2::element_blank()) +
  
  # Ensure fill is mapped to variable
  ggplot2::geom_area(ggplot2::aes(fill = variable), alpha = 0.5, show.legend = FALSE) +
  
  # Crisis periods shading
  ggplot2::annotate("rect", xmin = as.Date("2017-01-01"),
                    xmax = as.Date("2018-01-01"), 
                    ymin = -Inf, ymax = Inf,
                    alpha = .2, fill='#0072B2')+
  # Crisis periods shading
  ggplot2::annotate("rect", xmin = as.Date("2022-01-01"),
                    xmax = as.Date("2023-01-01"), 
                    ymin = -Inf, ymax = Inf,
                    alpha = .2, fill='#861313')+
  # Add text only to the first facet
  ggplot2::annotate("text", x = as.Date("2017-07-01"), y = Inf, 
                    label = "2017-2018", vjust = 1.5, hjust = 0.5, 
                    color = "#0072B2", size = 6, 
                    data = subset(data_long, predictor_name == data_long$predictor_name[1])) +
  ggplot2::annotate("text", x = as.Date("2022-07-01"), y = Inf, 
                    label = "2022-2023", vjust = 1.5, hjust = 0.5, 
                    color = "#861313", size = 6, 
                    data = subset(data_long, predictor_name == data_long$predictor_name[1])) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(vjust=1,hjust=1, angle=0, size=ggplot2::rel(2)), 
                 strip.text = ggplot2::element_text(size = 20)) +
  ggplot2::guides(color = "none") # Remove legend for predictors

# save plot
ggplot2::ggsave("04_process_predictors/visualisation/output/som_predictors_time.png", bg = 'white',
       width = 16, height = 12)

# 2 - Visualizations of each predictor raw per time unit per district ----------

data_long <- rio::import("04_process_predictors/output/som_predictors_data_complete.rds") |> 
  dplyr::select(
    date, region, district, acled_event, acled_fatalities, 
    aidworkers_killed, aidworkers_wounded, dep_rate, pop_average, 
    prop_idp, cholera_cases, malaria_cases, measles_cases, 
    cdi_lag5, sam_admissions_rate_lag1, 
    tot_goat_cereal_smooth_lag4, tot_wage_cereal_smooth_lag4, 
    tot_wage_cereal_smooth_lag4, water_price_smooth_lag4) |> 
  dplyr::group_by(region, district, date) |> 
  dplyr::mutate(dplyr::across(
    dplyr::where(is.numeric),
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
  ggplot2::ggsave(paste("04_process_predictors/visualisation/output/", 
               "som_pred_district_dist_", predictor, ".png", sep = ""), 
         dpi = "print", height = 30, width = 40, units= "cm")
}

# 3 - Check co-correlations between predictors ---------------------------------

pred_data <- rio::import("04_process_predictors/output/som_predictors_data_complete.rds") |> 
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
    `Combined Drought Index (lag 5)` = cdi_lag5, 
    `SAM admissions rate (lag 1)` = sam_admissions_rate_lag1, 
    `Terms of trade (goat, lag 4)` = tot_goat_cereal_smooth_lag4, 
    `Terms of trade (wage, lag 4)` = tot_wage_cereal_smooth_lag4, 
    `Water price (lag 4)` = water_price_smooth_lag4) 

# run correlation plot
pred_data |> 
  cor(use = "pairwise.complete.obs")  |> 
  ggcorrplot::ggcorrplot(show.diag = TRUE, 
                         type = "lower", lab = TRUE, 
                         lab_size = 3) +
  ggplot2::theme(legend.position = "none")

# save plot
ggplot2::ggsave("04_process_predictors/visualisation/output/som_predictors_corcorrelation.png", bg = 'white',
       width = 10, height = 10)


# 5 - Make a plot to identify missing data -------------------------------------

# Get unprocessed predictor and rename vars
pred_data_pp <- 
  rio::import("04_process_predictors/output/som_predictors_none_complete.rds") |> 
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
    `Combined Drought Index` = cdi, 
    `SAM admissions rate` = sam_admissions, 
    `Terms of trade (goat)` = tot_goat_cereal_smooth, 
    `Terms of trade (wage)` = tot_wage_cereal_smooth, 
    `Water price` = water_price) |> dplyr::select(-month, -district)

# Get unprocessed predictor and run through the plot function
mast::f_plot_missing_data(pred_data_pp)

# save plot
ggplot2::ggsave("04_process_predictors/visualisation/output/som_prd_completeness.pdf",
       width = 10, height = 8, dpi = 320)


# 6 - Plot distribution of each predictors -------------------------------------
predictors <- c(
  "acled_event_rate_scn", 
  "malaria_cases_rate_log",
  "measles_cases_rate_log",
  "cholera_cases_rate_log",
  "dep_rate_sqt", "prop_idp",
  "cdi_lag5", "sam_admissions_rate_lag1", 
  "tot_wage_cereal_smooth_lag4", 
  "water_price_smooth_lag4")

data <- rio::import("04_process_predictors/output/som_predictors_data_complete.rds") 

data$malaria_cases_rate_log <- exp(data$malaria_cases_rate) 
data$measles_cases_rate_log <- exp(data$measles_cases_rate)
data$cholera_cases_rate_log <- exp(data$cholera_cases_rate)

data |>
  dplyr::select(predictors) |>                     # Keep only numeric columns
  tidyr::gather() |>                             # Convert to key-value pairs
  ggplot2::ggplot(ggplot2::aes(value)) +                     # Plot the values
  ggplot2::facet_wrap(~ key, scales = "free") +   # In separate panels
  ggplot2::geom_density() + ggplot2::theme_bw()

ggplot2::ggsave(filename="04_process_predictors/visualisation/output/som_density_variables.png",
       width = 10, height = 8, dpi = 320)

