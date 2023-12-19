# Install or load required R packages-------------------------------------------
pacman::p_load(
  boot,        # Too compute model diagnostics
  broom,       # For wrangling tables of models
  ggalt,       # For fancy dumbbell plots
  ggcorrplot,  # For correlation matrix plotting
  ggplot2,     # Data visualization
  ggpubr,      # Arranging multiple plots into a single plot
  lubridate,   # Makes it easier to work with dates and times
  mast,        # Matching administrative names and geospatial operations
  scales,      # Scaling and formatting data for visualizations
  tidyverse,   # Tidyverse suite of packages
  zoo,         # For computing running means
  bnlearn,     # For Bayesian Networks
  rio,         # For importing files
  pbmcapply,   # For parallel computing 
  plotly)      # Interactive plot


# Function to process datasets
process_data <- function(data) {
  data <-  data|> 
    mutate(
      acled_event_rate = ifelse(acled_event_rate == 0, "0", "> 0"),
      acled_event_rate = factor(acled_event_rate, levels = c("0", "> 0"))
    ) |> 
    dplyr::rename(
      dep_rate = dep_rate_sqt,
      cdi = cdi_lag5,
      malaria_cases = malaria_cases_rate,
      cholera_cases = cholera_cases_rate,
      measles_cases = measles_cases_rate,
      sam_admission = sam_admissions_rate_lag1,
      tot_goat_cereal = tot_goat_cereal_smooth_lag4,
      tot_wage_cereal = tot_wage_cereal_smooth_lag4,
      water_price = water_price_smooth_lag4
    ) |> 
    as.data.frame()
  
  return(data)
  
}

# Read the training and prediction dataset -------------------------------------

# Training data (household mortality observations matched with predictors)
training_data <- rio::import('05_prepare_training_data/output/som_training_data.rds') |> 
  # Create crude death rate
  mutate(
    cdr = n_died  / p_time,
    cdr_u5 = n_died_u5  / p_time_u5)

# Prediction data (district-month predictor observations)
predictor_data <- rio::import('06_prepare_predicting_data/output/som_predicting_data.rds')

# Prepare dataset --------------------------------------------------------------

process_data <- function(data_source) {
  data_source %>%
    group_by(date) %>%
    summarise(
      across(
        c(acled_event_rate_scn, sam_admissions_rate_lag1_scn, 
          tot_wage_cereal_smooth_lag3_scn, 
          tot_goat_cereal_smooth_lag3_scn, 
          water_price_smooth_lag2_scn, measles_cases_rate_scn, 
          cholera_cases_rate_scn, malaria_cases_rate_scn, cdi_lag5_scn),
        mean
      )
    ) %>%
    tidyr::pivot_longer(cols = -c(date)) |> 
    ungroup()
}

data_train <- process_data(training_data)

data_comb <- process_data(predictor_data) %>%
  left_join(
    data_train, suffix = c("_pred", "_train"),
    by = c("date", "name")
  ) %>%
  pivot_longer(cols = c(value_pred, value_train), 
               names_to = "type", 
               values_to = "value") |> ungroup()


ggplot(data_comb, aes(y=value, x=date, color = type, na.rm = TRUE)) +
  facet_wrap(~name, scales = "free") +geom_line() +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 0.2, hjust=0.1), 
                     strip.text = element_text(size = 5))
ggsave('06_prepare_predicting_data/visualisation/output/som_comparison_training_predicting.png',
       dpi = "print", units = "cm", width = 30, height = 30)