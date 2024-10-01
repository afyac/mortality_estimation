# Install or load required R packages-------------------------------------------
# Function to process datasets
process_data <- function(data) {
  data <-  data|> 
    dplyr::mutate(
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
  dplyr::mutate(
    cdr = n_died  / p_time,
    cdr_u5 = n_died_u5  / p_time_u5)

# Prediction data (district-month predictor observations)
predictor_data <- rio::import('06_prepare_predicting_data/output/som_predicting_data.rds')

# Prepare dataset --------------------------------------------------------------

process_data <- function(data_source) {
  data_source %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      dplyr::across(
        c(acled_event_rate_scn, sam_admissions_rate_lag1_scn, 
          tot_wage_cereal_smooth_lag3_scn, 
          tot_goat_cereal_smooth_lag3_scn, 
          water_price_smooth_lag2_scn, measles_cases_rate_scn, 
          cholera_cases_rate_scn, malaria_cases_rate_scn, cdi_lag5_scn),
        mean
      )
    ) %>%
    tidyr::pivot_longer(cols = -c(date)) |> 
    dplyr::ungroup()
}

data_train <- process_data(training_data)

data_comb <- process_data(predictor_data) %>%
  dplyr::left_join(
    data_train, suffix = c("_pred", "_train"),
    by = c("date", "name")
  ) %>%
  tidyr::pivot_longer(cols = c(value_pred, value_train), 
               names_to = "type", 
               values_to = "value") |> dplyr::ungroup()


ggplot2::ggplot(data_comb, ggplot2::aes(y=value, x=date, color = type, na.rm = TRUE)) +
  ggplot2::facet_wrap(~name, scales = "free") + ggplot2::geom_line() +
  ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.2, hjust=0.1), 
                     strip.text = ggplot2::element_text(size = 5))
ggplot2::ggsave('06_prepare_predicting_data/visualisation/output/som_comparison_training_predicting.png',
       dpi = "print", units = "cm", width = 30, height = 30)