# Install or load required R packages-------------------------------------------

# Upload functions
source('08_define_final_model/code/00_source_functions.R')

# Upload the training and predicting data ---------------------------------------
training_data <- rio::import('05_prepare_training_data/output/som_training_data.rds')|> 
  dplyr::filter(date >= "2015-01-01"& date <= "2023-09-01" & p_time > 0) |> tidyr::drop_na() 

predicting_data <- rio::import('06_prepare_predicting_data/output/som_predicting_data.rds')|> 
  dplyr::filter(date >= "2015-01-01" & date <= "2023-09-01") |> tidyr::drop_na()

# select the variables used of the predictions ---------------------------------
features <- c('cdi_lag5_scn', 'measles_cases_rate_cat',
              'malaria_cases_rate_cat', 'sam_admissions_rate_lag1_scn')


# Train and predict and return the boostrap data
boostrapping_results <- f_pred_glm(training_data = training_data,
           predicting_data = predicting_data, 
           pred_var=features, resp_var='cdr')

# save this boostrapping results for the counterfactuals
boostrapping_results |>
  saveRDS("08_define_final_model/output/som_boostraping_data.rds")

# Generate the results with mean, low and upper data
final_res <- f_generate_final_results(boostrapping_results, agg_level = 'date', 
                         nb_boostrap = 1000, resp_var = 'cdr')

final_res |> 
  saveRDS("08_define_final_model/output/som_final_toll_dr.rds")
