## Fit, predict and generate boostrapping using the final model ----------------
END_DATE_ANALYSIS <- "2024-06-01"

# Upload the training and predicting data ---------------------------------------
training_data <- rio::import('05_prepare_training_data/output/som_training_data.rds')|> 
  dplyr::filter(date >= "2015-01-01"& date <= END_DATE_ANALYSIS & p_time > 0) |> 
  tidyr::drop_na() 

predicting_data <- rio::import('06_prepare_predicting_data/output/som_predicting_data.rds')|> 
  dplyr::filter(date >= "2015-01-01" & date <= END_DATE_ANALYSIS) |> 
  tidyr::drop_na()

# select the variables used of the predictions ---------------------------------
features <- c('cdi_lag5_scn', 'measles_cases_rate_cat', 
              'sam_admissions_rate_lag1_scn', 'malaria_cases_rate_cat')

# Define the final model
##Define the variables under study overall
resp_var <- 'cdr'
p_time_var <- samrat::f_define_p_time(resp_var)
pred_value <- samrat::f_define_pred_value(resp_var)
pop_var <- samrat::f_define_pop_var(resp_var)
annot <- samrat::f_define_annot_value(resp_var)

##Defime the model -------------------------------------------------------------
fit_formula <- as.formula(paste(pred_value," ~",
                                paste(features, collapse = " + "),
                                '+','(1|district)', '+ offset(log(', p_time_var, '))'))
glm_fit <- glmmTMB::glmmTMB(formula = fit_formula,
                            data = training_data, weights = qualityScore,
                            family = "poisson")
# Save it
glm_fit |>
  saveRDS(paste('08_define_final_model/output/', 'som_glm_model', annot, '.rds', sep=""))

# Train and predict and return the boostrap data
boostrapping_results <- samrat::f_pred_glm(training_data=training_data, 
                                           predicting_data=predicting_data,
                                           pred_var=features, resp_var = resp_var,
                                           formula = fit_formula,
                                           model = glm_fit,
                                           admin1_col = 'region',
                                           admin2_col = 'district',
                                           name_output='som_glm_model')

# save this boostrapping results for the counterfactuals
boostrapping_results |>
  saveRDS("08_define_final_model/output/som_boostraping_data.rds")

# Generate the results with mean, low and upper data
final_res <- samrat::f_generate_final_results(boostrapping_results, 
                                              agg_level = 'date', 
                                              nb_boostrap = 1000, resp_var = 'cdr')

final_res |> 
  saveRDS("08_define_final_model/output/som_final_toll_dr.rds")
