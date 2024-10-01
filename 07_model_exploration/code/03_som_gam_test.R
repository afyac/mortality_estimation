# Define the variable 
resp_var <- 'n_died'
p_time_var <- samrat::f_define_p_time(resp_var)
pred_value <- samrat::f_define_pred_value(resp_var)
pop_var <- samrat::f_define_pop_var(resp_var)

# Upload the training and predicting data --------------------------------------
training_data <- rio::import('05_prepare_training_data/output/som_training_data.rds')|> 
  dplyr::filter(date >= "2015-01-01" & p_time > 0 ) |> tidyr::drop_na() 

predicting_data <- rio::import('06_prepare_predicting_data/output/som_predicting_data.rds')|> 
  dplyr::filter(date >= "2015-01-01") |> tidyr::drop_na()

# Calculate folds --------------------------------------------------------------
training_data <- samrat::f_calculate_folds(training_data, admin2_col='district',
                                           k_folds = 10)

# Define list of predictors ----------------------------------------------------
list_pred_to_try <- c('cdi_lag5_scn',
                      'measles_cases_rate_cat',
                      'malaria_cases_rate_cat',
                      "sam_admissions_rate_lag1_scn")

# Feature selection ------------------------------------------------------------
samrat::f_feature_selection(list_pred_to_try = list_pred_to_try, 
                            path = '07_model_exploration/output/res_cv_gam.csv',
                            pred_value = pred_value, 
                            training_data = training_data,
                            which_fold_f = 'fold3',
                            p_time_var = p_time_var, 
                            model = 'gam', 
                            admin2_col = NULL
)

# Define list of predictors ----------------------------------------------------
list_pred_to_try <- c('cdi_lag5_scn',
                      'measles_cases_rate_cat',
                      'malaria_cases_rate_cat',
                      "sam_admissions_rate_lag1_scn")


# Cross Validation -------------------------------------------------------------
fit_formula <- as.formula(paste(pred_value," ~",
                                paste(list_pred_to_try, collapse = " + "), 
                                '+ offset(log(', p_time_var, '))'))
gam_fit <- mgcv::gam(formula = fit_formula, data = training_data, weights = qualityScore,
               family = "poisson")

res <- samrat::f_cv(data_f = training_data, which_fold_f = "fold3",
                    k_folds_f = 10, fit_f = gam_fit, f_family_f = samrat::f_family, 
                    f_predict_f = samrat::f_predict, verbose = TRUE)

## Plot Cross Validation -------------------------------------------------------
samrat::f_cv_plot(res, plot_type = "scatter", admin1_col = 'region',
                  dir_output_f = '07_model_exploration/visualization/output/',
                  save_plot = TRUE)