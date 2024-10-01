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
training_data <- samrat::f_calculate_folds(training_data, 
                                           admin2_col='district',
                                           k_folds = 10)

# Define list of predictors ----------------------------------------------------
list_pred_to_try <- c('cdi_lag5_scn',
                      'measles_cases_rate',
                      'malaria_cases_rate',
                      "sam_admissions_rate_lag1_scn", 
                      'aidworkers_wounded_rate_scn', 
                      'tot_wage_cereal_smooth_lag2_scn', 
                      'tot_goat_cereal_smooth_lag3_scn', 
                      'water_price_smooth_lag2_scn',
                      'acled_fatalities_rate')


pca_result <- FactoMineR::PCA(training_data[, list_pred_to_try], graph = TRUE)  # Set graph to TRUE for graphical output
features <- pca_result$ind$coord[, 1:5]

training_data <- cbind(training_data, features)


# Apply PCA transformation to predicting data using the same PCA model
res_predicting <- predict(pca_result, newdata = predicting_data[, list_pred_to_try])$coord[, 1:4]
predicting_data <- cbind(predicting_data, res_predicting)

##Defime the model -------------------------------------------------------------
features <- c('Dim.1', 'Dim.2', 'Dim.3', 'Dim.4', 'Dim.5')
fit_formula <- as.formula(paste(pred_value," ~",
                                paste(features, collapse = " + "),
                                '+ offset(log(', p_time_var, '))'))


# Cross Validation -------------------------------------------------------------
# fit_formula <- as.formula(paste(pred_value," ~",
#                                 paste(features, collapse = " + "),
#                                 '+', '(1|district)',
#                                 '+ offset(log(', p_time_var, '))'))

glm_fit <- glm(formula = fit_formula, 
                            data = training_data, 
                            weights = qualityScore,
                            family = "poisson")

res <- samrat::f_cv(data_f = training_data, 
                    which_fold_f = "fold3",
                    k_folds_f = 10, 
                    fit_f = glm_fit, 
                    f_family_f = samrat::f_family, 
                    f_predict_f = samrat::f_predict, 
                    verbose = TRUE, 
                    admin1_col='region')

## PRINT MSE/RMSE
print(paste('MSE TRAIN: ', mean((res$cv_by_admin1_year$pred_train - res$cv_by_admin1_year$obs_train)^2), sep=""))
print(paste('MSE TEST: ', mean((res$cv_by_admin1_year$pred - res$cv_by_admin1_year$obs)^2), sep=""))
print(paste('RMSE TRAIN: ', sqrt(mean((res$cv_by_admin1_year$pred_train - res$cv_by_admin1_year$obs_train)^2)), sep=""))
print(paste('RMSE TEST: ', sqrt(mean((res$cv_by_admin1_year$pred - res$cv_by_admin1_year$obs)^2)), sep=""))
print(paste('MAE TRAIN: ', Metrics::mae(res$cv_by_admin1_year$pred_train, res$cv_by_admin1_year$obs_train), sep=""))
print(paste('MAE: ', Metrics::mae(res$cv_by_admin1_year$pred, res$cv_by_admin1_year$obs), sep=""))
print(paste('Error Rate: ', (sum(res$cv_by_admin1_year$pred)-sum(res$cv_by_admin1_year$obs))/sum(res$cv_by_admin1_year$obs)*100, sep=""))

## Plot Cross Validation -------------------------------------------------------
samrat::f_cv_plot(cv_data=res, plot_type = 'scatter', admin1_col = 'region',
                  dir_output_f = '07_model_exploration/visualization/output/',
                  save_plot = TRUE)
