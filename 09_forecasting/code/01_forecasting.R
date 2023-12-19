palette_cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")

##-----------------------------------------------------------------
# 1. Import data, functions and other variables
## ----------------------------------------------------------------

## Source functions
source("09_forecasting/code/00_source_function.R")

## Import data
preds_df <- rio::import("10_rshiny_app/data/som_predicting_data.rds") |>
  dplyr::filter(date >= "2015-01-01" & date <= "2023-09-01")

# Forecast Date and Lenght
DATE_FORECAST <- max(preds_df$date)
# how long are we forecasting for
forecast_length <- 5

end_date_forecast <- max(preds_df$date) + months(forecast_length) 

# ADMIN2 and TIME_UNIT
analysis_strata <- rio::import("10_rshiny_app/data/som_admin2.xlsx")
time_unit_data <- f_generate_time_units(
  y_analysis_start = 2010, y_analysis_end = lubridate::year(end_date_forecast),
  m_analysis_start = 1, m_analysis_end = lubridate::month(end_date_forecast)
)

# Import Model Overall and Under 5
glm_model <- rio::import('10_rshiny_app/data/som_glm_model.rds')
glm_model_u5 <- rio::import('10_rshiny_app/data/som_glm_model_u5.rds')


##------------------------------------------------------------------
# 2. Forecast Data 
## -----------------------------------------------------------------
forecast_data <- f_forecast_using_simple_model(preds_df, forecast_length, time_unit_data, analysis_strata)
static_data <- forecast_data[[1]]
pessimistic_data <- forecast_data[[2]]
optimistic_data <- forecast_data[[3]]

#Save the dataset in order to check it if needed
write.csv(static_data, '09_forecasting/output/som_statict_forecasting_data.csv', row.names = FALSE)
write.csv(pessimistic_data, '09_forecasting/output/som_pessimistic_forecasting_data.csv', row.names = FALSE)
write.csv(optimistic_data, '09_forecasting/output/som_optimistic_forecasting_data.csv', row.names = FALSE)

##-------------------------------------------------------------------
# 3. Generate plot when the data are not modified
##-------------------------------------------------------------------
# First for Overall
static_data_pred <- f_pred_forecast(static_data, glm_model, name_output='static_scen')
pessimistic_data_pred <- f_pred_forecast(pessimistic_data, glm_model, name_output='pessimistic_scen')
optimistic_data_pred <- f_pred_forecast(optimistic_data, glm_model, name_output='optmistic_scen')

## SAVE THE DATA FOR COUNTERFACTUALS--------------------------------------------------------------
write.csv(static_data_pred, '09_forecasting/output/som_statict_forecasting_data_predictions.csv', row.names = FALSE)
write.csv(pessimistic_data_pred, '09_forecasting/output/som_pessimistic_forecasting_predictions.csv', row.names = FALSE)
write.csv(optimistic_data_pred, '09_forecasting/output/som_optimistic_forecasting_predictions.csv', row.names = FALSE)

#Same for Under5
static_data_pred_u5 <- f_pred_forecast(static_data, glm_model_u5, name_output='static_scen')
pessimistic_data_pred_u5 <- f_pred_forecast(pessimistic_data, glm_model_u5, name_output='pessimistic_scen')
optimistic_data_pred_u5 <- f_pred_forecast(optimistic_data, glm_model_u5, name_output='optmistic_scen')

## SAVE THE DATA FOR COUNTERFACTUALS--------------------------------------------------------------
write.csv(static_data_pred_u5, '09_forecasting/output/som_statict_forecasting_data_predictions_u5.csv', row.names = FALSE)
write.csv(pessimistic_data_pred_u5, '09_forecasting/output/som_pessimistic_forecasting_predictions_u5.csv', row.names = FALSE)
write.csv(optimistic_data_pred_u5, '09_forecasting/output/som_optimistic_forecasting_predictions_u5.csv', row.names = FALSE)

##-------------------------------------------------------------------
# 4. Plot the forecasting curve with static modifications
##-------------------------------------------------------------------
plot_stat <- f_plot_forecasting(static_data_pred, pessimistic_data_pred, optimistic_data_pred, 
                                static_data_pred_u5, pessimistic_data_pred_u5, optimistic_data_pred_u5,
                                DATE_FORECAST,
                                start_period="2021-06-10")
# Save the plot for the report
ggsave('09_forecasting/visualization/output/som_glmm_global_forecasting.png', 
       dpi = "print", width = 25, height = 25, units = "cm")

##---------------------------------------------------------------------
# 5. Calculate Correlation Matrix
##---------------------------------------------------------------------
MODIFIED_FEATURES <- c('measles_cases_rate', 'sam_admissions_rate_lag1_scn', 
                       'malaria_cases_rate', 'acled_event_rate', 'water_price_smooth_lag2_scn',
                       'tot_wage_cereal_smooth_lag3_scn', 'dep_rate_sqt_scn', 'prop_idp_scn')
PERCENT_MODIFICATIONS <- c(0,0, 0, -0.3, 
                           0, 0, 0, 0)

res_corr <- f_return_data_corr_pred(MODIFIED_FEATURES, PERCENT_MODIFICATIONS, 
                        static_data, pessimistic_data, optimistic_data, 
                        DATE_FORECAST, preds_df, glm_model, glm_model_u5)

##---------------------------------------------------------------------
# 6. Plot the forecasting curve
##---------------------------------------------------------------------

plot_corr <-f_plot_forecasting(res_corr[[1]], res_corr[[2]], res_corr[[3]],
                   res_corr[[4]], res_corr[[5]], res_corr[[6]],
                   DATE_FORECAST,
                   start_period="2021-06-10")

# Save the plot for the report
ggsave('09_forecasting/visualization/output/som_glmm_fore_correlation.png', 
       dpi = "print", width = 25, height = 25, units = "cm")
