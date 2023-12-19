#source
source("09_forecasting/code/00_source_function.R")

## Import GLMM and GLMM Under 5 Models -----------------------------------------
# Import Model Overall and Under 5
glm_model <- rio::import('10_rshiny_app/data/som_glm_model.rds')
glm_model_u5 <- rio::import('10_rshiny_app/data/som_glm_model_u5.rds')

# Import the data
static_data <- rio::import('09_forecasting/output/som_statict_forecasting_data.csv')
pessimist_data <- rio::import('09_forecasting/output/som_pessimistic_forecasting_data.csv')
optimistic_data <- rio::import('09_forecasting/output/som_optimistic_forecasting_data.csv')

# Calculate Boostrapping Data --------------------------------------------------

boostrapping_results_median <- f_boostrapping(glm_model, static_data)
boostrapping_results_pess <- f_boostrapping(glm_model, pessimist_data)
boostrapping_results_opt <- f_boostrapping(glm_model, optimistic_data)

boostrapping_results_u5_median <- f_boostrapping(glm_model_u5, static_data)
boostrapping_results_u5_pess <- f_boostrapping(glm_model_u5, pessimist_data)
boostrapping_results_u5_opt <- f_boostrapping(glm_model_u5, optimistic_data)

# Compute the excess of death---------------------------------------------------
#First choose the date for period1 
start_date_period_1 <- "2015-01-01"
end_date_period_1 <- "2016-06-01"
start_date_period_2 <- "2022-01-01"
end_date_period_2 <- "2022-12-01"

# Second select the level of aggregation
agg_level <- 'region'

# Then compute the excess for overall (static, optimistic, pessimistic cases)
excess_res_median <- f_counterfactuals(boostrapping_results_median, resp_var = 'cdr', agg_level = agg_level, 
                                start_date_per_1 = start_date_period_1, 
                                end_date_per_1 = end_date_period_1, 
                                start_date_per_2 = start_date_period_2, 
                                end_date_per_2 = end_date_period_2, 
                                nb_boostrap=1000, type_model = 'glm')

excess_res_pess <- f_counterfactuals(boostrapping_results_pess, resp_var = 'cdr', agg_level = agg_level, 
                                       start_date_per_1 = start_date_period_1, 
                                       end_date_per_1 = end_date_period_1, 
                                       start_date_per_2 = start_date_period_2, 
                                       end_date_per_2 = end_date_period_2, 
                                       nb_boostrap=1000, type_model = 'glm')

excess_res_opt <- f_counterfactuals(boostrapping_results_opt, resp_var = 'cdr', agg_level = agg_level, 
                                       start_date_per_1 = start_date_period_1, 
                                       end_date_per_1 = end_date_period_1, 
                                       start_date_per_2 = start_date_period_2, 
                                       end_date_per_2 = end_date_period_2, 
                                       nb_boostrap=1000, type_model = 'glm')

# Then compute the excess for under 5 (static, optimistic, pessimistic cases)
excess_res_u5_median <- f_counterfactuals(boostrapping_results_u5_median, resp_var = 'cdr_u5', agg_level = agg_level, 
                                   start_date_per_1 = start_date_period_1, 
                                   end_date_per_1 = end_date_period_1, 
                                   start_date_per_2 = start_date_period_2, 
                                   end_date_per_2 = end_date_period_2, 
                                   nb_boostrap=1000, type_model = 'glm')

excess_res_u5_pess <- f_counterfactuals(boostrapping_results_u5_pess, resp_var = 'cdr_u5', agg_level = agg_level, 
                                          start_date_per_1 = start_date_period_1, 
                                          end_date_per_1 = end_date_period_1, 
                                          start_date_per_2 = start_date_period_2, 
                                          end_date_per_2 = end_date_period_2, 
                                          nb_boostrap=1000, type_model = 'glm')

excess_res_u5_opt <- f_counterfactuals(boostrapping_results_u5_opt, resp_var = 'cdr_u5', agg_level = agg_level, 
                                          start_date_per_1 = start_date_period_1, 
                                          end_date_per_1 = end_date_period_1, 
                                          start_date_per_2 = start_date_period_2, 
                                          end_date_per_2 = end_date_period_2, 
                                          nb_boostrap=1000, type_model = 'glm')

#Merge OVerall and U5 for the different scenarios 
clean_median_excess <- f_clean_counterfactuals_table(merge(excess_res_median, excess_res_u5_median, by=agg_level))
clean_pess_excess <- f_clean_counterfactuals_table(merge(excess_res_pess, excess_res_u5_pess, by=agg_level))
clean_opt_excess <- f_clean_counterfactuals_table(merge(excess_res_opt, excess_res_u5_opt, by=agg_level))

# save the clean data
write.csv(clean_median_excess, paste("09_forecasting/output/som_clean_forecast_count_statistic_", 
                                  lubridate::year(start_date_period_1), '_vs_', 
                                  lubridate::year(start_date_period_2), ".csv", sep=""), row.names=FALSE)

# save the clean data
write.csv(clean_pess_excess, paste("09_forecasting/output/som_clean_forecast_count_pessimistic_", 
                                  lubridate::year(start_date_period_1), '_vs_', 
                                  lubridate::year(start_date_period_2), ".csv", sep=""), row.names=FALSE)

# save the clean data
write.csv(clean_opt_excess, paste("09_forecasting/output/som_clean_forecast_count_optimistic_", 
                                  lubridate::year(start_date_period_1), '_vs_', 
                                  lubridate::year(start_date_period_2), ".csv", sep=""), row.names=FALSE)

# Produce final table report using pessistic and optimistic as CI

res <- merge(excess_res_median |> dplyr::select(c(toll_excess_period, dr_excess_period, agg_level)), 
      excess_res_opt |> dplyr::select(c(toll_excess_period, dr_excess_period, agg_level)) |> 
        dplyr::rename(toll_excess_period_low = toll_excess_period, dr_excess_period_low = dr_excess_period), 
      by=agg_level)
res <- merge(res, excess_res_pess|> dplyr::select(c(toll_excess_period, dr_excess_period, agg_level)) |> 
               dplyr::rename(toll_excess_period_up = toll_excess_period,dr_excess_period_up = dr_excess_period), 
             by=agg_level)
res <- merge(res, excess_res_u5_median|> dplyr::select(c(toll_excess_period_u5, dr_excess_period_u5,agg_level)), 
             by=agg_level)
res <- merge(res, excess_res_u5_opt|> dplyr::select(c(toll_excess_period_u5,dr_excess_period_u5, agg_level)) |> 
               dplyr::rename(toll_excess_period_u5_low = toll_excess_period_u5, dr_excess_period_u5_low = dr_excess_period_u5), 
             by=agg_level)
res <- merge(res, excess_res_u5_pess|> dplyr::select(c(toll_excess_period_u5,dr_excess_period_u5, agg_level)) |> 
               dplyr::rename(toll_excess_period_u5_up = toll_excess_period_u5, dr_excess_period_u5_up = dr_excess_period_u5), 
             by=agg_level)
      
clean_res <- f_clean_counterfactuals_table(res)

# save the clean data
write.csv(clean_res, paste("09_forecasting/output/som_clean_forecast_ci_scenarios_", 
                                  lubridate::year(start_date_period_1), '_vs_', 
                                  lubridate::year(start_date_period_2), ".csv", sep=""), row.names=FALSE)