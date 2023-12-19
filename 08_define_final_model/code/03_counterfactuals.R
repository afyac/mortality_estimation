
# Upload functions
source('08_define_final_model/code/00_source_functions.R')


# Upload Boostrapping overall and under 5
boostrapping_results <- rio::import("08_define_final_model/output/som_boostraping_data.rds")
boostrapping_results_u5 <- rio::import("08_define_final_model/output/som_boostraping_data_u5.rds")

# Compute the excess of death---------------------------------------------------
#First choose the date for period1 
start_date_period_1 <- "2015-01-01"
end_date_period_1 <- "2016-06-01"
start_date_period_2 <- "2022-01-01"
end_date_period_2 <- "2022-12-01"

# Second select the level of aggregation
agg_level <- 'region'

# Then compute the excess for overall and under 5
excess_res <- f_counterfactuals(boostrapping_results, resp_var = 'cdr', agg_level = agg_level, 
                                start_date_per_1 = start_date_period_1, 
                              end_date_per_1 = end_date_period_1, 
                              start_date_per_2 = start_date_period_2, 
                              end_date_per_2 = end_date_period_2, 
                              nb_boostrap=1000, type_model = 'glm')

excess_res_u5 <- f_counterfactuals(boostrapping_results_u5, resp_var = 'cdr_u5', agg_level = agg_level, 
                                start_date_per_1 = start_date_period_1, 
                                end_date_per_1 = end_date_period_1, 
                                start_date_per_2 = start_date_period_2, 
                                end_date_per_2 = end_date_period_2, 
                                nb_boostrap=1000, type_model = 'glm')

# merge both to have the results overall and under 5
excess_res <- merge(excess_res, excess_res_u5, by=agg_level)

# save the results 
excess_res |> 
  saveRDS(paste("08_define_final_model/output/som_counterfactuals_", 
                lubridate::year(start_date_period_1), '_vs_', 
                lubridate::year(start_date_period_2), ".rds", sep=""))

# Add the total column and clean up names
clean_res_excess <- f_clean_counterfactuals_table(excess_res)

# save the clean data
write.csv(clean_res_excess, paste("08_define_final_model/output/som_clean_counterfactuals_", 
                                  lubridate::year(start_date_period_1), '_vs_', 
                                  lubridate::year(start_date_period_2), ".csv", sep=""), row.names=FALSE)

# Upload the actual deaths and select the date you want to study----------------
actual_res <- boostrapping_results |>
  filter(date >= start_date_period_2 & date <= end_date_period_2)
actual_res_u5 <- boostrapping_results_u5 |>
  filter(date >= start_date_period_2 & date <= end_date_period_2)

actual_res <- f_generate_final_results(actual_res, agg_level = c('region'), 
                                      nb_boostrap = 1000, resp_var = 'cdr')
actual_res_u5 <- f_generate_final_results(actual_res_u5, agg_level = c('region'), 
                                       nb_boostrap = 1000, resp_var = 'cdr_u5')

# merge overall and under 5
actual_res <- merge(actual_res, actual_res_u5, by=agg_level)

# clean the data into a nice format and save it
clean_res_actual <- f_clean_actual_table(actual_res)

# save the clean actual data
write.csv(clean_res_actual, paste("08_define_final_model/output/som_clean_actual_", 
                                  lubridate::year(start_date_period_2), '_', 
                                  lubridate::month(start_date_period_2), '_to_', 
                                  lubridate::year(end_date_period_2),'_', 
                                  lubridate::month(end_date_period_2), ".csv", sep=""), row.names=FALSE)
