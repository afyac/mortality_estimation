f_counterfactuals <- function(boostrapping_results, resp_var,
                              agg_level, admin2_col,
                              start_date_per_1 = "2015-01-01",
                              end_date_per_1 = "2016-06-01",
                              start_date_per_2 = "2017-01-01",
                              end_date_per_2 = "2019-06-01",
                              nb_boostrap=1000, type_model = 'glm'){
  ##Define the variables under study overall or under5
  p_time_var <- samrat::f_define_p_time(resp_var)
  pred_value <- samrat::f_define_pred_value(resp_var)
  pop_var <- samrat::f_define_pop_var(resp_var)
  annot <- samrat::f_define_annot_value(resp_var)
  
  ##First Calculate the median (counterfactual) of the non_crisis period (or period 1)
  median_c <- boostrapping_results |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(admin2_col)))) |>
    dplyr::filter(date >= start_date_per_1 & date <= end_date_per_1) |>
    dplyr::mutate(median_c = mean(exp(pred))) |>
    dplyr::select(median_c, district) |>
    dplyr::distinct(district, .keep_all = TRUE) |>
    dplyr::ungroup()
  
  boostrapping_results <- merge(boostrapping_results, median_c,
                                by=c(admin2_col), all.x=TRUE)
  
  boostrapping_results$median_c <- boostrapping_results$median_c*boostrapping_results[, pop_var]*lubridate::days_in_month(boostrapping_results$date)
  
  ## Calculate the excess -- number of death crisis - number of death non crisis
  boostrapping_results <- subset(boostrapping_results, date >= start_date_per_2  & date <= end_date_per_2)
  boostrapping_results[, c(paste('X_e', seq(1:nb_boostrap), sep="")) ] <- boostrapping_results[, c(paste('X', seq(1:nb_boostrap), sep="")) ] - 
    boostrapping_results$median_c
  
  #Compute ptime if wanted
  boostrapping_results[, p_time_var] <- boostrapping_results[, pop_var]*lubridate::days_in_month(boostrapping_results$date)
  
  # D_e --  Matrix of excess death -- the number
  if(length(agg_level) == 1){
    D_e <- aggregate(boostrapping_results[, c(paste('X_e', seq(1:nb_boostrap), sep="")) ] ,
                     by = list(boostrapping_results[, agg_level]), FUN = sum )
  }else{
    D_e <- aggregate(boostrapping_results[, c(paste('X_e', seq(1:nb_boostrap), sep="")) ] ,
                     by = boostrapping_results[, agg_level], FUN = sum )
  }
  
  colnames(D_e)[1:length(agg_level)] <- agg_level
  
  if(length(agg_level) == 1){
    pred_agg_region_ptime <- aggregate(boostrapping_results[, c(p_time_var)] ,
                                       by = list(boostrapping_results[, agg_level]), FUN = sum )
  }else{
    pred_agg_region_ptime <- aggregate(boostrapping_results[, c(p_time_var)] ,
                                       by = boostrapping_results[, agg_level], FUN = sum )
  }
  
  colnames(pred_agg_region_ptime) <- c(agg_level, p_time_var)
  
  ## Calculate mean, q1 and q3 of the boostrapping on D_e
  D_e <- data.frame(t(apply(D_e[, c(paste('X_e', seq(1:nb_boostrap), sep=""))], 1,
                            FUN = function(x) {return(c(mean(x), quantile(x, c(0.25, 0.75))))} )))
  colnames(D_e) <- c('mean', 'q1', 'q3')
  D_e[, agg_level]<- pred_agg_region_ptime[, agg_level]
  
  # Calculate dr
  D_e_dr <- data.frame(pred_agg_region_ptime[, agg_level])
  colnames(D_e_dr) <- agg_level
  D_e_dr$mean <- D_e$mean *10000/ pred_agg_region_ptime[, p_time_var]
  D_e_dr$q1 <- D_e$q1 *10000/ pred_agg_region_ptime[, p_time_var]
  D_e_dr$q3 <- D_e$q3 *10000/ pred_agg_region_ptime[, p_time_var]
  
  D_e <- merge(D_e, D_e_dr, by=c(agg_level), all.x=TRUE, all.y=TRUE)
  colnames(D_e) <- c(agg_level, paste('toll_excess_period', annot, sep=''), paste('toll_excess_period', annot, '_low', sep=''),
                     paste('toll_excess_period', annot, '_up', sep=''), paste('dr_excess_period', annot, sep=''),
                     paste('dr_excess_period', annot, '_low', sep=''), paste('dr_excess_period', annot, '_up', sep='') )
  return(D_e)
}

# Upload Boostrapping overall and under 5
boostrapping_results <- rio::import("08_define_final_model/output/som_boostraping_data.rds")
boostrapping_results_u5 <- rio::import("08_define_final_model/output/som_boostraping_data_u5.rds")

# Compute the excess of death---------------------------------------------------
#First choose the date for period1 
start_date_period_1 <- "2015-01-01"
end_date_period_1 <- "2016-06-01"
start_date_period_2 <- "2022-01-01"
end_date_period_2 <- "2024-06-01"

# Second select the level of aggregation
agg_level <- 'date'

# Then compute the excess for overall and under 5
excess_res <- f_counterfactuals(boostrapping_results, resp_var = 'cdr', 
                                        admin2_col='district',
                                        agg_level = agg_level, 
                                        start_date_per_1 = start_date_period_1, 
                                        end_date_per_1 = end_date_period_1,
                                        start_date_per_2 = start_date_period_2, 
                                        end_date_per_2 = end_date_period_2, 
                                        nb_boostrap=1000, type_model = 'glm')

excess_res_u5 <- f_counterfactuals(boostrapping_results_u5, 
                                           resp_var = 'cdr_u5', 
                                           agg_level = agg_level,
                                           admin2_col='district',
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
clean_res_excess <- samrat::f_clean_counterfactuals_table(excess_res)

# save the clean data
utils::write.csv(clean_res_excess, 
                 paste("08_define_final_model/output/som_clean_counterfactuals_", 
                       lubridate::year(start_date_period_1), '_vs_', 
                       lubridate::year(start_date_period_2), ".csv", sep=""),
                 row.names=FALSE)

# Upload the actual deaths and select the date you want to study----------------
actual_res <- boostrapping_results |>
  dplyr::filter(date >= start_date_period_2 & date <= end_date_period_2)
actual_res_u5 <- boostrapping_results_u5 |>
  dplyr::filter(date >= start_date_period_2 & date <= end_date_period_2)

actual_res <- samrat::f_generate_final_results(actual_res, agg_level = c('region'), 
                                       nb_boostrap = 1000, resp_var = 'cdr')
actual_res_u5 <- samrat::f_generate_final_results(actual_res_u5, agg_level = c('region'), 
                                          nb_boostrap = 1000, resp_var = 'cdr_u5')

# merge overall and under 5
actual_res <- merge(actual_res, actual_res_u5, by=agg_level)

# clean the data into a nice format and save it
clean_res_actual <- samrat::f_clean_actual_table(actual_res)

# save the clean actual data
write.csv(clean_res_actual, paste("08_define_final_model/output/som_clean_actual_", 
                                  lubridate::year(start_date_period_2), '_', 
                                  lubridate::month(start_date_period_2), '_to_', 
                                  lubridate::year(end_date_period_2),'_', 
                                  lubridate::month(end_date_period_2), ".csv", sep=""), 
          row.names=FALSE)
