##Source functions for RShiny App


palette_cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")


# Function to generate time units ---------------------------------------
f_generate_time_units <- function(y_analysis_start, y_analysis_end,
                                  m_analysis_end, m_analysis_start) {
  
  burn_out_period <- 0
  burn_in_period <- 1
  time_unit <- seq(1, ((y_analysis_end + burn_out_period - y_analysis_start + 
                          burn_in_period) * 12 + m_analysis_end - 
                         m_analysis_start + 1), 1)
  
  time_unit <- as.data.frame(time_unit)
  
  time_unit$year <- as.numeric(
    format(
      as.Date(seq(as.Date(paste(y_analysis_start - 1, 
                                "-", m_analysis_start, 
                                "-01", sep = "")),
                  as.Date(paste(y_analysis_end, "-", m_analysis_end, "-01", 
                                sep = "")),
                  by = "months"
      )), "%Y"))
  
  time_unit$month <- as.numeric(
    format(as.Date(seq(as.Date(paste(y_analysis_start - 1, "-", 
                                     m_analysis_start, "-01", sep = "")),
                       as.Date(paste(y_analysis_end, "-", 
                                     m_analysis_end, "-01", sep = "")),
                       by = "months"
    )), "%m"))
  return(time_unit)
}

# Quantile interpolation function
# Goal is to calculate a movin median or quantile per district
f_quantile_interp <- function(key_vars, neg_vars, ts_expand, tm_span, proba = 0.5, lenght_forecast = 6){
  res_cases <- c()
  # For reach variable to caculate the forecasting
  for(var in key_vars){
    if(var %in% neg_vars){
      if(proba == 0.025){
        proba <- 0.95
      }else if(proba == 0.95){
        proba <- 0.025
      }
    }
    # If cdi -- lag 5 we can use the data from the last 5 months
    res_forecast <- c()
    if(var == 'cdi_lag5_scn'){
      data_forecast <- ts_expand |> dplyr::select(district, time_unit, var) |> 
        dplyr::filter(time_unit <= tm_span[length(tm_span)]-lenght_forecast)
      data_cdi <- ts_expand |> dplyr::select(district, time_unit, 'cdi_scn') |> 
        dplyr::filter(time_unit %in% (tm_span-lenght_forecast)) |>
        dplyr::rename(cdi_lag5_scn = cdi_scn)
      data_cdi$time_unit <- data_cdi$time_unit +lenght_forecast
      res <- data_cdi |> dplyr::filter(time_unit > tm_span[length(tm_span)]-lenght_forecast)
    }else if(var %in% c('pop_average', 'pop_average_u5')){
      data_forecast <- ts_expand |> dplyr::select(district, time_unit, var)|> 
        dplyr::filter(time_unit <= tm_span[length(tm_span)]-lenght_forecast)
      res <- c() 
      res_dist <- c()
      res_time <- c()
      for(dist in unique(data_forecast$district)){
        data <- data_forecast |> dplyr::filter(district == dist)
        res <- append(res, signal::interp1(c(data$time_unit)[((length(data$time_unit)-6):length(data$time_unit))], 
                                           array(data[, c(var)])[((length(data$time_unit)-6):length(data$time_unit))], tm_span, extrap=TRUE))
        res_dist <- append(res_dist, rep(dist, lenght_forecast))
        res_time <- append(res_time, tm_span)
      }
      res <- data.frame(res, res_dist, res_time)
      colnames(res) <- c(var, 'district', 'time_unit')
    }else{ # For all the other variable
      data_forecast <- ts_expand |> dplyr::select(district, time_unit, var)|> 
        dplyr::filter(time_unit <= tm_span[length(tm_span)]-lenght_forecast)
      res <- c()
      for(i in 1:(lenght_forecast)){
        res_ <- subset(data_forecast, data_forecast$time_unit %in% (tm_span[length(tm_span)]-2*lenght_forecast+i-1):(tm_span[length(tm_span)]-lenght_forecast+i-1))  %>%
          group_by(district) %>%
          summarise_at(vars(var), list(var = ~quantile(., probs = proba)))
        colnames(res_) <- c('district', var)
        res_$time_unit <- tm_span[length(tm_span)] -lenght_forecast + i
        res <- rbind(res, res_)
      }
    }
    if(length(res_cases) == 0){
      res_cases <- res
    }else{
      res_cases <- merge(res_cases, res, by=c('district', 'time_unit'), all.x=TRUE)
    }
  }
  return(res_cases)
}

# Function to categorise variables in data -------------------------------------

f_categorise <- function(data, breaks_and_labels) {
  
  # Function to apply the transformation
  cut_variable <- function(x, breaks, labels) {
    cut(x, breaks = breaks, labels = labels, right = FALSE, 
        include.lowest = TRUE)
  }
  
  # Function to select columns based on prefix and exclusion of "_scn"
  select_columns <- function(data, prefix) {
    col_names <- names(data)
    selected_cols <- col_names[startsWith(col_names, prefix) & 
                                 !stringr::str_detect(col_names, "_scn")]
    selected_cols
  }
  
  # Iterate over breaks_and_labels to apply the transformation
  data_transformed <- data
  
  for (prefix in names(breaks_and_labels)) {
    selected_columns <- select_columns(data_transformed, prefix)
    data_transformed <- data_transformed |>
      dplyr::mutate(dplyr::across(
        .cols = all_of(selected_columns),
        .fns = ~ cut_variable(.x, 
                              breaks = breaks_and_labels[[prefix]]$breaks, 
                              labels = breaks_and_labels[[prefix]]$labels),
        .names = "{.col}_cat"
      ))
  }
  
  # Check for missing values in newly created variables
  new_vars <- grep("_cat$", names(data_transformed), value = TRUE)
  if (any(sapply(data_transformed[new_vars], function(x) any(is.na(x))))) {
    message("Warning: Some variables have not been properly categorized and contain missing values.")
  }
  
  return(data_transformed)
}


# Function to generate simple forecasting -- static, pessimistic and optimistic
f_forecast_using_simple_model <- function(preds_df, forecast_length, time_unit_data, analysis_strata){
  # First select the variables
  key_vars <- c('cdi_lag5_scn', 'measles_cases_rate', 'sam_admissions_rate_lag1_scn', 
                'malaria_cases_rate', 'acled_event_rate', 'water_price_smooth_lag2_scn',
                'tot_wage_cereal_smooth_lag3_scn', 'dep_rate_sqt_scn', 'prop_idp_scn',
                'cdi_scn', 'pop_average', 'pop_average_u5')
  # For quantile -- Negative variable
  neg_vars <- c('cdi_lag5_scn', 'tot_wage_cereal_smooth_lag3_scn')
  
  # Complete the time series with the forecasting time we needed
  tm_span <- c((max(preds_df$time_unit)+1): (max(preds_df$time_unit)+forecast_length))
  
  # Rolling Mean to complete the data
  ## For static
  middle_cases <- f_quantile_interp(key_vars=key_vars, neg_vars = neg_vars, 
                                  ts_expand=preds_df, 
                                  tm_span=tm_span, proba = 0.5, 
                                  lenght_forecast = forecast_length)
  middle_cases <- rbind(preds_df[, c(key_vars, "time_unit", 'district')], middle_cases) #merge to have all the data
  middle_cases <- merge(middle_cases, time_unit_data, by='time_unit', all.x=TRUE) #Add date, year, month
  middle_cases <- merge(middle_cases, analysis_strata[, c('district', 'region')], by=c('district'), all.x=TRUE) #Add region
  middle_cases$date <- as.Date(paste0(middle_cases$year, "-", middle_cases$month, "-01")) #Complete date
  
  ## For pessimistic
  low_quant_cases <- f_quantile_interp(key_vars=key_vars, neg_vars = neg_vars, 
                                     ts_expand=preds_df, 
                                     tm_span=tm_span, proba = 0.025,
                                     lenght_forecast = forecast_length)
  low_quant_cases <- rbind(preds_df[, c(key_vars, "time_unit", 'district')], low_quant_cases)
  low_quant_cases <- merge(low_quant_cases, time_unit_data, by='time_unit', all.x=TRUE)
  low_quant_cases <- merge(low_quant_cases, analysis_strata[, c('district', 'region')], by=c('district'), all.x=TRUE)
  low_quant_cases$date <- as.Date(paste0(low_quant_cases$year, "-", low_quant_cases$month, "-01"))
  
  ## For optimistic
  high_quant_cases <- f_quantile_interp(key_vars=key_vars, neg_vars = neg_vars, 
                                      ts_expand=preds_df, 
                                      tm_span=tm_span, proba = 0.95,
                                      lenght_forecast = 5)
  high_quant_cases <- rbind(preds_df[, c(key_vars, "time_unit", 'district')], high_quant_cases)
  high_quant_cases <- merge(high_quant_cases, time_unit_data, by='time_unit', all.x=TRUE)
  high_quant_cases <- merge(high_quant_cases, analysis_strata[, c('district', 'region')], by=c('district'), all.x=TRUE)
  high_quant_cases$date <- as.Date(paste0(high_quant_cases$year, "-", high_quant_cases$month, "-01"))
  
  # Define breaks and labels for different groups for categorisation
  breaks_and_labels <- list(
    malaria_cases_rate = list(
      breaks = c(0, 0.0000001, 50, 10000),
      labels = c("0", "1 to 49", ">= 50")
    ),
    measles_cases_rate = list(
      breaks = c(0, 0.0000001, 100000),
      labels = c("0", ">= 1")
    )
  )
  
  middle_cases <- f_categorise(middle_cases, breaks_and_labels)
  low_quant_cases <- f_categorise(low_quant_cases, breaks_and_labels)
  high_quant_cases <- f_categorise(high_quant_cases, breaks_and_labels)
  return(list(middle_cases, low_quant_cases, high_quant_cases))
}

# Function to predict data using a glmm
f_pred_glm <- function(fit_glm, predicting_data,  pred_var, resp_var, name_output, type_model='glm'){
  if(resp_var == 'n_died' | resp_var == 'cdr'){
    p_time_var <- 'p_time'
    pred_value <- 'n_died'
    pop_var <- 'pop_average'
    annot <- ''
  }else{
    p_time_var <- 'p_time_u5'
    pred_value <- 'n_died_u5'
    pop_var <- 'pop_average_u5'
    annot <- '_u5'
  }
  ##Plot the country level curve and save it
  predicting_data[, p_time_var] <- 1
  predicting_data[, 'qualityScore'] <- 1
  
  pred <- predict(fit_glm, predicting_data, type='response')
  
  predicting_data <- as.data.frame(predicting_data)
  predicting_data$pred <- pred
  #Return the data with boostrapping
  return(predicting_data)
}

# Function to predict and generate the correct format of data
f_pred_forecast <- function(data, glmm_model, name_output='static_scen', 
                            level_plot = 'national', district_ = 'Baki'){
  # Extract variables from the formula
  pred_var <- all.vars(glmm_model$call$formula[-2])[1:(length(all.vars(glmm_model$call$formula[-2]))-2)]
  resp_var <- all.vars(glmm_model$call$formula)[1]
  
  if(resp_var == 'n_died' | resp_var == 'cdr'){
    p_time_var <- 'p_time'
    pred_value <- 'n_died'
    pop_var <- 'pop_average'
    annot <- ''
  }else{
    p_time_var <- 'p_time_u5'
    pred_value <- 'n_died_u5'
    pop_var <- 'pop_average_u5'
    annot <- '_u5'
  }
  
  # Prediction using the model
  data_pred <- f_pred_glm(fit_glm = glmm_model,predicting_data = data,
                           pred_var = pred_var, 
                           name_output = name_output,resp_var = resp_var)
  
  # Compute predicted death toll and person-time
  data_pred$pred <- data_pred$pred * data_pred[, c(pop_var)] *
    lubridate::days_in_month(data_pred$month)
  
  data_pred[, c(p_time_var)] <- data_pred[, c(pop_var)] * days_in_month(data_pred$month)
  
  # Aggregate to country-month
  if(level_plot == 'national'){
    data_pred <- aggregate(data_pred[, c("pred", p_time_var)],
                           by = list(date = data_pred$date), FUN = sum)
  }else{
    data_pred <- data_pred |> dplyr::filter(district == district_)
    data_pred <- aggregate(data_pred[, c("pred", p_time_var)],
                           by = list(date = data_pred$date), FUN = sum)
  }
  
  
  # Compute death rate and add dates
  data_pred$dr <- data_pred$pred * 10000 / data_pred[, c(p_time_var)]
  return(data_pred)
}

# Plot curve using static, pessimistic and optimistic data
f_plot_forecasting <- function(stat_data, pess_data, opt_data, 
                               stat_data_u5, pess_data_u5, opt_data_u5,
                               DATE_FORECAST,
                               start_period="2021-06-10", type_plot='overall'){
  if(type_plot == 'overall'){
    ts_all <- list(pess_data, stat_data, opt_data)
    ts_all[[1]]$scen <- 'pess_data'
    ts_all[[2]]$scen <- 'stat_data'
    ts_all[[3]]$scen <- 'opt_data'
    ts_comb <- do.call(rbind, ts_all) %>% mutate(date = as.Date(date))
    
    cdr <- ts_comb %>%
      dplyr::filter(date > as.Date(start_period)) %>%
      dplyr::filter(date < as.Date(max(stat_data$date))) %>%
      ggplot(aes(date, dr, color = scen)) +
      geomtextpath::geom_textvline(label = "Start of Forecasting", xintercept = as.Date(DATE_FORECAST), linetype = "dashed", hjust = 0.05,
                                   lwd = 1) +
      geom_point() +
      geom_smooth(se = FALSE, span = 0.9, data = . %>% dplyr::filter(date >= as.Date(DATE_FORECAST))) +
      geom_smooth(data = . %>% dplyr::filter(date <= as.Date(DATE_FORECAST)), color = "darkred", span = 0.2, se = FALSE) +
      geom_point(data = . %>% dplyr::filter(date <= as.Date(DATE_FORECAST)), color = "black") +
      theme_bw(base_size = 14) +
      scale_color_viridis_d(end = 0.8, name = "Forecast:   ", labels = c("Optimistic  ", "Pessimistic  ", "Average")) +
      scale_x_date(date_labels = "%m-%Y", breaks = seq.Date(as.Date(start_period), as.Date(max(stat_data$date)), by = "6 months")) +
      ylim(c(min(opt_data$dr), max(pess_data$dr)))+
      ylab(" \n Median crude death rate\n(per 10,000 person-days)") +
      xlab(" \nDate")
    
  }else{
    ts_all_u5 <- list(pess_data_u5, stat_data_u5, opt_data_u5)
    ts_all_u5[[1]]$scen <- 'pess_data'
    ts_all_u5[[2]]$scen <- 'stat_data'
    ts_all_u5[[3]]$scen <- 'opt_data'
    ts_comb_u5 <- do.call(rbind, ts_all_u5) %>% mutate(date = as.Date(date))
    
    cdr <- ts_comb_u5 %>%
      dplyr::filter(date > as.Date(start_period)) %>%
      dplyr::filter(date < as.Date(max(stat_data$date))) %>%
      ggplot(aes(date, dr, color = scen)) +
      geomtextpath::geom_textvline(label = "Start of Forecasting", xintercept = as.Date(DATE_FORECAST), linetype = "dashed", hjust = 0.05, lwd = 1) +
      geom_point() +
      geom_smooth(se = FALSE, data = . %>% dplyr::filter(date >= as.Date(DATE_FORECAST))) +
      geom_smooth(data = . %>% dplyr::filter(date <= as.Date(DATE_FORECAST)), color = "darkred", span = 0.2, se = FALSE) +
      geom_point(data = . %>% dplyr::filter(date <= as.Date(DATE_FORECAST)), color = "black") +
      theme_bw(base_size = 14) +
      scale_color_viridis_d(end = 0.8, name = "Forecast Scenario:   ", labels = c("Optimistic  ", "Pessimistic  ", "Average")) +
      scale_x_date(date_labels = "%m-%Y", breaks = seq.Date(as.Date(start_period), as.Date(max(stat_data$date)), by = "6 months")) +
      ylim(c(min(opt_data_u5$dr), max(pess_data_u5$dr)))+
      ylab(" \n Median under 5s death rate \n(per 10,000 person-days)") +
      xlab(" \nDate")
  }
  

  return(cdr)
}

## FOR CORRELATION FORECASTING
f_return_data_corr_pred <- function(MODIFIED_FEATURES, PERCENT_MODIFICATIONS, 
                        static_data, pessimistic_data, optimistic_data, DATE_FORECAST,
                        preds_df, glm_model, glm_model_u5, 
                        level_plot = 'national', district_ = 'Baki'){
  
  ## PERCENTAGE OF MODIFICATION OF THE different FEATURES upload in the App
  coeff_data <- data.frame(MODIFIED_FEATURES, PERCENT_MODIFICATIONS)
  
  #Estimates Correlation Matrix
  corr_variable_measles <- c('water_price_smooth_lag2_scn', 'tot_wage_cereal_smooth_lag3_scn', 
                             'dep_rate_sqt_scn', 'prop_idp_scn','acled_event_rate',
                             'sam_admissions_rate_lag1_scn', 'measles_cases_rate')
  correlation_measles <- as.data.frame(cor(preds_df |> dplyr::select(corr_variable_measles)))
  
  corr_variable_malaria <- c('water_price_smooth_lag2_scn',
                             'dep_rate_sqt_scn', 'prop_idp_scn','acled_event_rate', 'malaria_cases_rate')
  correlation_malaria <- as.data.frame(cor(preds_df |> dplyr::select(corr_variable_malaria)))
  
  corr_variable_sam <- c('water_price_smooth_lag2_scn', 'tot_wage_cereal_smooth_lag3_scn', 'acled_event_rate',
                         'dep_rate_sqt_scn', 'prop_idp_scn', 'sam_admissions_rate_lag1_scn',
                         'malaria_cases_rate', 'measles_cases_rate')
  correlation_sam <- as.data.frame(cor(preds_df |> dplyr::select(corr_variable_sam)))
  
  #Cases no DIRECT MODIFICATIN OF MEASLES
  if(coeff_data[coeff_data$MODIFIED_FEATURES == 'measles_cases_rate',]$PERCENT_MODIFICATIONS == 0){
    corr_coeff <- data.frame(t(correlation_measles['measles_cases_rate', corr_variable_measles]))
    corr_coeff$MODIFIED_FEATURES  <- c(rownames(corr_coeff))
    coeff_data_measles <- coeff_data |> dplyr::filter(MODIFIED_FEATURES %in% corr_variable_measles)
    coeff_ <- merge(corr_coeff, coeff_data_measles, by='MODIFIED_FEATURES')
    coeff_$prod <- coeff_$measles_cases_rate * coeff_$PERCENT_MODIFICATIONS

    ## Calculate the new forecasting values
    static_data_for <- static_data |> dplyr::filter(date > DATE_FORECAST) |> dplyr::mutate(
      measles_cases_rate = (prod(1+coeff_$prod*(time_unit - min(time_unit)+1)/5))*measles_cases_rate)
    pessim_data_for <- pessimistic_data |> dplyr::filter(date > DATE_FORECAST) |> dplyr::mutate(
      measles_cases_rate = (prod(1+coeff_$prod*(time_unit - min(time_unit)+1)/5))*measles_cases_rate)
    
    optim_data_for <- optimistic_data |> dplyr::filter(date > DATE_FORECAST) |> dplyr::mutate(
      measles_cases_rate = (prod(1+coeff_$prod*(time_unit - min(time_unit)+1)/5))*measles_cases_rate)
  }else{
    static_data_for <- static_data |> dplyr::filter(date > DATE_FORECAST) |> 
      dplyr::mutate(measles_cases_rate = (1+coeff_data[coeff_data$MODIFIED_FEATURES == 'measles_cases_rate',]$PERCENT_MODIFICATIONS*(time_unit - min(time_unit)+1)/5)*measles_cases_rate)
  
    pessim_data_for <- pessimistic_data |> dplyr::filter(date > DATE_FORECAST) |> 
      dplyr::mutate(measles_cases_rate = (1+coeff_data[coeff_data$MODIFIED_FEATURES == 'measles_cases_rate',]$PERCENT_MODIFICATIONS*(time_unit - min(time_unit)+1)/5)*measles_cases_rate)
    
    optim_data_for <- optimistic_data |> dplyr::filter(date > DATE_FORECAST) |> 
      dplyr::mutate(measles_cases_rate = (1+coeff_data[coeff_data$MODIFIED_FEATURES == 'measles_cases_rate',]$PERCENT_MODIFICATIONS*(time_unit - min(time_unit)+1)/5)*measles_cases_rate)
  }
  
  if(coeff_data[coeff_data$MODIFIED_FEATURES == 'malaria_cases_rate',]$PERCENT_MODIFICATIONS == 0){
    corr_coeff <- data.frame(t(correlation_malaria['malaria_cases_rate', corr_variable_malaria]))
    corr_coeff$MODIFIED_FEATURES  <- c(rownames(corr_coeff))
    coeff_data_malaria <- coeff_data |> dplyr::filter(MODIFIED_FEATURES %in% corr_variable_malaria)
    coeff_ <- merge(corr_coeff, coeff_data_malaria, by='MODIFIED_FEATURES')
    coeff_$prod <- coeff_$malaria_cases_rate * coeff_$PERCENT_MODIFICATIONS
    
    ## Calculate the new forecasting values
    static_data_for <- static_data_for |> dplyr::filter(date > DATE_FORECAST) |> dplyr::mutate(
      malaria_cases_rate = (prod(1+coeff_$prod*(time_unit - min(time_unit)+1)/5))*malaria_cases_rate)
    pessim_data_for <- pessim_data_for |> dplyr::filter(date > DATE_FORECAST) |> dplyr::mutate(
      malaria_cases_rate = (prod(1+coeff_$prod*(time_unit - min(time_unit)+1)/5))*malaria_cases_rate)
    optim_data_for <- optim_data_for |> dplyr::filter(date > DATE_FORECAST) |> dplyr::mutate(
      malaria_cases_rate = (prod(1+coeff_$prod*(time_unit - min(time_unit)+1)/5))*malaria_cases_rate)
  }else{
    static_data_for <- static_data_for |> dplyr::filter(date > DATE_FORECAST) |> 
      dplyr::mutate(malaria_cases_rate = (1+coeff_data[coeff_data$MODIFIED_FEATURES == 'malaria_cases_rate',]$PERCENT_MODIFICATIONS*(time_unit - min(time_unit)+1)/5)*malaria_cases_rate)
    pessim_data_for <- pessim_data_for |> dplyr::filter(date > DATE_FORECAST) |> 
      dplyr::mutate(malaria_cases_rate = (1+coeff_data[coeff_data$MODIFIED_FEATURES == 'malaria_cases_rate',]$PERCENT_MODIFICATIONS*(time_unit - min(time_unit)+1)/5)*malaria_cases_rate)
    optim_data_for <- optim_data_for |> dplyr::filter(date > DATE_FORECAST) |> 
      dplyr::mutate(malaria_cases_rate = (1+coeff_data[coeff_data$MODIFIED_FEATURES == 'malaria_cases_rate',]$PERCENT_MODIFICATIONS*(time_unit - min(time_unit)+1)/5)*malaria_cases_rate)
  }
  
  if(coeff_data[coeff_data$MODIFIED_FEATURES == 'sam_admissions_rate_lag1_scn',]$PERCENT_MODIFICATIONS == 0){
    corr_coeff <- data.frame(t(correlation_sam['sam_admissions_rate_lag1_scn', corr_variable_sam]))
    corr_coeff$MODIFIED_FEATURES  <- c(rownames(corr_coeff))
    coeff_data_sam <- coeff_data |> dplyr::filter(MODIFIED_FEATURES %in% corr_variable_sam)
    coeff_ <- merge(corr_coeff, coeff_data_sam, by='MODIFIED_FEATURES')
    coeff_$prod <- coeff_$sam_admissions_rate_lag1_scn * coeff_$PERCENT_MODIFICATIONS
    
    ## Calculate the new forecasting values
    static_data_for <- static_data_for |> dplyr::filter(date > DATE_FORECAST) |> dplyr::mutate(
      sam_admissions_rate_lag1_scn = (prod(1+coeff_$prod*(time_unit - min(time_unit)+1)/5))*sam_admissions_rate_lag1_scn)
    pessim_data_for <- pessim_data_for |> dplyr::filter(date > DATE_FORECAST) |> dplyr::mutate(
      sam_admissions_rate_lag1_scn = (prod(1+coeff_$prod*(time_unit - min(time_unit)+1)/5))*sam_admissions_rate_lag1_scn)
    
    optim_data_for <- optim_data_for |> dplyr::filter(date > DATE_FORECAST) |> dplyr::mutate(
      sam_admissions_rate_lag1_scn = (prod(1+coeff_$prod*(time_unit - min(time_unit)+1)/5))*sam_admissions_rate_lag1_scn)
  }else{
    static_data_for <- static_data_for |> dplyr::filter(date > DATE_FORECAST) |> 
      dplyr::mutate(sam_admissions_rate_lag1_scn = (1+coeff_data[coeff_data$MODIFIED_FEATURES == 'sam_admissions_rate_lag1_scn',]$PERCENT_MODIFICATIONS*(time_unit - min(time_unit)+1)/5)*sam_admissions_rate_lag1_scn)
    pessim_data_for <- pessim_data_for |> dplyr::filter(date > DATE_FORECAST) |> 
      dplyr::mutate(sam_admissions_rate_lag1_scn = (1+coeff_data[coeff_data$MODIFIED_FEATURES == 'sam_admissions_rate_lag1_scn',]$PERCENT_MODIFICATIONS*(time_unit - min(time_unit)+1)/5)*sam_admissions_rate_lag1_scn)
    optim_data_for <- optim_data_for |> dplyr::filter(date > DATE_FORECAST) |> 
      dplyr::mutate(sam_admissions_rate_lag1_scn = (1+coeff_data[coeff_data$MODIFIED_FEATURES == 'sam_admissions_rate_lag1_scn',]$PERCENT_MODIFICATIONS*(time_unit - min(time_unit)+1)/5)*sam_admissions_rate_lag1_scn)
  }

  ## Concate the previous results with our new data
  static_data_corr <- rbind(static_data |> dplyr::filter(date <= DATE_FORECAST), static_data_for) |> dplyr::select(-c(malaria_cases_rate_cat, measles_cases_rate_cat))
  pessimistic_data_corr <- rbind(static_data |> dplyr::filter(date <= DATE_FORECAST), pessim_data_for) |> dplyr::select(-c(malaria_cases_rate_cat, measles_cases_rate_cat))
  optimistic_data_corr <- rbind(static_data |> dplyr::filter(date <= DATE_FORECAST), optim_data_for) |> dplyr::select(-c(malaria_cases_rate_cat, measles_cases_rate_cat))

  
  ## Categorise data
  # Define breaks and labels for different groups for categorisation
  breaks_and_labels <- list(
    malaria_cases_rate = list(
      breaks = c(-Inf, 0.0000001, 50, 10000),
      labels = c("0", "1 to 49", ">= 50")
    ),
    measles_cases_rate = list(
      breaks = c(-Inf, 0.0000001, 100000),
      labels = c("0", ">= 1")
    )
  )
  static_data_corr <- f_categorise(static_data_corr, breaks_and_labels)
  pessimistic_data_corr <- f_categorise(pessimistic_data_corr, breaks_and_labels)
  optimistic_data_corr <- f_categorise(optimistic_data_corr, breaks_and_labels)
  
  # First for Overall
  static_data_pred <- f_pred_forecast(static_data_corr, glm_model, name_output='static_scen', 
                                      level_plot, district_)
  pessimistic_data_pred <- f_pred_forecast(pessimistic_data_corr, glm_model, name_output='pessimistic_scen', 
                                           level_plot, district_)
  optimistic_data_pred <- f_pred_forecast(optimistic_data_corr, glm_model, name_output='optmistic_scen', 
                                          level_plot, district_)
  
  #Same for Under5
  static_data_pred_u5 <- f_pred_forecast(static_data_corr, glm_model_u5, name_output='static_scen', 
                                         level_plot, district_)
  pessimistic_data_pred_u5 <- f_pred_forecast(pessimistic_data_corr, glm_model_u5, name_output='pessimistic_scen', 
                                              level_plot, district_)
  optimistic_data_pred_u5 <- f_pred_forecast(optimistic_data_corr, glm_model_u5, name_output='optmistic_scen', 
                                             level_plot, district_)
  
  return(list(static_data_pred, pessimistic_data_pred, optimistic_data_pred, 
              static_data_pred_u5, pessimistic_data_pred_u5, optimistic_data_pred_u5))
}

## For RShiny App only access to the data
dateInput2 <- function(inputId, label, minview = "days", maxview = "decades", ...) {
  d <- shiny::dateInput(inputId, label, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}



