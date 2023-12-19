
# Train and Predict glm and return the result with boostrap
f_pred_glm <- function(training_data, predicting_data, pred_var, resp_var){
  
  ##Define the variables under study overall or under5
  if (resp_var %in% c("cdr", "n_died")) {resp_var <- "cdr"}
  if (resp_var %in% c("cdr_u5", "n_died_u5")) {resp_var <- "cdr_u5"}
  
  if(resp_var == 'cdr'){
    p_time_var <- 'p_time'
    pred_value <- 'n_died'
    pop_var <- 'pop_average'
    boost_pop_var <- 'pop'
    annot <- ''
  }else{
    p_time_var <- 'p_time_u5'
    pred_value <- 'n_died_u5'
    pop_var <- 'pop_average_u5'
    annot <- '_u5'
    boost_pop_var <- 'pop_u5_alt'
  }

  fit_formula <- as.formula(paste(pred_value," ~",
                                  paste(pred_var, collapse = " + "), '+','(1|district)', '+ offset(log(', p_time_var, '))'))
  glm_fit <- glmmTMB::glmmTMB(formula = fit_formula, data = training_data, weights = qualityScore,
                              family = "poisson")
  
  glm_fit |>
    saveRDS(paste('08_define_final_model/output/som_glm_model', annot, '.rds', sep=""))
  
  # Fit summary and save them
  summary <- parameters::model_parameters(glm_fit, ci_method = "wald", exponentiate = TRUE)
  x <- flextable::flextable(insight::format_table(summary))
  x <- flextable::autofit(x)
  flextable::save_as_docx(x, path = paste('08_define_final_model/output/som_out_glm_summary', annot, '.docx',sep=""))
  
  ##Plot the country level curve and save it
  predicting_data[, p_time_var] <- 1
  predicting_data[, c('qualityScore')] <- 1
  
  ##Boostraping part
  #Firt pred and standard error
  pred <- predict(glm_fit, predicting_data[, c(p_time_var,   pred_var, 'district', 'region', 'qualityScore')])
  se <- predict(glm_fit, predicting_data[, c(p_time_var,  pred_var, 'district',  'region', 'qualityScore')], se.fit=TRUE)$se.fit
  
  #Save pred and se
  predicting_data <- as.data.frame(predicting_data)
  predicting_data$pred <- pred
  predicting_data$se <- se
  predicting_data$date <- anytime::anydate(paste(predicting_data$year, predicting_data$month,sep="-")) # date format
  
  #Boostrap pred
  # Be careful of the order -- as we need to have the same between res_pop and predicting data
  predicting_data <- predicting_data[order(predicting_data$district), ]
  predicting_data <- predicting_data[order(predicting_data$date), ]
  
  #With CI population
  # bootstrap_pred <- suppressWarnings(
  #   replicate(1000,
  #             exp(
  #               {
  #                 rnorm(
  #                   nrow(predicting_data),
  #                   predicting_data$pred,
  #                   predicting_data$se
  #                 )
  #                 }
  #               ) * as.data.frame(do.call(rbind, lapply(res_pop,
  #                                         sample, 1, replace = TRUE)))$V1*
  #               lubridate::days_in_month(predicting_data$date)
  #             )
  #   )
  
  #Without CI population
  bootstrap_pred <- suppressWarnings(
    replicate(1000,
              exp(
                {
                  rnorm(
                    nrow(predicting_data),
                    predicting_data$pred,
                    predicting_data$se
                  )
                }
              ) *predicting_data[, c(pop_var)]*lubridate::days_in_month(predicting_data$date)
    )
  )
  
  bootstrap_pred <- t(apply(bootstrap_pred, 1, function(x) {if (all(is.na(x))) return(x) else return(sort(x)) }))
  
  predicting_data <- data.frame(predicting_data, bootstrap_pred) |> dplyr::select(-p_time_var)

  ##Return the data with boostrapping
  return(predicting_data)
}

# Generate the actual mean, low and upp dr and toll using a agg level
f_generate_final_results <- function(boostrapping_results, agg_level, nb_boostrap, resp_var){
  ##Define the variables under study overall or under5
  if (resp_var %in% c("cdr", "n_died")) {resp_var <- "cdr"}
  if (resp_var %in% c("cdr_u5", "n_died_u5")) {resp_var <- "cdr_u5"}
  
  if(resp_var == 'cdr'){
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
  
  #Compute ptime if wanted
  boostrapping_results[, p_time_var] <- boostrapping_results[, pop_var]*lubridate::days_in_month(boostrapping_results$date)
  
  # Aggregate the boostrapping results per agg level
  if(length(agg_level) == 1){
    D_a <- aggregate(boostrapping_results[, c(paste('X', seq(1:nb_boostrap), sep="")) ] ,
                     by = list(boostrapping_results[, agg_level]), FUN = sum )
  }else{
    D_a <- aggregate(boostrapping_results[, c(paste('X', seq(1:nb_boostrap), sep="")) ] ,
                     by = boostrapping_results[, agg_level], FUN = sum )
  }
  
  # Aggreagte the p_time results per agg level
  if(length(agg_level) == 1){
    pred_agg_region_ptime <- aggregate(boostrapping_results[, c(p_time_var)] ,
                                       by = list(boostrapping_results[, agg_level]), FUN = sum )
  }else{
    pred_agg_region_ptime <- aggregate(boostrapping_results[, c(p_time_var)] ,
                                       by = boostrapping_results[, agg_level], FUN = sum )
  }
  colnames(pred_agg_region_ptime) <- c(agg_level, p_time_var)
  
  ## Calculate mean, q1 and q3 of the boostrapping on the actual matrix results
  D_a <- data.frame(t(apply(D_a[, c(paste('X', seq(1:nb_boostrap), sep=""))], 1,
                            FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))} )))
  colnames(D_a) <-c('mean', 'q1', 'q3')
  D_a[, agg_level] <- pred_agg_region_ptime[, agg_level]
  
  # Calculate the dr rate
  D_a_dr <- data.frame(pred_agg_region_ptime[, agg_level])
  colnames(D_a_dr) <- agg_level
  D_a_dr$mean <- D_a$mean *10000/ pred_agg_region_ptime[, p_time_var]
  D_a_dr$q1 <- D_a$q1 *10000/ pred_agg_region_ptime[, p_time_var]
  D_a_dr$q3 <- D_a$q3 *10000/ pred_agg_region_ptime[, p_time_var]
  
  # Merge toll and dr
  D_a <- merge(D_a, D_a_dr, by=c(agg_level), all.x=TRUE, all.y=TRUE)
  colnames(D_a) <- c(agg_level, paste('toll_mean', annot, sep=""),
                     paste('toll_low', annot, sep=""),
                     paste('toll_up', annot, sep=""),
                     paste('dr_mean', annot, sep=""),
                     paste('dr_low', annot, sep=""),
                     paste('dr_up', annot, sep="")
                     )
  return(D_a)
}

f_counterfactuals <- function(boostrapping_results, resp_var, agg_level, start_date_per_1 = "2015-01-01", 
                              end_date_per_1 = "2016-06-01", start_date_per_2 = "2022-01-01", 
                              end_date_per_2 = "2022-12-01", nb_boostrap=1000, type_model = 'glm'){
  ##Define the variables under study overall or under5
  if (resp_var %in% c("cdr", "n_died")) {resp_var <- "cdr"}
  if (resp_var %in% c("cdr_u5", "n_died_u5")) {resp_var <- "cdr_u5"}
  
  if(resp_var == 'cdr'){
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
  
  ##First Calculate the median (counterfactual) of the non_crisis period (or period 1)
  
  median_c <- boostrapping_results |> dplyr::group_by(district) |> filter(date >= start_date_per_1 & date <= end_date_per_1) |>
    dplyr::mutate(median_c = mean(exp(pred))) |> dplyr::select(median_c, district) |> dplyr::distinct(district, .keep_all = TRUE) |> dplyr::ungroup()
  
  boostrapping_results <- merge(boostrapping_results, median_c, by=c('district'), all.x=TRUE)
  
  boostrapping_results$median_c <- boostrapping_results$median_c*boostrapping_results[, pop_var]*lubridate::days_in_month(boostrapping_results$date)
  
  ## Calculate the excess -- number of death crisis - number of death non crisis
  boostrapping_results <- subset(boostrapping_results, date >= start_date_per_2  & date <= end_date_per_2)
  boostrapping_results[, c(paste('X_e', seq(1:nb_boostrap), sep="")) ] <- boostrapping_results[, c(paste('X', seq(1:nb_boostrap), sep="")) ] - boostrapping_results$median_c
  
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

# Return clean counterfactuals
f_clean_counterfactuals_table <- function(excess_res){
  excess_est_full <- excess_res |>
    dplyr::bind_rows(
      excess_res |> dplyr::summarise(
        dplyr::across(
          starts_with('toll_'), sum,
          na.rm = T
        )
      )  |>
        dplyr::mutate(region = "Total")
    ) |>
    dplyr::mutate(across(starts_with('toll_'), round, -2)) |>
    dplyr::mutate(across(starts_with('dr_'), round, 3)) |>
    dplyr::mutate(across(where(is.numeric), format, big.mark = ",")) |>
    dplyr::summarise(
      # Estimates
      `Excess Toll (Overall)` = glue::glue("{toll_excess_period} ({toll_excess_period_low} to {toll_excess_period_up})"),
      `Excess Death Rate (Overall)` = glue::glue("{dr_excess_period} ({dr_excess_period_low} to {dr_excess_period_up})"),
      `Excess Toll (Under 5)` = glue::glue("{toll_excess_period_u5} ({toll_excess_period_u5_low} to {toll_excess_period_u5_up})"),
      `Excess Death Rate (Under 5)` = glue::glue("{dr_excess_period_u5} ({dr_excess_period_u5_low} to {dr_excess_period_u5_up})"),
      .by = "region"
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), ~ stringr::str_replace_all(., " ", "")
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), ~ stringr::str_replace_all(., "\\(", " (")
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), ~ stringr::str_replace_all(., "to", " to ")
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(Region = region)
  return(excess_est_full)
}

# Return clean actual results
f_clean_actual_table <- function(actual_res){
  actual_est_full <- actual_res |>
    dplyr::bind_rows(
      actual_res |> dplyr::summarise(
        dplyr::across(
          tidyselect::where(is.numeric), sum,
          na.rm = T
        )
      ) |>
        dplyr::mutate(region = "Total")
    ) |>
    dplyr::mutate(across(starts_with('toll_'), round, -2)) |>
    dplyr::mutate(across(starts_with('dr_'), round, 3)) |>
    dplyr::mutate(across(where(is.numeric), format, big.mark = ",")) |>
    dplyr::summarise(
      # Estimates
      `Actual Toll (Overall)` = glue::glue("{toll_mean} ({toll_low} to {toll_up})"),
      `Actual Death Rate (Overall)` = glue::glue("{dr_mean} ({dr_low} to {dr_up})"),
      `Actual Toll (Under 5)` = glue::glue("{toll_mean_u5} ({toll_low_u5} to {toll_up_u5})"),
      `Actual Death Rate (Under 5)` = glue::glue("{dr_mean_u5} ({dr_low_u5} to {dr_up_u5})"),
      .by = "region"
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), ~ stringr::str_replace_all(., " ", "")
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), ~ stringr::str_replace_all(., "\\(", " (")
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), ~ stringr::str_replace_all(., "to", " to ")
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(Region = region)
  return(actual_est_full)
}


##Function for boostrapping
f_plot_country_plot_with_boostrap <- function(boostrapping_results, name_output, resp_var='n_died', 
                                              start_date_cf_a = "2015-01-01", 
                                              end_date_cf_a = "2016-06-01", start_date_cf_b = "2020-01-01", 
                                              end_date_cf_b = "2021-06-01", cf_b_label = "2015-16 counterfactual", 
                                              cf_a_label = "2020 counterfactual"){
  
  palette_cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                  "#0072B2", "#D55E00", "#CC79A7")
  if (resp_var %in% c("cdr", "n_died")) {resp_var <- "cdr"}
  if (resp_var %in% c("cdr_u5", "n_died_u5")) {resp_var <- "cdr_u5"}
  
  dr_lab <- if_else(
    resp_var == "cdr", 
    "Estimated crude death rate (per 10,000 person-days) \n", 
    "Estimated under 5 years death rate (per 10,000 child-days) \n")
  annot <- if_else(
    resp_var == "cdr", 
    '', 
    '_u5')
  
  if (resp_var == "cdr") {
    y_label <- "Estimated crude death rate (per 10,000 person-days) \n"
  } else if (resp_var == "cdr_u5") {
    y_label <- "Estimated under 5 years death rate (per 10,000 child-days) \n"
  } else {  stop("Invalid type provided!")  }
  
  df_plot <- f_generate_final_results(boostrapping_results, agg_level = 'date', 
                                      nb_boostrap = 1000, resp_var = resp_var)
  
  countfact_mean_a <- df_plot |>
    filter(date > as.Date(start_date_cf_a) & date < as.Date(end_date_cf_a)) |>
    summarise(dr = mean(get(paste('dr_mean', annot, sep='')))) |>
    pull()
  
  countfact_mean_b <- df_plot |>
    filter(date > as.Date(start_date_cf_b) & date < as.Date(end_date_cf_b)) |>
    summarise(dr= mean(get(paste('dr_mean', annot, sep='')))) |>
    pull()
  
  plot <- df_plot |>
    ggplot(
      aes(x = date, y = df_plot[,paste('dr_mean', annot, sep='')], 
          ymin = df_plot[,paste('dr_low', annot, sep='')], 
          ymax = df_plot[,paste('dr_up', annot, sep='')])) +
    geom_ribbon(
      alpha = 0.2, fill = "grey25") +
    geom_point(
      alpha = 0.3, size = 3, colour = palette_cb[6]) +
    geom_smooth(
      aes(ymin = NULL, ymax = NULL),
      se = FALSE, span = 0.15, linewidth = 1, colour = palette_cb[7]) +
    # Thick line four counterfactual period a
    geom_segment(
      aes(x = as.Date(start_date_cf_b), xend = as.Date(end_date_cf_b), 
          y = countfact_mean_b, 
          yend = countfact_mean_b), 
      color = palette_cb[6], size = 0.7) +
    # Dotted line four counterfactual period a
    geom_segment(
      aes(x = as.Date(start_date_cf_b), xend = max(date), 
          y = countfact_mean_b, 
          yend = countfact_mean_b), 
      color = palette_cb[6], linetype = "dotted", size = 0.7) +
    geom_text(
      aes(x = max(date), y = countfact_mean_b, label = cf_a_label), 
      hjust = -0.075, vjust = 0.4, color = palette_cb[6]) +
    # Thick line four counterfactual period b
    geom_segment(
      aes(x = as.Date(start_date_cf_a), xend = as.Date(end_date_cf_a),
          y = countfact_mean_a, yend = countfact_mean_a),
      alpha = 1, color = palette_cb[4], size = 0.7) +
    # Dotted line four counterfactual period b
    geom_segment(
      aes(x = as.Date(start_date_cf_a), xend = max(date),
          y = countfact_mean_a,
          yend = countfact_mean_a),
      color = palette_cb[4], linetype = "dotted", size = 0.7) +
    # Title for counterfactual period a
    geom_text(
      aes(x = max(date), y = countfact_mean_a,
          label = cf_b_label),
      hjust = -0.073, vjust = 0.4, color = palette_cb[4]) +
    coord_cartesian(clip = "off") +
    theme_bw() +
    theme(plot.margin = margin(0.5, 5, 0.5, 0.5, "cm")) +
    scale_y_continuous(y_label) +
    scale_x_date("\n Year", date_breaks = "1 year", 
                 date_labels = "%Y", expand = expansion(add = 12))
  
  
  ggsave(paste('08_define_final_model/visualisation/output/som_', name_output, '.png', sep=""), dpi = "print", width = 25, height = 25, units = "cm")
}