# pacman::p_load(
#   boot,        # Too compute model diagnostics
#   broom,       # For wrangling tables of models
#   DHARMa,      # To generate residuals and diagnostics for mixed models
#   flextable,   # To write tables in .docx format
#   gamlss,      # For fitting generalised additive models
#   ggalt,       # For fancy dumbbell plots
#   ggcorrplot,  # For correlation matrix plotting
#   ggplot2,     # Data visualization
#   ggpubr,      # Arranging multiple plots into a single plot
#   glmmTMB,     # For fitting generalised linear mixed models
#   INLA,        # For fitting mixed models of different kinds (not from CRAN)
#   insight,     # For formatting model summary tables
#   lme4,        # Alternative to glmmTMB for generalised linear mixed models
#   lubridate,   # Makes it easier to work with dates and times
#   MASS,        # For various statistical functions
#   mast,        # Matching administrative names and geospatial operations
#   mgcv,        # For fitting GAM(M) models
#   multiwayvcov,# To compute clustered variance-covariance matrices     
#   parameters,  # Visualise model output
#   pscl,        # Fit zero-inflated GLMs
#   ranger,      # For growing random forest models
#   reshape2,    # For converting between wide and long data structure
#   Rgraphviz,   # For plotting DAGs - need 'BiocManager::install("Rgraphviz")'
#   scales,      # Scaling and formatting data for visualizations
#   tidyverse,   # Tidyverse suite of packages
#   zoo, 
#   rio)         # For computing running means

library(tidyr)
library(rio)
library(ggplot2)
library(gamlss)

colours_f <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")

#source("07_model_exploration/code/00_functions.R")

f_cv_francesco <- function(data_f = obs_fit, which_fold_f = "fold3",
                           k_folds_f = 10, fit_f = fit, f_family_f = f_family, 
                           f_predict_f = f_predict, verbose = TRUE) {
  
  # List of k folds
  folds_k <- unique(data_f$fold_k)
  
  #...................................
  ## More preparations
  
  # Prepare output
  # output 1 - by fold, to compute CV metrics
  out1 <- data.frame(fold_k = folds_k, obs = NA, pred = NA, 
                     mse = NA, rmse = NA, bias_abs = NA, bias_rel = NA)
  
  # output 2 - by district-year, for plotting
  out2 <- c()
  
  # Identify response variable
  resp_var <- as.character(formula(fit_f))[2]
  
  #...................................
  ## For classes "glm", "glm.nb", "zeroinfl", "glmmTMB", "gam", "bam"
  if (any(class(fit_f) %in% c("glm", "glm.nb", "zeroinfl", "glmmTMB",
                              "gam", "bam") ) ) {
    
    # For each fold...
    for (i in 1:nrow(out1) ) {
      
      # progress
      if (verbose == TRUE) {print(paste("now working on fold ", i, " of ", 
                                        nrow(out1), sep = ""))}
      
      # select training and holdout data
      df_train <- subset(data_f, fold_k != out1[i, "fold_k"])
      df_hold <- subset(data_f, fold_k == out1[i, "fold_k"])
      
      # fit on all data but the fold
      if (any(class(fit_f) %in% c("glm", "glm.nb", "zeroinfl", "glmmTMB") ) ) {
        cv_fit <- update(fit_f, data = df_train)
      }
      if (class(fit_f)[1] == "gam" ) {
        # figure out the weights variable(s)
        x <- names(getCall(fit_f))
        x <- as.character(getCall(fit_f))[which(x == "weights")]
        # re-fit
        cv_fit <- tryCatch(gam(formula(fit_f), data = df_train, 
                               family = family(fit_f)$family, weights = eval(as.name(x)) ) )
      }
      if (class(fit_f)[1] == "bam" ) {
        # figure out the weights variable(s)
        x <- names(getCall(fit_f))
        x <- as.character(getCall(fit_f))[which(x == "weights")]
        # re-fit
        cv_fit <- tryCatch(bam(formula(fit_f), data = df_train, 
                               family = family(fit_f)$family, weights = eval(as.name(x)) ) )
      }
      ###FC: mgcv throws error on fitting the first fold
      # (there is *no* information about some basis coefficients)
      # for now have fixed with an error catcher
      
      if(inherits(cv_fit, "try-error")) {
        message(
          "ERROR: mgcv not fitting for this fold - execution continues");
        next}
      
      # predict on holdout fold
      df_hold$pred <- f_predict_f(fit_f = cv_fit, newdata_f = df_hold)
      
      # collect predictive metrics
      out1[i, "obs"] <- sum(df_hold[, resp_var])
      out1[i, "pred"] <- sum(df_hold$pred)
      out1[i, "mse"] <- mean((df_hold$pred - df_hold[, resp_var])^2)
      out1[i, "rmse"] <- sqrt(out1[i, "mse"])
      
      # track results by region-year
      x <- aggregate(df_hold[, c(resp_var, "pred")], 
                     by = df_hold[, c("region", "year")], FUN = sum)
      x <- x[order(x$region, x$year), ]
      out2 <- rbind(out2, x)
    }
  }
  
  #...................................
  ## Return output
  
  # Compute additional metrics
  out1$bias_abs <- out1$pred - out1$obs
  out1$bias_rel <- out1$bias_abs / out1$obs
  out1 <- out1
  
  # Aggregate summary metrics across all folds
  out3 <- c(colMeans(out1[, c("mse", "rmse", "bias_rel")]), sd(out1$rmse) )
  names(out3) <-c ("mse", "rmse", "bias_rel", "sd_rmse")
  
  # Final aggregation of output 2 by region-year
  out2 <- aggregate(out2[, c(resp_var, "pred")], 
                    by = out2[, c("region", "year")], FUN = sum)
  colnames(out2)[colnames(out2) == resp_var] <- "obs"
  
  # Return
  out <- list(out1, out2, out3, class(fit_f)[1], f_family_f(fit_f))
  names(out) <- c("cv_by_fold", "cv_by_region_year", "cv_metrics", "class",
                  "family")
  return(out)
}

f_predict <- function(fit_f = fit,
                      newdata_f = df_pred, type_f = "response",
                      resp_var = NULL,
                      vcov_rob_f = NULL, bootstrap_f = FALSE, 
                      n_bootstraps = NULL) {
  
  # set seed
  set.seed(768)
  
  # Prediction for 'bn.fit' class
  if (class(fit_f)[1] == 'bn.fit') {
    newdata_f$pred <- predict(fit_f, node = resp_var,
                              data = newdata_f[, c(preds)],
                              method = 'bayes-lw', n = 50000)
    return(newdata_f$pred)
  }
  
  # Prediction for other classes
  if (any(class(fit_f) %in% c("glm", "glm.nb", "zeroinfl", 
                              "glmmTMB", "gam", "bam"))) {
    
    # If desire point estimate
    if (!bootstrap_f) {
      return(predict(fit_f, newdata = newdata_f, type = type_f, 
                     allow.new.levels = TRUE))
    }
    
    # If desire bootstrap random prediction
    est <- predict(fit_f, newdata = newdata_f, type = "link", 
                   se.fit = TRUE, allow.new.levels = TRUE)
    out <- exp(replicate(n_bootstraps, rnorm(n = nrow(newdata_f), 
                                             mean = est$fit, sd = est$se.fit)))
    out[is.nan(out)] <- NA
    out <- t(apply(out, 1, function(x) {
      if (all(is.na(x))) return(x) else return(sort(x))
    }))
    
    return(out)
  }
}
  
# Function to extract the distribution ("family") used to fit the model---------
f_family <- function(fit_f = fit) {
  
    #...................................
    ## Depending on the class of the fit object...  
    
    if (class(fit_f)[1] == "glm") { return(family(fit_f)$family)}    
    if (class(fit_f)[1] == "glm.nb") { return("negbin")}  
    if (class(fit_f)[1] == "zeroinfl") { return(fit_f$dist)}
    if (class(fit_f)[1] == "glmmTMB") { return(fit_f$modelInfo$family$family)} 
    if (class(fit_f)[1] %in% c("gam", "bam")) {return(family(fit_f)$family)}
    
  }

# Upload the training and predicting data ---------------------------------------
training_data <- rio::import('05_prepare_training_data/output/som_training_data.rds')|> 
  dplyr::filter(date >= "2015-01-01" & p_time > 0) |> drop_na() 

predicting_data <- rio::import('06_prepare_predicting_data/output/som_predicting_data.rds')|> 
  dplyr::filter(date >= "2015-01-01") |> drop_na()

# Define the variable
resp_var <- 'n_died'

## Define the pvalue and population average
if(resp_var == 'cdr' | resp_var == 'n_died'){
  p_time_var <- 'p_time'
  pred_value <- 'n_died'
  pop_var <- 'pop_average'
}else{
  p_time_var <- 'p_time_u5'
  pred_value <- 'n_died_u5'
  pop_var <- 'pop_average_u5'
}

## Calculate folds
training_data <- training_data |> dplyr::mutate(fold_unit = paste(district, cut(time_unit, 
                                                                             breaks = c(seq(0, max(time_unit), by = 3)) ), sep="_"))

##Concatenation of some folds to have less folds
k_folds <- 10
fold_units <- sort(unique(training_data$fold_unit))
shuffle <- data.frame(fold_unit = fold_units,
                      new_rank = sample(1:length(fold_units), 
                                        length(fold_units), 
                                        replace = FALSE)
)
concat_folds <- split(shuffle$new_rank, sort(shuffle[, "new_rank"]) %% k_folds)
names(concat_folds) <- paste("fold", names(concat_folds), sep = "")
splits <- data.frame(fold_k = rep(names(concat_folds), sapply(concat_folds, length)),
                     new_rank = unlist(concat_folds))
shuffle <- merge(shuffle, splits, by = "new_rank")    

# Attribute folds to data, and order by fold
training_data <- merge(training_data, shuffle, by = "fold_unit", all.x = TRUE)
training_data <- training_data[order(training_data$fold_k), ]


###-----------------------------------------------------------------------------
list_pred_to_try <- c('acled_event_rate_scn', 'cdi_lag5_scn', 'rainfall_lag5_scn',
                      'measles_cases_rate_cat', 'tot_goat_cereal_smooth_lag4_scn', 
                      'cholera_cases_rate_cat',
                      'malaria_cases_rate_cat','tot_wage_cereal_smooth_lag4_scn', 
                      "sam_admissions_rate_lag1_scn")


####-- CROSS VALIDATION -- SELECTION OF FEATURES -------------------------------
metrics <- c()
list_preds <- c()
for(nb_pred in seq(from = 9, to = 5)){
  list_preds_possible <- combn(list_pred_to_try, m=as.integer(nb_pred))
  for(ind in 1:ncol(list_preds_possible)){
    select_preds <- c(list_preds_possible[, ind])
    print(select_preds)
    fit_formula <- as.formula(paste(pred_value," ~", 
                                    paste(select_preds, collapse = " + "), "+ offset(log(", p_time_var, "))"))
    glm_fit <- glm(formula = fit_formula, data = training_data, weights = qualityScore,
                   family = "poisson")
    
    res <- f_cv_francesco(data_f = training_data, which_fold_f = "fold3",
                          k_folds_f = 10, fit_f = glm_fit, f_family_f = f_family, 
                          f_predict_f = f_predict, verbose = TRUE)
    
    metrics <- rbind(metrics, res$cv_metrics)
    list_preds <- rbind(list_preds, paste(c(select_preds), collapse=", "))
  }
}

results_final <- data.frame(metrics)
results_final$variable <- list_preds


write.csv(results_final, '07_model_exploration/ouput/som_final_output_glm.csv', row.names = FALSE)
