################################################################################
#                                                                              #
# Goal:            Process predictors transf norm and cat                      #
#                                                                              #
################################################################################

# get data
predictors_data <- rio::import('04_process_predictors/output/som_predictors_complete_rate.rds')

# Create lags ------------------------------------------------------------------

# select vars to make 6 lags
vars_6lags <- c("tot_wage_cereal_smooth", "water_price_smooth", 
                "tot_goat_cereal_smooth")
vars_2lags <- c("sam_admissions_rate")

# generate lags for each data
res_6lags <- mast::f_create_lags(predictors_data, c("rainfall_rollmean", "cdi"),
                                col_admin1 = 'region', col_admin2 = 'district',
                                 lags = 2:6)
res_6lags <- mast::f_create_lags(res_6lags, vars_6lags,
                                 col_admin1 = 'region', col_admin2 = 'district',
                                 lags = 2:4)
predictor_complete <- mast::f_create_lags(res_6lags, vars_2lags,
                                          col_admin1 = 'region', col_admin2 = 'district',
                                          lags = 1:2)

# check to seem if lags are created
skimr::skim(predictor_complete)

## Outliers --------------------------------------------------------------------
# Outliers ---------------------------------------------------------------------
# Table of maximum cutoffs at which to cut some predictors
df_outliers <- data.frame(
  preds_outliers = c(
    "acled_event_rate", "acled_fatalities_rate",
    "dep_rate", "malaria_cases_rate",
    "sam_admissions_rate_lag1"
  ),
  cutoff = c(7.5, 10, 0.2, 100, 1000)
)

# Constrain outliers to a certain max (only in training data)
predictor_complete <-
  predictor_complete |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(df_outliers$preds_outliers),
      .fns = ~ scales::oob_squish(
        .x,
        range = c(
          0,
          df_outliers[
            df_outliers$preds_outliers == dplyr::cur_column(),
            "cutoff"
          ]
        )
      ),
      .names = "{.col}"
    )
  )


# Transform some predictors ----------------------------------------------------
# Square root departure rate
predictor_complete <- predictor_complete |>
  dplyr::mutate(dep_rate_sqt = sqrt(dep_rate))

# Scale and normalise continuous predictors (as an alternative to be tested)
# which predictors
preds_scn <- c(
  "acled_event_rate", "acled_fatalities_rate",
  "aidworkers_killed_rate", "aidworkers_wounded_rate",
  "malaria_cases_rate", "measles_cases_rate", 'cdi', 'cdi_lag2',
  "cholera_cases_rate", "dep_rate_sqt", "prop_idp",
  "rainfall_rollmean_lag5", "cdi_lag5", "sam_admissions_rate_lag1",
  "tot_goat_cereal_smooth_lag2", "tot_wage_cereal_smooth_lag2",
  'tot_goat_cereal_smooth_lag3', "tot_wage_cereal_smooth_lag3",
  "water_price_smooth_lag2"
)

# scale and normalise
predictor_complete <- mast::f_norm_preds(predictor_complete, preds_scn)

# Categorise all predictors ----------------------------------------------------

## Upload breaks and lavels for Categories
source('04_process_predictors/code/00_som_breaks_and_labels.R')

# create categories of variables of interest
predictor_complete <- mast::f_categorise(predictor_complete, breaks_and_labels)

# Save final predictor data ----------------------------------------------------

saveRDS(predictor_complete, 
        "04_process_predictors/output/som_predictors_data_complete.rds")