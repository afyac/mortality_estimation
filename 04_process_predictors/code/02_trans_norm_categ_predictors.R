################################################################################
#                                                                              #
# Goal:            Process predictors transform, normalize and categorize      #
#                                                                              #
# Written:         Mo Yusuf                                                    #
# Date written:    2023-08-03                                                  #
#                                                                              #
# Reviewer:        Yamna Ouchtar                                               #
# Date reviewed:   2023-08-xx                                                  #
#                                                                              #
#                                                                              #
################################################################################
# PLEASE BEFORE ANYTHING CHANGE OR CHECKS THE DATE  ----------------------------
END_DATE_ANALYSIS <- "2023-09-01"

# get functions
source("04_process_predictors/code/00_functions_predictors.R")

# get data
predictors_data <- rio::import('04_process_predictors/output/som_predictors_complete_rate.rds')

# Create lags ------------------------------------------------------------------

# select vars to make 6 lags
vars_6lags <- c("tot_wage_cereal_smooth", "water_price_smooth", 
                "tot_goat_cereal_smooth")
vars_2lags <- c("sam_admissions_rate")

# generate lags for each data
res_6lags <- f_create_lags(predictors_data, c("rainfall", "cdi"), lags = 2:6)
res_6lags <- f_create_lags(res_6lags, vars_6lags, lags = 2:4)
predictor_complete <- f_create_lags(res_6lags, vars_2lags, lags = 1:2)

# select data from 2014 onwards
predictor_complete <- predictor_complete |>
  dplyr::select(
    time_unit, date, year, month, region, 
    district, pcode, pop_average, pop_average_u5, 
    prop_idp, dep_rate, everything() 
  ) |> 
  dplyr::arrange(region, district, date)


# check to seem if lags are created
skimr::skim(predictor_complete)

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
  predictor_complete %>%
  mutate(
    across(
      all_of(df_outliers$preds_outliers),
      .fns = ~ scales::oob_squish(
        .x,
        range = c(
          0,
          df_outliers[
            df_outliers$preds_outliers == cur_column(),
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
  "rainfall_lag5", "cdi_lag5", "sam_admissions_rate_lag1",
  "tot_goat_cereal_smooth_lag2", "tot_wage_cereal_smooth_lag2",
  'tot_goat_cereal_smooth_lag3', "tot_wage_cereal_smooth_lag3",
  "water_price_smooth_lag2"
)

# scale and normalise
for (i in preds_scn) {
  # training data
  x <- scales::rescale(deframe(predictor_complete[, i]), to=c(0,1))
  # apply same scaling parameters to prediction data
  predictor_complete[, paste(i, "_scn", sep = "")] <- x
}

# Categorise all predictors ----------------------------------------------------

# Define breaks and labels for different groups for categorisation
breaks_and_labels <- list(
  acled_event_rate = list(
    breaks = c(0, 0.0000001, 1, 2, 1000000),
    labels = c("0.00", "0.01 to 0.99", "1.00 to 1.99", ">= 2.00")
  ),
  acled_fatalities_rate = list(
    breaks = c(0, 0.0000001, 1, 2, 1000000),
    labels = c("0.00", "0.01 to 0.99", "1.00 to 1.99", ">= 2.00")
  ),
  aidworkers_killed_rate = list(
    breaks = c(0, 0.0000001, 100000),
    labels = c("0", ">= 1")
  ),
  aidworkers_wounded_rate = list(
    breaks = c(0, 0.0000001, 100000),
    labels = c("0", ">= 1")
  ),
  cholera_cases_rate = list(
    breaks = c(0, 0.0000001, 100000),
    labels = c("0", ">= 1")
  ),
  dep_rate = list(
    breaks = c(0, 0.0000001, 0.01, 0.02, 10),
    labels = c("0.000", "0.001 to 0.009", "0.010 to 0.019", "> = 0.020")
  ),
  malaria_cases_rate = list(
    breaks = c(0, 0.0000001, 50, 10000),
    labels = c("0", "1 to 49", ">= 50")
  ),
  measles_cases_rate = list(
    breaks = c(0, 0.0000001, 100000),
    labels = c("0", ">= 1")
  ),
  prop_idp = list(
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = c("< 25%", "25 to 49%", "50 to 74%", ">= 75%")
  ),
  rainfall = list(
    breaks = c(0, 50, 100, 150, 200, Inf),
    labels = c(
      "< 50mm", "50 to 99mm", "100 to 149mm", "150 to 199mm",
      ">= 200mm"
    )
  ),
  cdi_lag5 = list(
    breaks = c(0, 0.4, 0.6, 0.8, 1.0, Inf),
    labels = c(
      "< 0.4", "0.4 - 0.59", "0.6 - 0.79", "0.8 - 0.99",
      ">= 1"
    )
  ),
  sam_admissions_rate = list(
    breaks = c(0, 100, 199, 100000),
    labels = c("< 100", "100 to 199", ">= 200")
  ),
  tot_goat_cereal_smooth = list(
    breaks = c(0, 200000, 300000, 400000, 1000000),
    labels = c(
      "<200,000", "200,000 to 299,999", "300,000 to 399,999",
      ">= 400,000"
    )
  ),
  tot_wage_cereal_smooth = list(
    breaks = c(0, 20000, 30000, 40000, 1000000),
    labels = c("<20,000", "20,000 to 29,999", "30,000 to 39,999", ">= 40,000")
  ),
  water_price_smooth = list(
    breaks = c(0, 10000, 22000, 34000, Inf),
    labels = c("<10,000", "10,000 to 19,999", "20,000 to 29,999", ">= 30,000")
  )
)

# create categories of variables of interest
predictor_complete <- f_categorise(predictor_complete, breaks_and_labels)

# Save final predictor data ----------------------------------------------------

saveRDS(predictor_complete, 
        "04_process_predictors/output/som_predictors_data_complete.rds")