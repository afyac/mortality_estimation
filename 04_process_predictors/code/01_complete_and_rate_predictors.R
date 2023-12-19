################################################################################
#                                                                              #
# Goal:            Process predictors complete and rate                        #
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

# Get and manage datasets ------------------------------------------------------

# get predictor datasets
markets_data <- import("03_update_predictors/output/som_markets_tot_updated.rds")
sam_data <- import("03_update_predictors/output/som_sam_admissions.rds")
climate_data <- import("03_update_predictors/output/som_climate_data.rds")
attacks_aid_workers <- import("03_update_predictors/output/som_attacks_aid_workers.rds")
acled_data <- import("03_update_predictors/output/som_conflict_acled.rds")
malaria_data <- import("03_update_predictors/output/som_malaria_data.rds")
measles_data <- import("03_update_predictors/output/som_malaria_measles_cholera.rds") |> 
  dplyr::select(-cholera_cases, -malaria_cases)
cholera_data <- import("03_update_predictors/output/som_malaria_measles_cholera.rds") |> 
  dplyr::select(-measles_cases, -malaria_cases)

# get district names
district_lables <- import("00_overall_data/som_admin2.xlsx") |>
  dplyr::select(region, district, pcode)

# get pop_denom
pop_denom <- 
  import("02_pop_reconstruction/output/som_out_pop_best.csv") |>
  dplyr::select(district, year = y, month = m, 
                pop_average = pop, pop_average_u5=pop_u5,
                pop_average_low = pop_low, pop_average_u5_low=pop_u5_low,
                pop_average_upp = pop_upp, pop_average_u5_upp=pop_u5_upp,
                prop_idp, dep_rate) |>
  # ensure district names are correct
  dplyr::mutate(district = mast::admin_match(district)) |>
  arrange(district, year, month) |>
  left_join(district_lables, by = "district")

# check if all combinations year, month and district exist in data
f_test_joins(pop_denom)

# Generate time-unit -----------------------------------------------------------

# make time unit (starts at 2014 to get 2013)
time_unit <- f_generate_time_units(
  y_analysis_start = 2010, y_analysis_end = 2024,
  m_analysis_start = 1, m_analysis_end = 12
)

# Join predictors, time-unit and pop_denom -------------------------------------

# variables to join by
var_by <- c(
  "district", "region", "pcode", "year", "month"
)

# Join datasets
# Note: I am starting from 2013 so that the lags use up the time before 2014
# All predictors except water price are complete; there are also additional
# terms of trade predictors included in the dataframe to impute the missing
# water price values later (they will be removed later on).

predictor_data <- pop_denom |>
  # join all other dataset 
  left_join(time_unit, by = c("year", "month")) |>
  left_join(acled_data, by = var_by) |>
  left_join(attacks_aid_workers, by = var_by) |>
  left_join(cholera_data, by = var_by) |>
  left_join(malaria_data, by = var_by) |>
  left_join(measles_data, by = var_by) |>
  left_join(climate_data, by = var_by) |>
  left_join(sam_data, by = var_by) |>
  left_join(markets_data, by = var_by) |>
  # remove vars that are not needed
  dplyr::select(-tot_wage_cereal, -tot_goat_cereal, -reg_code) |>
  # give missing values for attack on aidworkrs a zero
  dplyr::mutate(across(
    starts_with(c("aidworkers", "acled")), 
    ~ replace_na(.x, 0))) |>
  dplyr::select(district, region, year, month, pop_average, everything()) |> 
  dplyr::rename(rainfall = rainfall_rollmean) |> 
  dplyr::mutate(date = as.Date(paste0(year, "-", month, "-01")))

# check if all combinations year, month and district exist in data
f_test_joins(predictor_data)

# save the predictor data without completeness
predictor_data |>
  saveRDS("04_process_predictors/output/som_predictors_none_complete.rds")

# Impute missing ---------------------------------------------------------------

# check missingness before may 2023 
# (water prices is the only variable with missing data - 36%)
predictor_data |>
  dplyr::filter(date <= END_DATE_ANALYSIS) |> 
  skimr::skim()

# impute water prices (seems decent!)
mice_mod <- mice(predictor_data, m = 1, method = "rf", seed = 500)

# inspect quality of imputations
stripplot(mice_mod, water_price, pch = 19, xlab = "Imputation number")

# get complete data and remove the predictors to impute water price
predictors_complete <- complete(mice_mod)

# smooth water prices over times
predictors_complete <- predictors_complete |>
  dplyr::group_by(district) |>
  dplyr::mutate(
    water_price_smooth = predict(
      smooth.spline(na.omit(cbind(time_unit, water_price)), 
                    spar = 0.4), time_unit)$y
  ) |> 
  dplyr::ungroup() 

# check again
predictors_complete |>
  dplyr::filter(year > 2014 & date <= END_DATE_ANALYSIS) |> 
  skimr::skim()

# Create rates -----------------------------------------------------------------

# select the predictors that require to be in rates
pred_rates <- c(
  "malaria_cases", "measles_cases",
  "cholera_cases", "acled_fatalities",
  "acled_event", "sam_admissions",
  "aidworkers_killed", "aidworkers_wounded"
)

# create the new variables with _rate at the end
predictors_complete <- predictors_complete |>
  dplyr::group_by(district, date) |> 
  dplyr::mutate(across(
    .cols = all_of(pred_rates),
    .fns = ~ (.x * 1e5) / pop_average,
    .names = "{.col}_rate"
  )) |>  dplyr::ungroup()

# check to seem if rates are created
skimr::skim(predictors_complete)

# save the predictor data with rate and completemess
predictors_complete |>
  saveRDS("04_process_predictors/output/som_predictors_complete_rate.rds")
