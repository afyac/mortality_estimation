################################################################################
#                                                                              #
# Goal:            Process predictors complete and rate                        #
#                                                                              #
################################################################################

# Get the dataset --------------------------------------------------------------

## Upload the different predictor datasets
acled_data <- rio::import('03_update_predictors/output/som_acled_data.rds')
aid_workers_data <- rio::import('03_update_predictors/output/som_aid_workers_data.rds')
cholera_data <- rio::import('03_update_predictors/output/som_cholera_fsnau.rds')
climate_data <- rio::import('03_update_predictors/output/som_climate_data.rds')
malaria_data <- rio::import('03_update_predictors/output/som_malaria_who.rds')
markets_data <- rio::import('03_update_predictors/output/som_markets_data.rds')
measles_data <- rio::import('03_update_predictors/output/som_measles_fsnau.rds')
sam_data <- rio::import('03_update_predictors/output/som_sam_data.rds')

## Upload the population dataset
pop_data <- rio::import("02_pop_reconstruction/output/som_out_pop_best.csv") |>
  dplyr::select(district, year = y, month = m, 
                pop_average = pop, pop_average_u5=pop_u5,
                pop_average_low = pop_low, pop_average_u5_low=pop_u5_low,
                pop_average_upp = pop_upp, pop_average_u5_upp=pop_u5_upp,
                prop_idp, dep_rate) |>
  # ensure district names are correct
  dplyr::mutate(district = mast::admin_match(district)) |>
  dplyr::arrange(district, year, month) 

## Generate time unit cross district datasets
admin2 <- rio::import('00_overall_data/som_admin2.xlsx')
ts <- mast::f_gen_ts(admin2_f = admin2,name_col = 'district', y_end = lubridate::year(END_DATE_ANALYSIS))

# variables to join by
var_by <- c(
  "district", "region", "pcode", "year", "month", 
  'reg_code', 'time_unit', 'date'
)

## Join Datasets
predictor_data <- pop_data |>
  # join all other dataset 
  dplyr::left_join(ts, by = c("year", "month", "district")) |>
  dplyr::left_join(acled_data, by = var_by) |>
  dplyr::left_join(aid_workers_data, by = var_by) |>
  dplyr::left_join(cholera_data, by = var_by) |>
  dplyr::left_join(malaria_data, by = var_by) |>
  dplyr::left_join(measles_data, by = var_by) |>
  dplyr::left_join(climate_data, by = var_by) |>
  dplyr::left_join(sam_data, by = var_by) |>
  dplyr::left_join(markets_data, by = var_by)

## Save the datasets
predictor_data |> saveRDS('04_process_predictors/output/som_predictor_none_complete.rds')

# Complete Data -- Impute Missing ----------------------------------------------

# check missingness before may 2023 
# (water prices is the only variable with missing data - 36%)
predictor_data |>
  dplyr::filter(year > 2014 & date <= END_DATE_ANALYSIS) |> 
  skimr::skim()

mice_mod <- mice::mice(predictor_data, m = 1, method = "rf", seed = 500)

# get complete data and remove the predictors to impute water price
predictors_complete <- mice::complete(mice_mod)

# smooth water prices over times
predictors_complete <- predictors_complete |>
  dplyr::group_by(dplyr::across(district)) |>
  dplyr::mutate(
    water_price_smooth = predict(
      smooth.spline(x= na.omit(cbind(time_unit, water_price)),
                    spar = 0.4), time_unit)$y
  ) |>
  dplyr::ungroup()

# check again
predictors_complete |>
  dplyr::filter(year > 2014 & date <= END_DATE_ANALYSIS) |> 
  skimr::skim()

# Create Rate ------------------------------------------------------------------
# select the predictors that require to be in rates
pred_rates <- c(
  "malaria_cases", "measles_cases",
  "cholera_cases", "acled_fatalities",
  "acled_event", "sam_admissions",
  "aidworkers_killed", "aidworkers_wounded"
)

# create the new variables with _rate at the end
predictors_complete <- mast::f_rate_variables(predictors_complete, 
                                              pred_rates = pred_rates, 
                                              col_admin2 = 'district',
                                              col_pop = 'pop_average')

# check to seem if rates are created
skimr::skim(predictors_complete)

# save the predictor data with rate and completemess
predictors_complete |>
  saveRDS("04_process_predictors/output/som_predictors_complete_rate.rds")
