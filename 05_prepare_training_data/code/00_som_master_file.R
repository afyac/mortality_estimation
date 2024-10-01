################################################################################
#                                                                              #
# Goal:            Household data preparation and meging with predictors       #
#                                                                              #
################################################################################

## CLEAN SMART SURVEYS & JOIN METADATA -----------------------------------------

## Upload smart surveys and metadata
hh_obs <- rio::import('01_extract_smart_surveys/output/data_smart_survey_updated.csv')
metadata <- rio::import('01_extract_smart_surveys/output/metadata_updated.csv') |>
  dplyr::rename(end_date = End_Date, 
                start_date = Start_Date, 
                recall_days = Recall_Days, 
                survey_id = SurveyID)

## Generate time unit cross district datasets
admin2 <- rio::import('00_overall_data/som_admin2.xlsx')
ts <- mast::f_gen_ts(admin2_f = admin2, name_col = 'district')

## Clean the Metadata file
metadata <- mast::f_clean_metadata(metadata, ts)

##Calculate proportion for each month in the time series and each survey, 
# update corresponding survey coverage column
surveys_cov <- expand.grid(unique(hh_obs$surveyId), sort(unique(ts$time_unit)))
colnames(surveys_cov)<- c("surveyId", "time_unit")
surveys_cov[, "month_coverage"] <- 0 # all values set to 0 for now
surveys_cov <- surveys_cov[order(surveys_cov[, "surveyId"], surveys_cov[, "time_unit"]), ]
surveys_cov[, "month_coverage"] <- apply(surveys_cov, 1, mast::f_calc_days, metadata)

# join hh_obs and metadata
hh_obs_complete <- merge(hh_obs, metadata, by.x=c('surveyId'),
                         by.y=c('survey_id'), 
                         all.x=TRUE)

## JOIN THE PREDICTORS DATASETS ------------------------------------------------

# Upload the dataset
predictors_data <- rio::import('04_process_predictors/output/som_predictors_data_complete.rds')

# Calculate a weighted predictors using the month coverage and the different predictors
weighted_predictors <- mast::f_calculate_weighted_predictors(predictors_data, ts, 
                                                             surveys_cov,
                                                             hh_obs_complete, 'district')

# Select Predictors to keep
variables <- c('region', 'district', 'admin0', 
           'pcode', 'n', 'n_u5', 
           'n_died', 'n_died_u5', 
           'p_time', 'p_time_u5', 'surveyId', 
           'qualityScore', 'hh_weights', 'LHZ', 
           'recall_days', 'mid_time_unit',
           'month_recall_mid', 'year_recall_mid')
hh_obs_complete <- hh_obs_complete |> dplyr::select(variables) |>
  dplyr::rename(survey_id = surveyId, 
                lhz_type = LHZ, 
                month = month_recall_mid, 
                year = year_recall_mid, 
                time_unit = mid_time_unit)

# complete the new predictors with the metadata and the data
data_complete <- merge(hh_obs_complete, weighted_predictors, 
                       by.y=c('surveyId'), by.x=c('survey_id'), 
                       all.y=TRUE)

# create categories of variables of interest

## Upload breaks and labels 
source('04_process_predictors/code/00_som_breaks_and_labels.R')
## Categorise the dataset
data_complete <- mast::f_categorise(data_complete, breaks_and_labels)

# save the training data
data_complete |>
  dplyr::mutate(date = as.Date(paste0(year, "-", month, "-01")))|> 
  saveRDS("05_prepare_training_data/output/som_training_data.rds")

