################################################################################
#                                                                              #
# Goal:            Household data preparation and meging with predictors       #
#                                                                              #
# Written:         Mo Yusuf                                                    #
# Date written:    2023-08-04                                                  #
#                                                                              #
# Reviewer:        xx xx                                                       #
# Date reviewed:   2023-08-xx                                                  #
#                                                                              #
#                                                                              #
################################################################################

# Prepare ----------------------------------------------------------------------
#source functions
source('05_prepare_training_data/code/00_source_functions.R')

## Upload smart surveys and metadata
hh_obs <- rio::import('01_extract_smart_surveys/output/data_smart_survey_updated.csv')
metadata <- rio::import('01_extract_smart_surveys/output/metadata_updated.csv')

## Create time_units 
# make time unit (starts at 2014 to get 2013)
time_unit_data <- f_generate_time_units(
  y_analysis_start = 2010, y_analysis_end = 2024,
  m_analysis_start = 1, m_analysis_end = 12
)

# create data frame of surveys * time series in which to store % of month covered
surveys_cov <- expand.grid(unique(hh_obs$surveyId), sort(time_unit_data$time_unit))
colnames(surveys_cov)<- c("surveyId", "time_unit")
surveys_cov[, "month_coverage"] <- 0 # all values set to 0 for now
surveys_cov <- surveys_cov[order(surveys_cov[, "surveyId"], surveys_cov[, "time_unit"]), ]

#...................................
## Calculate proportion of days in each month in the time series that are covered by each survey's recall period

# Generate necessary time quantities for calculation
metadata$end_recall_date <- as.Date(metadata$End_Date) - (as.Date(metadata$End_Date) - as.Date(metadata$Start_Date))/2
metadata$start_recall_date <- as.Date(metadata$end_recall_date) - metadata$Recall_Days
metadata$year_end_recall_date <- lubridate::year(metadata$end_recall_date)
metadata$month_end_recall_date <- lubridate::month(metadata$end_recall_date)
metadata$year_start_recall_date <- lubridate::year(metadata$start_recall_date)
metadata$month_start_recall_date <- lubridate::month(metadata$start_recall_date)
metadata$days_in_month_start <- lubridate::days_in_month(metadata$start_recall_date)
metadata$days_in_month_end <- lubridate::days_in_month(metadata$end_recall_date)
metadata$day_start <- lubridate::day(metadata$start_recall_date)
metadata$day_end <- lubridate::day(metadata$end_recall_date)

# merge metadata with time units - to have a start and end time unit
metadata <- merge(metadata, time_unit_data, by.y=c('year', 'month'), 
                  by.x=c('year_start_recall_date', 'month_start_recall_date')) |> 
  dplyr::rename(time_unit_recall_start = time_unit)

metadata <- merge(metadata, time_unit_data, by.y=c('year', 'month'), 
                  by.x=c('year_end_recall_date', 'month_end_recall_date')) |> 
  dplyr::rename(time_unit_recall_end = time_unit)

# merge metadata with time units - to have a mid time unit
metadata$recall_mid <- as.Date(metadata$End_Date) - metadata$Recall_Days/2
metadata$year_recall_mid <- year(metadata$recall_mid)
metadata$month_recall_mid <- month(metadata$recall_mid)
metadata <- merge(x=metadata, y=time_unit_data, by.x=c('year_recall_mid', 'month_recall_mid'), 
                  by.y = c('year', 'month'), all.x=TRUE) |> 
  dplyr::rename(mid_time_unit = time_unit)


# Calculate proportion for each month in the time series and each survey, 
# update corresponding survey coverage column
surveys_cov[, "month_coverage"] <- apply(surveys_cov, 1, f_calc_days, metadata)

# join hh_obs and metadata
hh_obs_complete <- merge(hh_obs, metadata, by.x=c('surveyId'), by.y=c('SurveyID'), all.x=TRUE)

# calculate the predictors values to have the right ones 
# first upload predictors data 
predictors_data <- rio::import('04_process_predictors/output/som_predictors_data_complete.rds')

# select only the numeric data
variables <- colnames(predictors_data|> dplyr::select(-district, -region, -date, -month, -pcode, -year, -time_unit, -ends_with("_cat")))

# calculate a weighted predictors using the month coverage and the different predictors
data_final <- data.frame()
for(survey_id in unique(surveys_cov$surveyId)){
  sub_hh_obs <- hh_obs_complete[which(hh_obs_complete$surveyId == survey_id),]
  sub_cov <- surveys_cov[which(surveys_cov$surveyId == survey_id &
                                 surveys_cov$time_unit %in% seq(unique(sub_hh_obs$time_unit_recall_start), unique(sub_hh_obs$time_unit_recall_end))),]
  
  sub <- subset(predictors_data, predictors_data$district == unique(sub_hh_obs$district) &
                  predictors_data$time_unit %in% seq(unique(sub_hh_obs$time_unit_recall_start), unique(sub_hh_obs$time_unit_recall_end)))
  # if(nrow(sub) == 0){
  #   print(sub_cov)
  # }
  if(nrow(sub)!=0){
    aggr <- data.frame(colSums(sub[,variables]*sub_cov$month_coverage,
                               na.rm=TRUE))
    aggr <- data.frame(aggr/sum(sub_cov$month_coverage,na.rm=TRUE))
    aggr <- cbind('names' = rownames(aggr), aggr)
    rownames(aggr) <- 1:nrow(aggr)
    colnames(aggr) <- c('names', 'values')
    aggr$surveyId <- survey_id
    if(nrow(data_final) ==0){
      data_final <- aggr
    }else{
      data_final <- rbind(data_final, aggr)
    }
  }
}
data_final <- data_final |> spread(names, values)

hh_obs_complete <- hh_obs_complete[c('region', 'district', 'admin0', 'pcode', 'n', 'n_u5', 'n_died', 'n_died_u5', 
                      'p_time', 'p_time_u5', 'surveyId', 'qualityScore', 'hh_weights', 'LHZ', 'Recall_Days', 'mid_time_unit',
                      'month_recall_mid', 'year_recall_mid')] |>
  dplyr::rename(survey_id = surveyId, lhz_type = LHZ, month=month_recall_mid, year=year_recall_mid, time_unit =mid_time_unit)
# complete the new predictors with the metadata and the data
data_complete <- merge(hh_obs_complete, data_final, by.y=c('surveyId'), by.x=c('survey_id'), all.x=TRUE)

# Need to calculate the categorical data now -----------------------------------
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
data_complete <- f_categorise(data_complete, breaks_and_labels)

# remove unknown district
data_complete <-data_complete[which(!is.na(data_complete$district)),]

# save the training data
data_complete |>
  dplyr::mutate(date = as.Date(paste0(year, "-", month, "-01")))|> 
  saveRDS("05_prepare_training_data/output/som_training_data.rds")


