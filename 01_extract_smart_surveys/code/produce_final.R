#...............................................................................   
### Function to generate time series of months
#...............................................................................

f_gen_ts <- function(when_f = when) {
  
  # create a time unit variable tm (from month 1 to month T of period) 
  time_unit <- seq(1, (( when_f$y_end + when_f$burn_out_period - when_f$y_start + 
                    when_f$burn_in_period ) * 12 + when_f$m_end - when_f$m_start + 1 ), 1)
  ts <- data.frame(time_unit)
  # Work out corresponding year, month and date values
  ts$year <- floor( (ts$time_unit + when_f$m_start - 2) / 12) + (when_f$y_start - 
                                                         when_f$burn_in_period)
  ts$month <- (ts$time_unit + when_f$m_start - 1) - (ts$year - (when_f$y_start - 
                                                    when_f$burn_in_period) ) * 12
  ts$date <- lubridate::ymd(paste(ts$y, ts$m, "1", sep = "-"))    
  
  # Sort time series
  ts <- ts[order(ts[, "time_unit"]), ]
}

####-------------------------------
# Function TO PRODUCE FINAL OUTPUT USED IN RECONSTRUC POP FOLDER
#contains the principal component used for the next steps and will be saved as a R file
####-------------------------------

produce_final_file <- function(dir_path){
  ##import smart surveys information
  hh_obs <- rio::import('01_extract_smart_surveys/output/data_smart_survey_updated.csv')
  hh_obs <- hh_obs[c('region', 'district', 'admin0', 'pcode', 'n', 'n_u5', 'n_died', 'n_died_u5', 
                     'p_time', 'p_time_u5', 'surveyId', 'qualityScore', 'hh_weights')]
  
  ## import metadata information
  metadata <- rio::import('01_extract_smart_surveys/output/metadata_updated.csv')
  metadata <- metadata[c('SurveyID', 'LHZ', 'Start_Date', 'End_Date', 'Recall_Days')]
  
  ##Merge both
  data <- merge(hh_obs, metadata, by.x='surveyId', by.y='SurveyID', all.x=TRUE)
  
  ## Generate time series 
  time_series <- f_gen_ts(when_f = WHEN)
  
  # Add stard and end date into our dataset
  data$start_year <- year(as.Date(data$Start_Date))
  data$start_month <- month(as.Date(data$Start_Date))
  data$end_year <- year(as.Date(data$End_Date))
  data$end_month <- month(as.Date(data$End_Date))
  data$recall_mid <- as.Date(data$End_Date) - data$Recall_Days/2
  data$year <- year(data$recall_mid)
  data$month <- month(data$recall_mid)
  
  ##Merge mid time to obtain the year, month and time unit
  data <- merge(data, time_series, by.x = c('year', 'month'), by.y=c('year', 'month'), all.x=TRUE)
  # colnames(data)[length(colnames(data))] <- 'time_unit'
  
  #Select the features we are going to use after
  data <- data[c('region', 'district', 'admin0', 'pcode', 'n', 'n_u5', 'n_died', 'n_died_u5', 
                 'p_time', 'p_time_u5', 'surveyId', 'qualityScore', 'hh_weights', 'year', 'month', 
                 'time_unit', 'LHZ', 'Recall_Days')]
  
  #Save the data as .rds
  saveRDS(data, file = paste(dir_path, "/output/som_smart_surveys_data.rds", 
                            sep =""))
}