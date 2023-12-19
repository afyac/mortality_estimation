update_district <- function(dir_path){
  hh_obs <-  read.csv(paste(dir_path, 'output/data_smart_survey_updated.csv', sep=""))
  hh_obs <- subset(hh_obs, !is.na(hh_obs$stratum))
  metadata <- read.csv(paste(dir_path, 'output/metadata_updated.csv', sep=""))
  hh_obs$stratum <- tolower(gsub("[[:punct:][:blank:]]+", "", hh_obs$stratum))
  hh_obs['stratum'][which(hh_obs$stratum == 'beledweyne'), ] <- 'beletweyne'
  hh_obs['stratum'][which(hh_obs$stratum == 'calula'), ] <- 'caluula'
  hh_obs['stratum'][which(hh_obs$stratum == 'dinsor'), ] <- 'diinsoor'
  hh_obs['stratum'][which(hh_obs$stratum == 'galkacyo'), ] <- 'gaalkacyo'
  hh_obs['stratum'][which(hh_obs$stratum == 'garowe'), ] <- 'garoowe'
  hh_obs['stratum'][which(hh_obs$stratum == 'kismayo'), ] <- 'kismaayo'
  hh_obs['stratum'][which(hh_obs$stratum == 'xuddur'), ] <- 'xudur'
  hh_obs['stratum'][which(hh_obs$stratum == 'dhusamareb'), ] <- 'dhuusamarreeb'
  hh_obs['stratum'][which(hh_obs$stratum == 'dhuusamareeb'), ] <- 'dhuusamarreeb'
  hh_obs['stratum'][which(hh_obs$stratum == 'goldogob'), ] <- 'galdogob'
  hh_obs['stratum'][which(hh_obs$stratum == 'laascanood'), ] <- 'laascaanood'
  hh_obs['stratum'][which(hh_obs$stratum == 'oodweyne'), ] <- 'owdweyne'
  hh_obs['stratum'][which(hh_obs$stratum == 'odweyne'), ] <- 'owdweyne'
  hh_obs['stratum'][which(hh_obs$stratum == 'wajid'), ] <- 'waajid'

  ## Complete missing quality Score
  for(strat in unique(hh_obs$stratum)){
    subset <- hh_obs[which(hh_obs$stratum == strat & is.na(hh_obs$qualityScore) == FALSE), ]
    mean_quality_score <- mean(subset$qualityScore)
    hh_obs['qualityScore'][which(hh_obs$stratum == strat & (is.na(hh_obs$qualityScore) | hh_obs$qualityScore == 0)), ] <- mean_quality_score
  }

  for(ind in which(is.na(metadata$Quality_Score) | metadata$Quality_Score == 0)){
    metadata['Quality_Score'][ind,] <- hh_obs[which(hh_obs$surveyId == metadata['SurveyID'][ind,]),]$qualityScore[1]
  }
  #Change LHZ variable into idp or other
  metadata <- metadata |> mutate(LHZ = replace(LHZ, LHZ %in% c('idp', 'idps'), 'idp'),
                                 LHZ = replace(LHZ, LHZ != 'idp' | is.na(LHZ), 'other'))
  ##Modify the date when there are some issues -- Kind of a manual part
  metadata$Start_Date <- ifelse(abs(year(as.Date(metadata$Start_Date)) - year(as.Date(metadata$End_Date))) == 10, as.character(as.Date(metadata$Start_Date) + years(10)), metadata$Start_Date)
  metadata$Start_Date <- ifelse(abs(year(as.Date(metadata$Start_Date)) - year(as.Date(metadata$End_Date))) == 20, as.character(as.Date(metadata$Start_Date) + years(20)), metadata$Start_Date)
  if(length(metadata['End_Date'][which(year(as.Date(metadata$End_Date))  == 2024),] != 0)){
    metadata['End_Date'][which(year(as.Date(metadata$End_Date)) == 2024),] <- as.character(as.Date(metadata['End_Date'][which(year(as.Date(metadata$End_Date))  == 2024),]) %m-% years(1))
  }
  metadata$End_Date[which(as.Date(year(as.Date(metadata$End_Date)),origin="1970-01-01") == 2023 & month(as.Date(metadata$End_Date)) > 5)] <- metadata$Start_Date[which(as.Date(lubridate::year(as.Date(metadata$End_Date, origin="1970-01-01")),origin="1970-01-01") == 2023 & lubridate::month(as.Date(metadata$End_Date,origin="1970-01-01")) > 5)]
  metadata$Start_Date[which(abs(as.Date(metadata$Start_Date) - as.Date(metadata$End_Date)) > 360)] <- metadata$End_Date[which(abs(as.Date(metadata$Start_Date) - as.Date(metadata$End_Date)) > 360)]
  metadata$Start_Date[which(year(as.Date(metadata$Start_Date)) == 2008)] <- as.character(as.Date('2019/06/17'))
  metadata$End_Date[which(year(as.Date(metadata$End_Date)) == 2008)] <- as.character(as.Date('2019/06/17'))
  
  ## Check Redundancy
  # ind <- which(duplicated(metadata[c('District', 'Region', 'LHZ', 'Start_Date', 'End_Date', 'Recall_Days', 'Quality_Score')]))
  # if(length(ind) != 0){
  #   metadata <- metadata[-c(ind),]
  #   hh_obs <- hh_obs[-c(which(hh_obs$surveyId %in% metadata['SurveyID'][ind,])),]
  # }
  # 
  ##Match district with OCHA one and add pcode
  
  admin2_file$stratum <- tolower(gsub("[[:punct:][:blank:]]+", "", admin2_file$district))
  
  hh_obs_ <- merge(hh_obs, admin2_file,  by='stratum', all.x = TRUE)
  hh_obs_ <- subset(hh_obs_, !is.na(hh_obs_$n))
  hh_obs_ <- hh_obs_[,!names(hh_obs_) %in% 
                       c("stratum")]
  
  
  write.csv(hh_obs_, paste(dir_path, 'output/data_smart_survey_updated.csv', sep=""), row.names = FALSE)
  write.csv(metadata, paste(dir_path, 'output/metadata_updated.csv', sep=""), row.names = FALSE)
}
