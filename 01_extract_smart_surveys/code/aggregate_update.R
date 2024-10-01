#---------------------
## UPDATE CSV CONTENT and  METATADA FOR A SMART SURVEY FILE at ADMIN2 level and AGGREGATE
#---------------------


aggregate_output_update <- function(dir_path){
  #' Update csv and metadata for admin2/aggregate level file
  #' @param dir_path
  #' @return nothing but add some rows into the data_smart_survey_updated.csv and update the metadata_updated.csv file

  files <- list.files(paste(dir_path, 'admin2_surveys/aggregate_surveys/csv_files/', sep=""))
  metadata_file <- read.csv(paste(dir_path, 'admin2_surveys/aggregate_surveys/csv_files/metadata.csv', sep=""))
  if(length(which(list.files(paste(dir_path, 'output/', sep="")) == 'metadata_updated.csv')) == 0 ){
    metadata_updated <- data.frame()
  }else{
    metadata_updated <- read.csv(paste(dir_path, 'output/metadata_updated.csv', sep=""))
  }
  if(length(which(list.files(paste(dir_path, 'output/', sep="")) == 'data_smart_survey_updated.csv')) == 0){
    hh_obs <- data.frame()
  }else{
    hh_obs <- read.csv(paste(dir_path, 'output/data_smart_survey_updated.csv', sep=""))
  }
  while(length(files) > 1){ ##because of the metadata file
    file_name_path <- paste(dir_path, 'admin2_surveys/aggregate_surveys/csv_files/', files[2], sep="")
    if(is.na(match(files[2], 'metadata.csv'))){
      metadata <- metadata_file[metadata_file$SurveyID == stringr::str_split(files[2], stringr::fixed('.csv'))[1][[1]][1],]
      df <- read.csv(paste(dir_path, 'admin2_surveys/aggregate_surveys/csv_files/', files[2], sep=""))
      if(nrow(df)!=1){
        new_data <- clean_data_aggregate(df, metadata)
        metadata_up <- update_metadata_aggregate(new_data, metadata)
        if(length(metadata_updated) != 0 & metadata$SurveyID %in% metadata_updated$SurveyID){
          metadata_updated[which(metadata_updated$SurveyID == metadata$SurveyID ), ] <- metadata_up
        }else{
          metadata_updated <- rbind(metadata_updated, metadata_up)
        }
        if(nrow(hh_obs) == 0){
          hh_obs <- new_data
        }else if(metadata$SurveyID %in% hh_obs$surveyId){
          hh_obs <- hh_obs[ - which(hh_obs$surveyId == metadata[which(!is.na(metadata$SurveyID)),]$SurveyID),]
          hh_obs <- rbind(hh_obs, new_data)
        }else{
          hh_obs <- rbind(hh_obs, new_data)
        }

      }
      file.copy(paste(dir_path, 'admin2_surveys/aggregate_surveys/csv_files/', files[2], sep = ""),
                paste(dir_path, 'admin2_surveys/aggregate_surveys/backup_folder/', sep=""))
      unlink(paste(dir_path, 'admin2_surveys/aggregate_surveys/csv_files/', files[2], sep=""))
      files <- list.files(paste(dir_path, 'admin2_surveys/aggregate_surveys/csv_files/', sep=""))
    }

  }
  write.csv(metadata_updated, paste(dir_path, 'output/', 'metadata_updated.csv', sep=""), row.names = FALSE)
  write.csv(hh_obs, paste(dir_path, 'output/', 'data_smart_survey_updated.csv', sep=""), row.names = FALSE)
}

clean_data_aggregate <- function(data, metadata){
  data <- lapply(data, as.numeric)
  data <- replace(data, is.na(data), 0)
  data <- as.data.frame(data)
  colnames(data) <- c("HH", "Cluster", "n", "n_u5" ,"n_join", "n_join_u5", "n_left", "n_left_u5", "n_born", "n_died", "n_died_u5")
  data <- subset(data, data$n >= data$n_join + data$n_born + data$n_join_u5 +  data$n_u5 - data$n_died - data$n_died_u5 - data$n_left - data$n_left_u5)
  data <- apply(data, c(1,2), as.character)
  data <- as.data.frame(apply(t(data), 1, readr::parse_vector, readr::col_integer() ))
  if (length(unique(data$Cluster)) > 1) { data <- subset(data, ! is.na(Cluster) ) }
  data[is.na(data)] <- 0
  recall_period <- metadata$Recall_Days
  if(length(recall_period) ==2){recall_period <- recall_period[2]}
  results <- data.frame()
  data['n_join'][which(is.na(data$n_join)),] <- 0
  data['n_left'][which(is.na(data$n_left)),] <- 0
  data['n_born'][which(is.na(data$n_born)),] <- 0
  data['n_died'][which(is.na(data$n_died)),] <- 0
  data[, 'p_time'] <- (data$n - data$n_join * 0.5 + data$n_left * 0.5 - data$n_born * 0.5 + data$n_died * 0.5 )* recall_period
  data[, 'p_time_u5'] <- (data$n_u5 - data$n_join_u5 * 0.5 + data$n_left_u5 * 0.5 - data$n_born * 0.5 + data$n_died_u5 * 0.5 )* recall_period
  # add blank columns for variables that aggregate surveys don't collect
  data[, c("n_f", "n_m", "n_5", "n_u5_m","n_u5_f", "n_join_f", "n_join_m", "n_left_f", "n_left_m", "n_born_f",
         "n_born_m", "n_died_m", "n_died_f", "n_died_u1", "n_died_5", "n_died_inj", "n_died_inj_f", "n_died_inj_m", "n_died_inj_u5",
         "n_died_unk", "n_died_oth","n_join_5", "n_left_5", "n_died_viol", "n_died_viol_u18", "n_died_viol_o18", "p_time_f", "p_time_m")] <- NA
  data$hh_weights <- 1

  if(length(metadata$SurveyID) ==2){data$SurveyID <- metadata$SurveyID[2]}else{data$surveyId <- metadata$SurveyID}
  if(length(metadata$District) ==2){data$stratum <- metadata$District[2]}else{data$stratum <- metadata$District}
  if(length(metadata$Quality_Score) ==2){data$qualityScore <- metadata$Quality_Score[2]}else{data$qualityScore <- metadata$Quality_Score}
  return(data)
}



update_metadata_aggregate <-function(new_data, metadata){
  metadata_up <- metadata
  unk_code <- metadata$UnkCode
  inj_code <- metadata$InjCode
  viol_code <- metadata$ViolCode

  if(length(unique(new_data$Cluster)) == 1 | length(unique(new_data$Cluster)) == 0){
    metadata_up$lshtm_survey_design <- "SRS or exhaustive"
    survey_design <- survey::svydesign(id = ~0, data = subset(new_data, p_time > 0) )
    survey_design_u5 <- survey::svydesign(id = ~0, data = subset(new_data, p_time_u5 > 0))
  }else{
    metadata_up$lshtm_survey_design <- "multi-stage cluster"
    survey_design <- survey::svydesign(id = ~Cluster, data = subset(new_data, p_time > 0) )
    survey_design_u5 <- survey::svydesign(id = ~Cluster, data = subset(new_data, p_time_u5 > 0))
  }

  fit <- survey::svyglm(n_died~NULL, survey_design, family="poisson", offset=log(p_time) )
  metadata_up$lshtm_cdr_est <- exp(summary(fit)$coefficients[[1]])* 10000
  metadata_up$lshtm_cdr_log_se <- summary(fit)$coefficients[[2]]
  metadata_up$lshtm_cdr_lci <- exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000
  metadata_up$lshtm_cdr_uci <- exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000

  fit <- survey::svyglm(n_died_u5~NULL, survey_design_u5, family="poisson", offset=log(p_time_u5) )
  metadata_up$lshtm_cdr_u5_est <- exp(summary(fit)$coefficients[[1]] ) * 10000
  metadata_up$lshtm_cdr_u5_log_se <- summary(fit)$coefficients[[2]]
  metadata_up$lshtm_cdr_u5_lci <- exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000
  metadata_up$lshtm_cdr_u5_uci <- exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000


  metadata_up$lshtm_cdr_f_est <- NA
  metadata_up$lshtm_cdr_f_log.se <- NA
  metadata_up$lshtm_cdr_f_lci <- NA
  metadata_up$lshtm_cdr_f_uci <- NA
  metadata_up$lshtm_cdr_m_est <- NA
  metadata_up$lshtm_cdr_m_log.se <- NA
  metadata_up$lshtm_cdr_m_lci <- NA
  metadata_up$lshtm_cdr_m_uci <- NA
  metadata_up$lshtm_cdr_inj_est <- NA
  metadata_up$lshtm_cdr_inj_log_se <- NA
  metadata_up$lshtm_cdr_viol_est <- NA
  metadata_up$lshtm_cdr_viol_log_se <- NA
  metadata_up$lshtm_cdr_inj_est <- NA


  fit <- survey::svyglm(n_born~NULL, survey_design, family="poisson", offset=log(p_time) )
  metadata_up$lshtm_cbr_est <- exp(summary(fit)$coefficients[[1]] ) * 1000 * 365

  fit <- survey::svyglm(n_join~NULL, survey_design, family="poisson", offset=log(p_time) )
  metadata_up$lshtm_in_migration_rate_est  <- exp(summary(fit)$coefficients[[1]] ) * 1000 * 365

  fit <- survey::svyglm(n_left~NULL, survey_design, family="poisson", offset=log(p_time) )
  metadata_up$lshtm_out_migration_rate_est  <- exp(summary(fit)$coefficients[[1]] ) * 1000 * 365

  metadata_up$lshtm_net_migration_rate_est <- metadata_up$lshtm_in_migration_rate_est -
    metadata_up$lshtm_out_migration_rate_est

  metadata_up$lshtm_mean_n <- mean(new_data$n - new_data$n_left - new_data$n_died, na.rm = TRUE)
  metadata_up$lshtm_prop_f <- sum(new_data$p_time_f) / sum(new_data$p_time)
  metadata_up$lshtm_prop_u5 <- sum(new_data$p_time_u5) / sum(new_data$p_time)

  sum_ <- sum(new_data$n_died_unk) + sum(new_data$n_died_inj) + sum(new_data$n_died_oth)
  metadata_up$lshtm_prop_unk <- sum(new_data$n_died_unk) / sum_
  metadata_up$lshtm_prop_inj <- sum(new_data$n_died_inj) / sum_
  metadata_up$lshtm_prop_oth <- sum(new_data$n_died_oth) / sum_
  metadata_up$lshtm_prop_viol <- sum(new_data$n_died_viol) / sum_
  metadata_up$lshtm_prop_died_u1 <- sum(new_data$n_died_u1) / sum(new_data$n_died_u5)
  metadata_up$lshtm_prop_inj_m <- sum(new_data$n_died_inj_m) / sum(new_data$n_died_inj)
  metadata_up$lshtm_prop_viol_u18 <- sum(new_data$n_died_viol_u18) / sum(new_data$n_died_viol)
  metadata_up$lshtm_prop_viol_f_o18 <- sum(new_data$n_died_viol_o18) / sum(new_data$n_died_viol)

  metadata_up$cdr_rr_males <- NA
  return(metadata_up)
}


