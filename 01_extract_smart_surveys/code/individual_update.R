#---------------------
## UPDATE CSV CONTENT and  METATADA FOR A SMART SURVEY FILE at ADMIN2 level and INDIVIDUAL
# FRANCESCO CODE -- MORTALITY PART 1
#---------------------

individual_output_update <- function(dir_path){
  #' Update csv and metadata for admin2/individual level file
  #' @param dir_path
  #' @return nothing but add some rows into the data_smart_survey_updated.csv and update the metadata_updated.csv file

  files <- list.files(paste(dir_path, 'admin2_surveys/individual_surveys/csv_files/', sep=""))
  if(file.exists(paste(dir_path, 'admin2_surveys/individual_surveys/csv_files/metadata.csv', sep=""))){
    metadata_file <- read.csv(paste(dir_path, 'admin2_surveys/individual_surveys/csv_files/metadata.csv', sep=""))
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
      file_name_path <- paste(dir_path, 'admin2_surveys/individual_surveys/csv_files/', files[2], sep="")
      if(is.na(match(files[2], 'metadata.csv'))){
        metadata <- metadata_file[metadata_file$SurveyID == stringr::str_split(files[2], stringr::fixed('.csv'))[1][[1]][1],]

        df <- read.csv(paste(dir_path, 'admin2_surveys/individual_surveys/csv_files/', files[2], sep=""))
        if(nrow(df)!=1){
          new_data <- rotate_data(df)
          new_data <- clean_data(new_data, metadata)
          metadata_up <- update_metadata(new_data, metadata)
          if((length(metadata_updated) != 0) & metadata[which(!is.na(metadata$SurveyID)),]$SurveyID %in% metadata_updated$SurveyID){
            metadata_updated[which(metadata_updated$SurveyID == metadata[which(!is.na(metadata$SurveyID)),]$SurveyID ), ] <- metadata_up
          }else{
            metadata_updated <- rbind(metadata_updated, metadata_up)
          }
          if(length(hh_obs) == 0){
            hh_obs <- new_data
          }else if(metadata[which(!is.na(metadata$SurveyID)),]$SurveyID %in% hh_obs$surveyId){
            hh_obs <- hh_obs[ - which(hh_obs$surveyId == metadata[which(!is.na(metadata$SurveyID)),]$SurveyID),]
            hh_obs <- rbind(hh_obs, new_data)
          }else{
            hh_obs <- rbind(hh_obs, new_data)
          }

        }
        file.copy(paste(dir_path, 'admin2_surveys/individual_surveys/csv_files/', files[2], sep = ""),
                  paste(dir_path, 'admin2_surveys/individual_surveys/backup_folder/', sep=""))
        unlink(paste(dir_path, 'admin2_surveys/individual_surveys/csv_files/', files[2], sep=""))
        files <- list.files(paste(dir_path, 'admin2_surveys/individual_surveys/csv_files/', sep=""))
      }

    }
    write.csv(metadata_updated, paste(dir_path, 'output/', 'metadata_updated.csv', sep=""), row.names = FALSE)
    write.csv(hh_obs, paste(dir_path, 'output/', 'data_smart_survey_updated.csv', sep=""), row.names = FALSE)
  }

}

update_metadata <-function(new_data, metadata){
  metadata_up <- metadata
  if(length(metadata[which(!is.na(metadata$UnkCode)),]$UnkCode) == 1){
    unk_code <- metadata[which(!is.na(metadata$UnkCode)),]$UnkCode
  }else{
    unk_code <- NA
  }
  if(length(metadata[which(!is.na(metadata$InjCode)),]$InjCode) == 1){
    inj_code <- metadata[which(!is.na(metadata$InjCode)),]$InjCode
  }else{
    inj_code <- NA
  }
  if(length(metadata[which(!is.na(metadata$ViolCode)),]$ViolCode) == 1){
    viol_code <- metadata[which(!is.na(metadata$ViolCode)),]$ViolCode
  }else{
    viol_code <- NA
  }
  ##Calculate the lshtm design to produce cdr update
  if(length(unique(new_data$Cluster)) == 1 | length(unique(new_data$Cluster)) == 0){
    metadata_up$lshtm_survey_design <- "SRS or exhaustive"
    survey_design <- survey::svydesign(id = ~0, data = subset(new_data, p_time > 0) )
    survey_design_u5 <- survey::svydesign(id = ~0, data = subset(new_data, p_time_u5 > 0))
    survey_design_f <- survey::svydesign(id = ~0, data = subset(new_data, p_time_f > 0))
    survey_design_m <- survey::svydesign(id = ~0, data = subset(new_data, p_time_m > 0) )
  }else{
    metadata_up$lshtm_survey_design <- "multi-stage cluster"
    survey_design <- survey::svydesign(id = ~Cluster, data = subset(new_data, p_time > 0) )
    survey_design_u5 <- survey::svydesign(id = ~Cluster, data = subset(new_data, p_time_u5 > 0))
    survey_design_f <- survey::svydesign(id = ~Cluster, data = subset(new_data, p_time_f > 0))
    survey_design_m <- survey::svydesign(id = ~Cluster, data = subset(new_data, p_time_m > 0) )
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

  fit <- survey::svyglm(n_died_f~NULL, survey_design_f, family="poisson", offset=log(p_time_f) )
  metadata_up$lshtm_cdr_f_est <- exp(summary(fit)$coefficients[[1]] ) * 10000
  metadata_up$lshtm_cdr_f_log.se <- summary(fit)$coefficients[[2]]
  metadata_up$lshtm_cdr_f_lci <- exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000
  metadata_up$lshtm_cdr_f_uci <- exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000

  fit <- survey::svyglm(n_died_m~NULL, survey_design_m, family="poisson", offset=log(p_time_m) )
  metadata_up$lshtm_cdr_m_est <- exp(summary(fit)$coefficients[[1]] ) * 10000
  metadata_up$lshtm_cdr_m_log.se <- summary(fit)$coefficients[[2]]
  metadata_up$lshtm_cdr_m_lci <- exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000
  metadata_up$lshtm_cdr_m_uci <- exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000

  if (is.na(inj_code) == FALSE){
    fit <- survey::svyglm(n_died_inj~NULL, survey_design, family="poisson", offset=log(p_time) )
    metadata_up$lshtm_cdr_inj_est <- exp(summary(fit)$coefficients[[1]] ) * 10000
    metadata_up$lshtm_cdr_inj_log_se <- summary(fit)$coefficients[[2]]
  }else{
    metadata_up$lshtm_cdr_inj_est <- NA
    metadata_up$lshtm_cdr_inj_log_se <- NA
  }

  if (is.na(viol_code)==FALSE) {
    fit <- survey::svyglm(n_died_viol~NULL, survey_design, family="poisson", offset=log(p_time) )
    metadata_up$lshtm_cdr_viol_est <- exp(summary(fit)$coefficients[[1]] ) * 10000
    metadata_up$lshtm_cdr_viol_log_se <- summary(fit)$coefficients[[2]]
  }else{
    metadata_up$lshtm_cdr_viol_est <- NA
    metadata_up$lshtm_cdr_viol_log_se <- NA
  }

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

  metadata_up$cdr_rr_males <- ifelse(metadata_up$lshtm_cdr_f_est > 0.01,
                                     metadata_up$lshtm_cdr_m_est / metadata_up$lshtm_cdr_f_est, NA)
  return(metadata_up)
}



rotate_data <- function(df){
  transpose_data <- dplyr::select(df, contains(c('sex', 'HH', 'Cluster', 'Team'))) |>
    tidyr::pivot_longer(cols=contains('sex'), cols_vary='slowest', names_to= c('personindex'))
  df[colnames(dplyr::select(df, contains('join')))] <- ifelse(dplyr::select(df, contains('join')) == 'y' & is.na(dplyr::select(df, contains('join'))) == FALSE, 1, 0)
  transpose_data <- cbind(transpose_data, dplyr::select(dplyr::select(df, contains(c('age'))) |>
                                                   tidyr::pivot_longer(cols=contains('age'), cols_vary='slowest', names_to= c('age')), 'value'))
  transpose_data <- cbind(transpose_data, dplyr::select(dplyr::select(df, contains(c('join'))) |>
                                                   tidyr::pivot_longer(cols=contains('join'), cols_vary='slowest', names_to= c('age')), 'value'))
  transpose_data <- cbind(transpose_data, dplyr::select(dplyr::select(df, contains(c('left'))) |>
                                                   tidyr::pivot_longer(cols=contains('left'), cols_vary='slowest', names_to= c('age')), 'value'))
  transpose_data <- cbind(transpose_data, dplyr::select(dplyr::select(df, contains(c('born'))) |>
                                                   tidyr::pivot_longer(cols=contains('born'), cols_vary='slowest', names_to= c('age')), 'value'))
  transpose_data <- cbind(transpose_data, dplyr::select(dplyr::select(df, contains(c('died'))) |>
                                                   tidyr::pivot_longer(cols=contains('died'), cols_vary='slowest', names_to= c('age')), 'value'))
  transpose_data <- cbind(transpose_data, dplyr::select(dplyr::select(df, contains(c('cause'))) |>
                                                   tidyr::pivot_longer(cols=contains('cause'), cols_vary='slowest', names_to= c('age')), 'value'))

  colnames(transpose_data) <- c('HH', 'Cluster', 'Team', 'PersonIndex', 'Sex', 'Age', 'Join', 'Left', 'Born', 'Died', 'Cause')
  transpose_data$Sex <- ifelse(transpose_data$Sex == 'm', 1, 0)
  transpose_data$Join <- ifelse(transpose_data$Join == 'y', 1, 0)
  transpose_data$Left <- ifelse(transpose_data$Left == 'y', 1, 0)
  transpose_data$Born <- ifelse(transpose_data$Born == 'y', 1, 0)
  transpose_data$Died <- ifelse(transpose_data$Died == 'y', 1, 0)
  transpose_data$newIndex <- paste(transpose_data$Cluster,transpose_data$HH, sep='_')
  return(transpose_data)
}

clean_data <- function(data, metadata){
  unk_code <- metadata$UnkCode
  inj_code <- metadata$InjCode
  viol_code <- metadata$ViolCode
  recall_period <- metadata$Recall_Days
  if(length(unique(data$Cluster)) > 1){
    data <- data[which(is.na(data$Cluster) == FALSE),]
  }
  data[which(is.na(data$Join)), 'Join'] <- 0
  data[which(is.na(data$Left)), 'Left'] <- 0
  data[which(is.na(data$Died)), 'Died'] <- 0
  data[which(is.na(data$Born)), 'Born'] <- 0
  data[which(is.na(data$Cause)), 'Cause'] <- 0
  data['eligeable'] <- ifelse(is.na(data$Sex) == FALSE & is.na(data$Age) == FALSE, 1, 0)
  results <- data[which(data$eligeable == 1),] |> dplyr::group_by(newIndex) |>
    dplyr::summarise(n = sum(eligeable), n_m = sum(Sex), n_f = sum(Sex == 0), n_u5 = sum(Age < 5),
              n_5 = sum(Age == 5), n_u5_m = sum(Age < 5 & Sex), n_u5_f = sum(Age <5 & Sex == 0),
              n_join = sum(Join), n_join_m = sum(Join & Sex), n_join_f = sum(Join&Sex==0),
              n_join_u5=sum(Join & Age <5), n_join_5 = sum(Join & Age == 5),
              n_left = sum(Left), n_left_m = sum(Left & Sex), n_left_f = sum(Left & Sex ==0),
              n_left_u5 = sum(Left & Age < 5), n_left_5 = sum(Left & Age == 5),
              n_born = sum(Born), n_born_m = sum(Born & Sex), n_born_f = sum(Born & Sex ==0),
              n_died = sum(Died), n_died_m = sum(Died & Sex), n_died_f = sum(Died & Sex == 0),
              n_died_u5 = sum(Died & Age <5), n_died_5 = sum(Died & Age == 5), n_died_u1 = sum(Died & Age < 1),
              n_died_inj =sum(Died & Cause == inj_code), n_died_inj_m = sum(Died & Cause == inj_code & Sex),
              n_died_inj_f = sum(Died & Cause == inj_code & Sex ==0), n_died_inj_u5 = sum(Died & Cause == inj_code& Age <5),
              n_died_unk = sum(Died & Cause == unk_code),
              n_died_viol = sum(Died & Cause == viol_code),
              n_died_viol_u18 = sum(Died & Cause == viol_code & Age < 18),
              n_died_viol_o18 = sum(Died & Cause == viol_code & Age > 18 & Sex == 0),
              n_died_oth = sum(Died) - n_died_inj- n_died_unk - n_died_viol,
              p_time = (n - 0.5 * n_join + 0.5 * n_left - 0.5 * n_born + 0.5 * n_died ) * recall_period,
              p_time_f = (n_f - 0.5 * n_join_f + 0.5 * n_left_f - 0.5 * n_born_f + 0.5 * n_died_f) * recall_period,
              p_time_m = (n_m - 0.5 * n_join_m + 0.5 * n_left_m - 0.5 * n_born_m + 0.5 * n_died_m) * recall_period,
              p_time_u5 = (n_u5 - 0.5 * n_join_u5 + 0.5 * n_left_u5 - 0.5 * n_born + 0.5 * n_died_u5) * recall_period +
                (n_5 - 0.5 * n_join_5 + 0.5 * n_left_5 + 0.5 * n_died_5 ) * recall_period / 365
    )
  results <- results |> tidyr::separate_wider_delim(newIndex, '_', names=c('Cluster', 'HH'))
  results$surveyId <- metadata[which(!is.na(metadata$SurveyID)),]$SurveyID
  results$stratum <- metadata[which(!is.na(metadata$District)),]$District
  if(length(metadata[which(!is.na(metadata$Quality_Score)),]$Quality_Score) ==0){
    results$qualityScore <- 1
  }else{
    results$qualityScore <- metadata[which(!is.na(metadata$Quality_Score)),]$Quality_Score
  }
  results$hh_weights <- 1

  return(results)
}



