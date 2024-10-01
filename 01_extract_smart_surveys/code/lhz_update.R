#---------------------
## UPDATE CSV CONTENT and  METATADA FOR A SMART SURVEY FILE at LHZ level and INDIVIDUAL
#---------------------

lhz_output_update <- function(dir_path){
  #' Update csv and metadata for lhz level file
  #' @param dir_path
  #' @return nothing but add some rows into the data_smart_survey_updated.csv and update the metadata_updated.csv file

  metadata_file <- read.csv(paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/metadata.csv', sep=""))
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

  files <- list.files(paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/', sep=""), pattern='\\.csv$')

  while(length(files) > 1){ ##because of the metadata file
    file_name_path <- paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/', files[2], sep="")
    cluster_name_path <- paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/', files[3], sep="")
    if(is.na(match(files[2], 'metadata.csv'))){
      metadata <- metadata_file[metadata_file$SurveyID == stringr::str_split(files[2], stringr::fixed('.csv'))[1][[1]][1],]

      df <- read.csv(paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/', files[2], sep=""))
      if(sum(df$P1_age, na.rm = TRUE) != 0){
        clusters <- read.csv(paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/', files[3], sep=""))
        if(nrow(df)!=1){
          ## modify the csv format to calculate metadata more easily
          new_data <- rotate_data(df)
          ## Clean the data
          new_data <- clean_data_lhz(new_data, metadata)
          ## Update the metadata
          metadata_up <- update_metadata(new_data, metadata)
          if(length(unique(new_data$Cluster)) > 1){
            new_data <- new_data[which(is.na(new_data$Cluster) == FALSE),]
          }
          data_per_clusters <- as.data.frame(table(new_data$Cluster))
          colnames(data_per_clusters) <- c('cluster', 'nb_data')
          size_data <- nrow(new_data)
          new_data <- find_district(new_data, clusters)
          new_data$hh_weights <- merge(new_data, data_per_clusters, by.x='Cluster', by.y='cluster')$nb_data /size_data
          if(length(metadata_updated) != 0 & metadata$SurveyID %in% metadata_updated$SurveyID){
            metadata_updated[which(metadata_updated$SurveyID == metadata$SurveyID ), ] <- metadata_up
          }else{
            metadata_updated <- rbind(metadata_updated, metadata_up)
          }
          if(length(hh_obs) == 0){
            hh_obs <- new_data
          }else if(metadata$SurveyID %in% hh_obs$surveyId){
            hh_obs <- hh_obs[ - which(hh_obs$surveyId == metadata[which(!is.na(metadata$SurveyID)),]$SurveyID),]
            hh_obs <- rbind(hh_obs, new_data)
          }else{
            hh_obs <- rbind(hh_obs, new_data)
          }

        }
      }

      file.copy(paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/', files[2], sep = ""),
                paste(dir_path, 'lhz_surveys/individual_surveys/backup_folder/', sep=""))
      file.copy(paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/', files[3], sep = ""),
                paste(dir_path, 'lhz_surveys/individual_surveys/backup_folder/', sep=""))
      unlink(paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/', files[2], sep=""))
      unlink(paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/', files[3], sep=""))
      files <- list.files(paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/', sep=""))
    }

  }
  write.csv(metadata_updated, paste(dir_path, 'output/', 'metadata_updated.csv', sep=""), row.names = FALSE)
  write.csv(hh_obs, paste(dir_path, 'output/', 'data_smart_survey_updated.csv', sep=""), row.names = FALSE)
}

clean_data_lhz <- function(data, metadata){
  #' Select only eligeable data and calculate some others variables
  #' @param data .csv content
  #' @param metadata metadata content
  #' @param cluster to select the right ones and find district
  #' @return new .csv content updated with new variables and qulity score and district

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
  if(length(metadata[which(!is.na(metadata$Quality_Score)),]$Quality_Score) ==0){
    results$qualityScore <- 1
  }else{
    results$qualityScore <- metadata[which(!is.na(metadata$Quality_Score)),]$Quality_Score
  }

  return(results)
}

find_district <- function(results, clusters){
  #' Find district for each clusters
  #' @param results .csv file
  #' @param clusters different clusters where the survey has been made
  #' @return .csv file updated with district for each clusters

  results$stratum <- NA
  for(ind in 1:length(clusters$nb_cluster)){
    if(is.character(clusters$nb_cluster)){
      nb_clust <- lapply(strsplit(clusters$nb_cluster[ind], ','), as.integer)[[1]]
    }else{
      nb_clust <- clusters$nb_cluster[ind]
    }
    for(nb_c in nb_clust){
      results[which(results$Cluster == nb_c),]$stratum <- clusters$district[ind]
    }
  }
  return(results)
}
