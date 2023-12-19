#---------------------
## GENERATE METATADA FOR A SMART SURVEY FILE
#---------------------


generate_metadata <- function(read_lines_file, nb_line_cv, new_name, old_name, folder_name){
  #' Generate the metadata link with a SMART Survey
  #' @param read_lines_file content of the .as file
  #' @param new_name
  #' @param old_name initial name of the SMART Survey file
  #' @param folder_name where the .as file is saved -- to update the right metadata file
  #' @return Nothing but update the metadata file

  survey_id <- new_name
  new_name_info <- strsplit(new_name, '_')[[1]]
  old_name <- old_name
  agency <- 'FSNAU'
  if(is.na(match(new_name_info[4],'lhz'))){
    place <- find_place_new(new_name_info[5], new_name_info[6])
  }else{
    place <- c(NA, NA, new_name_info[5])
  }

  find_date <- read_lines_file[grep('/20', read_lines_file)]
  date <- sapply(strsplit(find_date, "\t"),"[[",1)
  date <- as.character(stringi::stri_remove_empty(date, na_empty = FALSE))
  date <- date[!grepl('#', date, fixed = TRUE)]
  if(nb_line_cv != 0){
    date <- date[(length(date)-nb_line_cv+1):length(date)]
  }
  date <- parsedate::parse_date(date)
  date_start <- min(date)
  date_end <- max(date)
  recall_days <- find_recall_days(read_lines_file)
  ##Plausibility score
  quality_score <- calcul_quality_score(read_lines_file)
  ##Code for injury/unknown or violent death
  codes <- find_codes(read_lines_file)
  unk_code <- codes[1]
  inj_code <- codes[2]
  viol_code <- codes[3]
  metadata <- c(survey_id, old_name, agency, place, as.character(date_start), as.character(date_end), recall_days, quality_score,
                unk_code, inj_code, viol_code)
  files <- list.files(paste(dir_path, folder_name, '/csv_files/', sep=""))
  ##Load metadata file
  if('metadata.csv' %in% list.files(paste(dir_path, folder_name, '/csv_files/', sep="")) == FALSE){
    metadata_file <- data.frame()
  }else{
    metadata_file <- read.csv(paste(dir_path, folder_name, '/csv_files/metadata.csv', sep=""))
  }
  ## if the metadata file is empty -- Complete it
  if(length(metadata_file) == 0){
    metadata_file <- rbind(metadata_file, metadata)
    colnames(metadata_file) <- c('SurveyID', 'Old_Name', 'Agency', 'District', 'Region', 'LHZ',
                                 'Start_Date', 'End_Date', 'Recall_Days', 'Quality_Score',
                                 'UnkCode', 'InjCode', 'ViolCode')
    write.table(metadata_file, file = paste(dir_path, folder_name, '/csv_files/metadata.csv', sep=""), append = FALSE,
                sep=',', col.names=TRUE, row.names = FALSE)

  }else{
    ##If the survey has not been studied previously
    if(length(which(metadata_file$SurveyID == new_name)) == 0){
      metadata_file <- rbind(metadata_file, metadata)
    }else{
      metadata_file[which(metadata_file$SurveyID == new_name), ] <- metadata
    }
    write.table(metadata_file, file = paste(dir_path, folder_name, '/csv_files/metadata.csv', sep=""), append = FALSE,
                sep=',', col.names=TRUE, row.names = FALSE)
  }

}

find_codes <- function(read_lines_file){
  #' Find code to know the type of death if known
  #' @param read_lines_file content of the .as file
  #' @return 3 integers for three different types of death: unknow, injury and violence

  pos <- grep('Mor_individual_options', read_lines_file, ignore.case=TRUE)
  pos_unk <- grep('Unknown', read_lines_file, ignore.case = TRUE)
  pos_inj <- grep('Injury', read_lines_file, ignore.case = TRUE)
  pos_viol <- grep('Violence', read_lines_file, ignore.case = TRUE)
  if(length(pos_unk) != 0){
    score_unk <- pos_unk - pos
  }else{
    score_unk <- 0
  }
  if(length(pos_inj) != 0){
    score_inj <- pos_inj - pos
  }else{
    score_inj <- 0
  }
  if(length(pos_viol) != 0){
    score_viol <- pos_viol - pos
  }else{
    score_viol <- 0
  }
  return(c(score_unk, score_inj, score_viol))
}

find_recall_days <- function(read_lines_file){
  #' Find the recall days of the SMART Surveys
  #' @param read_lines_file content of the .as file
  #' @return 1 integer

  pos <- grep("Cluster", read_lines_file, ignore.cas=TRUE)[2]
  return(strsplit(read_lines_file[pos+8], '\t')[[1]][1])
}

find_place_new<- function(name, lhz){
  #' Find the district name and LHZ of the studied .as file
  #' @param name of the file
  #' @param lhz area
  #' @return list containing the district, region and LHZ name

  settlement_file$DISTRICT <- tolower(iconv(settlement_file$DISTRICT,"WINDOWS-1252","UTF-8"))
  settlement_file$SETTLEMENT <- tolower(iconv(settlement_file$SETTLEMENT,"WINDOWS-1252","UTF-8"))
  if(name %in% settlement_file$SETTLEMENT){
    return(c(settlement_file[which(settlement_file$SETTLEMENT == name),]$DISTRICT,
             settlement_file[which(settlement_file$SETTLEMENT == name),]$REGION,
             lhz))
  }else{
    return(c(name,
             settlement_file[which(settlement_file$DISTRICT == name),]$REGION[1],
             lhz))
  }
}


