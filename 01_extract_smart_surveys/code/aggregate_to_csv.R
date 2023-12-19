#---------------------
## GENERATE CSV FILE METATADA FOR A SMART SURVEY FILE at ADMIN2 level and AGGREGATE
#---------------------

aggregate_survey_to_csv<- function(dir_name){
  #' Produce csv files and metadata one for files saved in admin2/aggregate folder
  #' @param dir_name
  #' @return nothing but generate csv file and update metadata

  ##Upload files to be transform into csv
  files <- list.files(paste(dir_path, 'admin2_surveys/aggregate_surveys/smart_surveys/', sep=""), pattern='\\.as$')
  log_file <- read.csv(paste(dir_path, 'backup_folder/log_file.csv', sep=""))
  while(identical(files, character(0)) == FALSE){
    ##Select file one by one
    file_name_path <- paste(dir_path, 'admin2_surveys/aggregate_surveys/smart_surveys/', files[1], sep="")
    new_name <- log_file[match(files[1], log_file$SMART_Survey_Name),]$CSV_Name
    read_lines_file <- readLines(file_name_path)
    #generate csv
    generate_csv_aggregate(read_lines_file, new_name)
    generate_metadata(read_lines_file, 0, new_name, files[1], 'admin2_surveys/aggregate_surveys')
    unlink(paste(dir_path, 'admin2_surveys/aggregate_surveys/smart_surveys/', files[1], sep=""))
    files <- list.files(paste(dir_path, 'admin2_surveys/aggregate_surveys/smart_surveys/', sep=""))
  }
}


generate_csv_aggregate <- function(read_lines_file, new_name){
  #' Generate csv content using .as file
  #' @param read_lines_files .as content
  #' @param new_name name of the file
  #' @return nothing but generate a csv file

  ##Select the part of the file where the csv data are
  pos <- grep("\\Mortality_new:", read_lines_file, ignore.cas=TRUE, useBytes = TRUE)
  pos2 <- grep("\\Mor_individual:", read_lines_file, ignore.cas=TRUE, useBytes = TRUE)
  df <- read_lines_file[(pos+1):(pos2-1)]
  df<- strsplit(df, split = "\t")
  df <- lapply(df, `length<-`, max(lengths(df)))
  df <- as.data.frame(do.call(rbind, df))
  df[is.na(df)] <- ""
  colnames(df) <- c('HH', 'Cluster',	'HH_members_Total', 'HH_members_u5', 'joined_HH_total', 'joined_HH_u5', 'left_HH_total', 'left_HH_u5', 'Births', 'Deaths_Total', 'Deaths_u5')
  write.csv(df, paste(dir_path, 'admin2_surveys/aggregate_surveys/csv_files/', new_name, '.csv', sep=""), row.names = FALSE)
}
