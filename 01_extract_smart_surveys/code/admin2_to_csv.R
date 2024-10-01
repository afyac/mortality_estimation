#----------------------
#Code to generate metadata and csv files for individual files -- ADMIN2 LEVEL
#----------------------
#---------------------
## GENERATE CSV FILE METATADA FOR A SMART SURVEY FILE at ADMIN2 level and INDIVIDUAL
#---------------------

admin2_survey_to_csv <- function(dir_name){
  #' Produce csv files and metadata one for files saved in admin2_individual folder
  #' @param dir_name
  #' @return nothing but generate csv file and update metadata

  log_file <- read.csv(paste(dir_path, 'backup_folder/log_file.csv', sep=""))
  ##Upload files to be transform into csv
  files <- list.files(paste(dir_path, 'admin2_surveys/individual_surveys/smart_surveys/', sep=""), 
                      pattern='\\.as$')
  while(identical(files, character(0)) == FALSE){
    file_name_path <- paste(dir_path, 'admin2_surveys/individual_surveys/smart_surveys/', files[1], sep="")
    new_name <- log_file[match(files[1], log_file$SMART_Survey_Name),]$CSV_Name
    read_lines_file <- readLines(file_name_path)
    #generate csv
    nb_lines <- generate_csv_individual(read_lines_file, new_name)
    #generate metadata
    generate_metadata(read_lines_file, nb_lines, new_name, files[1], 'admin2_surveys/individual_surveys')
    unlink(paste(dir_path, 'admin2_surveys/individual_surveys/smart_surveys/', files[1], sep=""))
    files <- list.files(paste(dir_path, 'admin2_surveys/individual_surveys/smart_surveys/', sep=""))
  }
}

generate_csv_individual <- function(read_lines_file, new_name){
  #' Generate csv content using .as file
  #' @param read_lines_files .as content
  #' @param new_name name of the file
  #' @return nothing but generate a csv file

  ##Select the part of the file where the csv data are
  pos <- grep("\\Mor_individual:", read_lines_file, ignore.cas=TRUE, useBytes = TRUE)
  pos2 <- grep("\\Mor_individual_options:", read_lines_file, ignore.cas=TRUE, useBytes = TRUE)
  df <- read_lines_file[(pos+1):(pos2-3)]
  df<- strsplit(df, split = "\t")
  df <- lapply(df, `length<-`, max(lengths(df)))
  df <- as.data.frame(do.call(rbind, df))
  ## If NA = no information
  df[is.na(df)] <- ""
  colnames(df) <- df[1,]
  df <- df[-1, ]
  if(nrow(df) > 1){
    write.csv(df, paste(dir_path, 'admin2_surveys/individual_surveys/csv_files/', 
                        new_name, '.csv', sep=""), row.names = FALSE)
  }
  return(nrow(df))
}

