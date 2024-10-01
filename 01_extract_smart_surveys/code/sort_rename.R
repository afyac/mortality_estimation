#--------------------------
#Sort files into different folders: lhz/admin2/issue and aggregate/individual
## GLOBAL VARIABLE: ERROR -- list of two: TRUE/FALSE and the error name
#--------------------------

sort_surveys <- function(dir_path){
  #' Sort SMART Surveys files in order to analyze them
  #' The first step is to add some .as files in the SMART_surveys_to_update folder
  #' @param dir_path Path where the code is
  #' @return Nothing but copy file in a backup folder and in the right folder and generate a logfile

  #Generate and create a log file  for the raw folder
  log_file <-  generate_log_file(dir_path)
  #List SMART Survey needed to be updated
  files <- list.files(paste(dir_path, 'SMART_surveys_to_update/', sep=""), pattern='\\.as$')
  ##Loop on the different .as file
  while(identical(files, character(0)) == FALSE){
    ERROR <<- c(FALSE, NA)
    file_name <- paste(dir_path, 'SMART_surveys_to_update/', files[1], sep="")
    new_name <- rename_file(file_name, files[1])
    ## If the SMART Surveys has already been processed before, we overwrite its information in the log file
    if(nrow(log_file) != 0){
      ind <- which(log_file$SMART_Survey_Name == files[1])
      if(length(ind) ==  0){
        ind <- nrow(log_file) +1
      }
    }

    ## Analyse the .as file and find its level (admin2/lhz) and others informations such as Date/Name etc..
    sort_info <- return_folder_info(files[1], file_name, new_name)
    folder <- sort_info[1] ## Folder to save the .as file
    info <- c(sort_info[2], sort_info[3], sort_info[4], sort_info[5]) ##Others informations
    if(nrow(log_file) == 0){ ##if logfile is empty
      log_file <- rbind(log_file, info)
      colnames(log_file) <- c('SMART_Survey_Name', 'CSV_Name', 'Last_update_time', 'Type of survey/issue')
    }else{
      log_file[ind, ] <- info
    }

    ## Copy the .as file in the right repository, make a copy in the backup folder and supress it from the initial folder
    file.copy(paste(dir_path, 'SMART_surveys_to_update/', files[1], sep = ""),
              paste(dir_path, folder, sep=""))
    file.copy(paste(dir_path, 'SMART_surveys_to_update/', files[1], sep = ""),
              paste(dir_path, 'backup_folder/', sep=""))
    unlink(paste(dir_path, 'SMART_surveys_to_update/', files[1], sep=""))
    files <- list.files(paste(dir_path, 'SMART_surveys_to_update/', sep=""))
  }

  if(nrow(log_file) != 0){
    ##Update the logfile
    write.table(log_file, file = paste(dir_path, 'backup_folder/log_file.csv', sep=""), append = FALSE,
                sep=',', col.names=TRUE, row.names = FALSE)
  }

}


generate_log_file <- function(dir_path){
  #'Generate a log file
  #'@param dirpath
  #'@return Log file DataFrame
  if(length(which(list.files(paste(dir_path, 'backup_folder/', sep="")) == 'log_file.csv')) == 0 ){ ##if logfile doesnt exist
    log_file <- data.frame()
  }else{
    log_file <- read.csv(paste(dir_path, 'backup_folder/log_file.csv', sep=""))
  }
  return(log_file)
}

return_folder_info <- function(file, file_name, new_name){
  #'Returns the location where the file is sorted and the necessary information to put in the log file.
  #' @params file Localisation of the file
  #' @param file_name Name of the old file
  #' @param new_name name use to rename the file SOM_Year_Month_admin2/lhz_district_lhztype
  #' @return folder and information to put in the logfile

  if(ERROR[1] == TRUE){
    ##If there is an error, the file is saved into the issue folder
    return(c('issue_surveys/', c(file, NA, format(Sys.time(), "%a %b %d %X %Y"), ERROR[2])))
  }else{
    if(is.na(ERROR[2]) == FALSE & ERROR[2] == 'lhz'){
      if(is_individual_survey(file_name)){
        return(c('lhz_surveys/individual_surveys/smart_surveys/',
                 c(file, new_name, format(Sys.time(), "%a %b %d %X %Y"), 'lhz')))
      }else{##it's an aggregate file
        return(c('lhz_surveys/aggregate_surveys/smart_surveys/',
                 c(file, new_name, format(Sys.time(), "%a %b %d %X %Y"), 'lhz')))
      }
    }else{
      if(is_individual_survey(file_name)){
        return(c('admin2_surveys/individual_surveys/smart_surveys/',
                 c(file, new_name, format(Sys.time(), "%a %b %d %X %Y"), 'individual')))
      }else{
        return(c('admin2_surveys/aggregate_surveys/smart_surveys/',
                 c(file, new_name, format(Sys.time(), "%a %b %d %X %Y"), 'aggregate')))
      }
    }
  }
}


is_individual_survey <- function(file_name){
  #' test if the survey is an individual or an aggregate one
  #' @param file name
  #' @return TRUE is the .as file is an individual one

  read_lines_file <- readLines(file_name)
  pos <- grep("\\Mortality_new:", read_lines_file, ignore.cas=TRUE, useBytes = TRUE)
  pos2 <- grep("\\Mor_individual:", read_lines_file, ignore.cas=TRUE, useBytes = TRUE)
  if((pos2 - pos) != 1){
    return(FALSE)
  }else{
    return(TRUE)
  }
}


rename_file <- function(file_name, file){
  #' Rename .as file with standard one SOM_Year_Month_admin2/lhz_district_lhztype
  #' @param path_name where the .as file is
  #' @param file name of the file
  #' @return new name

  read_lines_file <- readLines(file_name)
  # Select the part of the as file where the name is mentioned
  pos <- grep("\\Planning:", read_lines_file, ignore.cas=TRUE, useBytes = TRUE)
  pos2 <- grep("Cluster", read_lines_file, ignore.cas=TRUE)[2]
  # Concatenate those information with the one obtained in the file name
  fileName <- lapply(strsplit(stringr::str_split(file, stringr::fixed('.as'))[[1]], "_|/| "), tolower)[[1]]
  name <- lapply(strsplit(read_lines_file[(pos+1):(pos2-1)], "_|/| "), tolower)[[1]]
  name <- c(fileName, name)
  name <- name[name!=""]
  ## If file name is empty or no information is available in the .as file
  if(length(name) == 0){
    ERROR <<- c(TRUE, 'no Name')
  }
  # Select information on the area, date and type of lhz
  area <- find_stratum_level(dir_path, name)
  date <- find_date(read_lines_file)
  type <- find_type_lhz(name, area)
  return(paste('SOM_', date[1], '_', date[2], '_', area[1], '_', area[2], '_', type, sep = ""))
}

find_date <- function(read_lines_file){
  #' Find the date using .as information
  #' @param read_lines_file content of .as file
  #' @return Date - Day, Month, Year
  find_date <- read_lines_file[grep('/20', read_lines_file)]
  date <- sapply(strsplit(find_date, "\t"),"[[",1)
  if(length(date)== 0){
    ERROR <<- c(TRUE, 'Missing Date')
    return(c('NA', 'NA', 'NA'))
  }else{
    ##If the date avec the wrong format, try an other format
    if(is.na(min(lubridate::mdy(date)))){
      date_start <- min(lubridate::dmy(date))
      date <- strsplit(as.character(date_start), '-')[[1]]
      return(c(date[1], date[2], date[3]))
    }else{
      date_start <- min(lubridate::mdy(date))
      date <- strsplit(as.character(date_start), '-')[[1]]
      return(c(date[1], date[2], date[3]))
    }
  }
}

find_type_lhz <-function(name, area){
  #' Find type of lhz using name of the file or the corresponding lhz
  #' @param name name of the file
  #' @param area settlement/district/region/lhz name

  lzh <- c('idps', 'rural', 'agro', 'pasto', 'urban', 'idp', 'pastoral', 'riverine')
  for(elmt in name){
    if(any(stringi::stri_detect_fixed(lzh, elmt))){
      return(elmt)
    }
  }
  settlement_file$DISTRICT <- tolower(iconv(settlement_file$DISTRICT,"WINDOWS-1252","UTF-8"))
  settlement_file$SETTLEMENT <- tolower(iconv(settlement_file$SETTLEMENT,"WINDOWS-1252","UTF-8"))
  if(area[2] %in% settlement_file$SETTLEMENT){
    return(settlement_file[settlement_file$SETTLEMENT == area[2],]$LHZ[1])
  }
  return('NA')
}

find_stratum_level <- function(dir_path, name){
  #' Find the stratum level using .as information and settlement.csv file
  #' @param dir_path path where files are
  #' @param name name of the .as file
  #' @return Two elements -- type of level and the name of the settlement/district/region/lhz

  settlement_file$DISTRICT <- tolower(gsub("[[:punct:][:blank:]]+", "", iconv(settlement_file$DISTRICT,"WINDOWS-1252","UTF-8")))
  settlement_file$SETTLEMENT <- tolower(gsub("[[:punct:][:blank:]]+", "", iconv(settlement_file$SETTLEMENT,"WINDOWS-1252","UTF-8")))
  settlement_file$REGION <- tolower(gsub("[[:punct:][:blank:]]+", "", iconv(settlement_file$REGION,"WINDOWS-1252","UTF-8")))
  settlement_file$LHZ <- tolower(gsub("[[:punct:][:blank:]]+", "", iconv(settlement_file$LHZ,"WINDOWS-1252","UTF-8")))
  for(elmt in name){
    ##if the name of the file corresponds to a settlement or a district
    if(elmt %in% settlement_file$DISTRICT || elmt %in% settlement_file$SETTLEMENT){
      return(c('admin2', elmt))
    }else if(elmt %in% settlement_file$REGION || elmt %in% settlement_file$LHZ){
      ERROR <<- c(FALSE, 'lhz')
      return(c('lhz', elmt))
    }
  }
  ERROR <<- c(TRUE, 'stratum not find')
  return(c('NA', 'NA'))
}


