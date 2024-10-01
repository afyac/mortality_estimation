#---------------------
## GENERATE CSV FILE METATADA FOR A SMART SURVEY FILE at LHZ level and INDIVIDUAL
#---------------------

lhz_survey_to_csv <- function(dir_path){
  #' Produce csv files and metadata one for files saved in lhz/individual folder
  #' @param dir_name
  #' @return nothing but generate csv file and update metadata

  files <- list.files(paste(dir_path, 'lhz_surveys/individual_surveys/smart_surveys/', sep=""), pattern='\\.as$')
  log_file <- read.csv(paste(dir_path, 'backup_folder/log_file.csv', sep=""))
  
  while(identical(files, character(0)) == FALSE){
    ##Select file one by one
    file_path <- paste(dir_path, 'lhz_surveys/individual_surveys/smart_surveys/', files[1], sep="")
    new_name <- log_file[match(files[1], log_file$SMART_Survey_Name),]$CSV_Name
    lhz <- strsplit(new_name, '_')[[1]][5]
    read_lines_file <- readLines(file_path)
    #generate csv
    generate_csv_lhz(read_lines_file, new_name, settlement_file, lhz)
    #generate metadata
    generate_metadata(read_lines_file, 0, new_name, files[1], 'lhz_surveys/individual_surveys')
    unlink(paste(dir_path, 'lhz_surveys/individual_surveys/smart_surveys/', files[1], sep=""))
    files <- list.files(paste(dir_path, 'lhz_surveys/individual_surveys/smart_surveys/', sep=""))
  }
}

generate_csv_lhz <- function(read_lines_file, new_name, settlement_file, lhz){
  #' Generate csv content using .as file
  #' @param read_lines_files .as content
  #' @param new_name name of the file
  #' @param settlement_file file containing link between settlement and district needed to select clusters
  #' @return nothing but generate a csv file


  clusters_data <- return_cluster_data(read_lines_file, settlement_file, lhz)
  if(length(clusters_data) != 0){
    pos <- grep("\\Mor_individual:", read_lines_file, ignore.cas=TRUE, useBytes = TRUE)
    pos2 <- grep("\\Mor_individual_options:", read_lines_file, ignore.cas=TRUE, useBytes = TRUE)
    data <- read_lines_file[(pos+1):(pos2-1)]

    data<- strsplit(data, split = "\t")
    data <- lapply(data, `length<-`, max(lengths(data)))
    data <- as.data.frame(do.call(rbind, data))
    data[is.na(data)] <- ""
    colnames(data) <- data[1,]
    data <- data[-1, ]
    data <- data[rowSums(is.na(data)) != ncol(data), ]

    # clusters <- unlist(strsplit(clusters_data$nb_cluster, ','))
    # data <- data[which(data$Cluster %in% clusters),]
    if(nrow(data)!=0){
      write.csv(data, paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/', new_name, '.csv', sep=""), row.names = FALSE)
      write.csv(clusters_data, paste(dir_path, 'lhz_surveys/individual_surveys/csv_files/', new_name, '_clusters.csv', sep=""), row.names = FALSE)
    }
  }
}

return_cluster_data <- function(read_lines_file, settlement_file, lhz){
  #' return only data where the cluster is linked with a known settlement and thus to a known district
  #' @param read_lines_file .as content
  #' @param  settlement_file file containing link with settlement and district
  #' @return nothing but generate csv file and update metadata

  ## Select the different cluster number and the name of the settlement
  pos <- grep("Cluster", read_lines_file, ignore.cas=TRUE)[2]
  pos2 <- grep("\\Training_new:", read_lines_file, ignore.cas=TRUE, useBytes = TRUE)
  clusters_data <- read_lines_file[(pos+10):(pos2-1)]
  clusters_data<- strsplit(clusters_data, split = "\t")
  clusters_data <- lapply(clusters_data, `length<-`, max(lengths(clusters_data)))
  clusters_data <- as.data.frame(do.call(rbind, clusters_data))
  if(nrow(clusters_data) < 3){
    return(data.frame())
  }
  colnames(clusters_data) <- c('settlements', 'pop', 'nb_cluster')
  clusters_data <- dplyr::select(clusters_data, c('settlements', 'nb_cluster'))
  clusters_data[which(clusters_data$nb_cluster == ""),]$nb_cluster <- NA
  clusters_data <- tidyr::drop_na(clusters_data)
  clusters_data$settlements <- tolower(clusters_data$settlements)
  clusters_data$settlements <- stringr::str_replace_all(clusters_data$settlements , "[[:punct:]]", "")
  clusters_data$settlements <- stringr::str_replace_all(clusters_data$settlements , "[[:space:]]", "")
  clusters_data$settlements <- stringr::str_replace_all(clusters_data$settlements , "[[:digit:]]+", "")

  settlement_file$SETTLEMENT <- tolower(gsub("[[:punct:][:blank:]]+", "", iconv(settlement_file$SETTLEMENT,"WINDOWS-1252","UTF-8")))
  settlement_file$DISTRICT <- tolower(gsub("[[:punct:][:blank:]]+", "", iconv(settlement_file$DISTRICT,"WINDOWS-1252","UTF-8")))
  settlement_file$LHZ <- tolower(gsub("[[:punct:][:blank:]]+", "", iconv(settlement_file$LHZ,"WINDOWS-1252","UTF-8")))
  lhz_code <- settlement_file['LHZ_CODE'][which(settlement_file$LHZ == lhz), ][1]

  district <-  c()
  for(name in clusters_data$settlements){
    ind <- which(settlement_file$SETTLEMENT == name & settlement_file$LHZ_CODE == lhz_code)
    if(length(ind) != 0){
      district <- rbind(district, c(settlement_file['DISTRICT'][ind[1],],
                                    settlement_file['LHZ'][ind[1],]))
    }else{
      district <- rbind(district, c(NA, NA))
    }
  }
  clusters_data <- cbind(clusters_data, district)
  if(nrow(clusters_data) ==0){return(data.frame())}
  colnames(clusters_data) <- c('settlements', 'nb_cluster', 'district', 'lhz')

  clusters_data <- tidyr::drop_na(clusters_data)
  return(clusters_data)
}



