################################################################################
#                                                                              #
# Goal:            Clean and Update Health Datasets                            #
#                                                                              #                                           #
#                                                                              #
#                                                                              #
#                                                                              #
################################################################################


# MALARIA ----------------------------------------------------------------------

f_generate_malaria_who_dataset <- function(file_name_who, ts){
  
  # Import datasets
  mal_df1 <- rio::import( # old malaria data covering pre 2019
    file_name_who[1])
  mal_df2 <- rio::import( # newish malaria data covering post 2019
    file_name_who[2])
  
  # Cleaning the malaria dataset by processing two data frames,
  # and then joining them together
  malaria_data <- mal_df2 |>
    # select the relevant variables from mdf2
    dplyr::select(Region, District, Year, Month,
                  # rename columns for easier readability and consistency
                  malaria_cases = `Total Malaria Positive (RDT+Slide)`,
                  malaria_tests = `Total Malaria Tested (RDT+Slide)`
    ) |>
    # append rows from mdf with renamed variables to the existing data frame
    dplyr::bind_rows(
      mal_df1 |>
        # rename columns for easier readability and consistency
        dplyr::rename(
          malaria_cases = `Total Malaria Positive`,
          malaria_tests = `Total Malaria Tested`
        )
    ) |>
    # group the data by District, Year, and Month
    dplyr::group_by(District, Year, Month) |>
    # summarise the grouped data, calculating the sum of 
    # 'malaria_cases' and 'malaria_tests'
    dplyr::summarise(
      malaria_cases = sum(malaria_cases),
      malaria_tests = sum(malaria_tests), 
      .groups = 'drop'
    ) |>
    # rename columns for consistency
    dplyr::rename(year = Year, month = Month, district = District) |> 
    # replace the month names with their corresponding numeric values
    dplyr::mutate(
      month = ifelse(month %in% month.abb, 
                     match(month, month.abb), match(month, month.name)),
      month = as.integer(month)
    ) |> 
    # match district names to a standardised format using the admin_match function
    dplyr::mutate(district = mast::admin_match(district)) |>
    # regroup the data by district, year, and month
    dplyr::group_by(district, year, month) |>
    # summarise the grouped data, calculating the sum of 'malaria_cases'
    dplyr::summarise(malaria_cases = sum(malaria_cases), .groups = 'drop') 
  
  malaria_data <- merge(ts, malaria_data, 
                        by=c('district', 'year', 'month'), all.x=TRUE) |>
  # # Replace all NAs in specified columns with 0
  dplyr::mutate(malaria_cases = tidyr::replace_na(malaria_cases, 0)) |>
    # # Remove duplicates
    dplyr::distinct() |>
    # # Arrange by year, month, and then district
    dplyr::arrange(year, month, district) |> 
    dplyr::filter(!is.na(year) | !is.na(month))
  
 return(malaria_data)
  
}


f_generate_fsnau_malaria <- function(fsnau_data_forecasting, ts){
  malaria_fsnau <- fsnau_data_forecasting |> 
    dplyr::select(`Malaria Cases`, y, m, district) |>
    dplyr::rename(year = y, 
                  month = m, 
                  malaria_cases = `Malaria Cases`) |>
    dplyr::mutate(district = mast::admin_match(district, country='SOM'), 
                  date = as.Date(paste(paste(year, '-', month, "-01", sep="")))
    ) ## 74 districts -- 9768 number of lines
  
  malaria_fsnau <- aggregate(malaria_fsnau$malaria_cases, 
                             list(district=malaria_fsnau$district, 
                                  date = malaria_fsnau$date
                             ), FUN=sum) |>
    dplyr::rename(malaria_cases = x)
  
  
  ts_fsnau <- ts |> 
    dplyr::filter(
      date >= min(malaria_fsnau$date) & 
        date <= max(malaria_fsnau$date)) # 9768 rows
  
  # Merge all the comb and malaria data 
  malaria_fsnau <- merge(ts_fsnau, malaria_fsnau, 
                         by=c('district', 'date'), all.x=TRUE) # 9768 rows
  
  malaria_fsnau[is.na(malaria_fsnau)] <- 0
  return(malaria_fsnau)
}

# MEASLES ----------------------------------------------------------------------

f_generate_measles_who <- function(who_file, ts){
  # First Tab
  measles_who_2015_2018 <- rio::import(who_file, sheet='2015-2018') |>
    dplyr::select(Year, Region, District, month, Total) |>
    dplyr::mutate(District = tolower(District),
                  Region = tolower(Region),
                  month = match(month, month.abb),
                  date = as.Date(paste(paste(Year, '-', month, "-01", sep=""))),
                  District = mast::admin_match(District, country='SOM')
    ) |>
    dplyr::rename(year = Year,
                  district = District,
                  region = Region,
                  measles_cases = Total)
  
  measles_who_2015_2018 <- aggregate(measles_who_2015_2018$measles_cases,
                                     list(district=measles_who_2015_2018$district,
                                          date = measles_who_2015_2018$date
                                     ), FUN=sum) |>
    dplyr::rename(measles_cases = x) ## 3312 -- 73 districts
  
  
  ts_who_2015_2018 <- ts |>
    dplyr::filter(
      date >= min(measles_who_2015_2018$date) &
        date <= max(measles_who_2015_2018$date)) # 3404 rows
  
  measles_who_2015_2018 <- merge(ts_who_2015_2018, measles_who_2015_2018,
                                 by=c('district', 'date'), all.x=TRUE) # 3404 rows
  measles_who_2015_2018[is.na(measles_who_2015_2018)] <- 0
  
  ## Second Tab
  measles_who_2019_2022 <- rio::import(who_file, sheet='2019-Sept 2022')|>
    dplyr::select(Year, Region, District, `WEEK NUMBER`, `< 5 Years`, `> 5 Years`) |>
    dplyr::mutate(District = tolower(District),
                  Region = tolower(Region),
                  date = as.Date(lubridate::ymd("2019-01-01") +
                                   lubridate::weeks(as.integer(sapply(strsplit(`WEEK NUMBER`," "), `[`, 2))-1)),
                  District = mast::admin_match(District, country='SOM')
    ) |>
    dplyr::rename(year = Year,
                  district = District,
                  region = Region,
                  measles_cases_u5 = `< 5 Years`,
                  measles_cases_o_5 =`> 5 Years` ) |>
    dplyr::mutate(measles_cases = measles_cases_u5 + measles_cases_o_5) |>
    dplyr::select(-measles_cases_u5, -measles_cases_o_5)
  
  measles_who_2019_2022 <- aggregate(measles_who_2019_2022$measles_cases,
                                     list(district=measles_who_2019_2022$district,
                                          date = measles_who_2019_2022$date
                                     ), FUN=sum) |>
    dplyr::rename(measles_cases = x) ## 3312 -- 73 districts
  
  
  ts_who_2019_2022 <- ts |>
    dplyr::filter(
      date >= min(measles_who_2019_2022$date) &
        date <= max(measles_who_2019_2022$date)) # 3404 rows
  
  measles_who_2019_2022 <- merge(ts_who_2019_2022, measles_who_2019_2022,
                                 by=c('district', 'date'), all.x=TRUE) # 3404 rows
  
  measles_who_2019_2022[is.na(measles_who_2019_2022)] <- 0
  
  measles_who <- rbind(measles_who_2015_2018, measles_who_2019_2022)
  return(measles_who)
}

f_generate_measles_fsnau <- function(fsnau_data_forecasting, ts){
  # get FSNAU dataset
  measles_fsnau <- fsnau_data_forecasting |>
    dplyr::select(`Measles Cases`, y, m, district) |>
    dplyr::rename(year = y,
                  month = m,
                  measles_cases = `Measles Cases`) |>
    dplyr::mutate(district = mast::admin_match(district, country='SOM'),
                  date = as.Date(paste(paste(year, '-', month, "-01", sep="")))
    ) ## 74 districts -- 9768 number of lines

  measles_fsnau <- aggregate(measles_fsnau$measles_cases,
                             list(district=measles_fsnau$district,
                                  date = measles_fsnau$date
                             ), FUN=sum) |>
    dplyr::rename(measles_cases = x)

  # Merge all the comb and malaria data
  measles_fsnau <- merge(ts, measles_fsnau,
                         by=c('district', 'date'), all.x=TRUE) # 9768 rows

  measles_fsnau[is.na(measles_fsnau)] <- 0
  return(measles_fsnau)
}

f_generate_cholera_fsnau <- function(fsnau_data_forecasting, ts){
  ## Cholera
  # get FSNAU dataset
  cholera_fsnau <- fsnau_data_forecasting |>
    dplyr::select(`AWD/cholera cases`, y, m, district) |>
    dplyr::rename(year = y,
                  month = m,
                  cholera_cases = `AWD/cholera cases`) |>
    dplyr::mutate(district = mast::admin_match(district, country='SOM'),
                  date = as.Date(paste(paste(year, '-', month, "-01", sep="")))
    ) ## 74 districts -- 9768 number of lines

  cholera_fsnau <- aggregate(cholera_fsnau$cholera_cases,
                             list(district=cholera_fsnau$district,
                                  date = cholera_fsnau$date
                             ), FUN=sum) |>
    dplyr::rename(cholera_cases = x)


  ts_fsnau <- ts |>
    dplyr::filter(
      date >= min(cholera_fsnau$date) &
        date <= max(cholera_fsnau$date)) # 9768 rows

  # Merge all the comb and malaria data
  cholera_fsnau <- merge(ts_fsnau, cholera_fsnau,
                         by=c('district', 'date'), all.x=TRUE) # 9768 rows

  cholera_fsnau[is.na(cholera_fsnau)] <- 0
  return(cholera_fsnau)
}

f_generate_immunization_who <- function(imm_file_name, ts){
  ## BCG -- Vaccine against TB
  ## DTP --  Diphtheria
  ## IPV --  tetanus, diphtheria and polio
  ## Measles1 (dose 1) and Measle2 (dose 2)
  ## OPV -- polio
  ## Immunization -- Between 2018/2023
  immunization_who <- rio::import(imm_file_name) |>
    dplyr::select(Year, Month, DISTRICT, Antegin, Doses) |>
    dplyr::mutate(Month = match(Month, month.abb),
                  DISTRICT = mast::admin_match(DISTRICT, country='SOM'),
                  date = as.Date(paste(paste(Year, '-', Month, "-01", sep="")))
                  ) |>
    dplyr::rename(district = DISTRICT,
                  year = Year,
                  month = Month)

  immunization_who <- aggregate(immunization_who$Doses, list(Antegin = immunization_who$Antegin,
                                         date = immunization_who$date,
                                         district = immunization_who$district), FUN=sum) |>
    dplyr::rename(doses = x)

  ts_who <- ts |>
    dplyr::filter(
      date >= min(immunization_who$date) &
        date <= max(immunization_who$date)) # 9768 rows

  immunization_who <- merge(ts_who[, c('date', 'district')], immunization_who,
                            by=c('district', 'date'), all.x=TRUE) # 9768 rows

  immunization_who <- immunization_who |> tidyr::complete(Antegin, date, district) |>
    dplyr::filter(is.na(Antegin) == FALSE)

  immunization_who[is.na(immunization_who)] <- 0
  
  immunization_who <- merge(ts_who, immunization_who,
                            by=c('district', 'date'), all.x=TRUE) # 9768 rows
  
  return(immunization_who)
}




