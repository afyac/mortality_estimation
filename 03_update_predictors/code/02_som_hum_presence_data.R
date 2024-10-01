# ACLED DATA
f_generate_acled_data <- function(file_acled, ts){
  # Read and process the data
  acled_data <- 
    rio::import(file_acled) |>
    # get unique events only
    dplyr::distinct(event_id_cnty, .keep_all = TRUE) |>
    dplyr::mutate(
      # parse dates
      event_date = lubridate::dmy(event_date),
      year = lubridate::year(event_date),
      month = lubridate::month(event_date),
      # create a new column acled_event and fill it with 1s
      acled_event = 1
    ) |>
    # rename and dplyr::select cols
    dplyr::select(
      district = admin2,
      year, month,
      acled_event_type = sub_event_type,
      acled_fatalities = fatalities,
      acled_event
    ) 
  
  acled_data <- aggregate(cbind(acled_data$acled_fatalities, acled_data$acled_event), 
                           by=list(district = acled_data$district, 
                                   year = acled_data$year,
                                   month= acled_data$month), 
                           FUN=sum)
  
  colnames(acled_data) <- c('district', 'year', 'month', 
                            'acled_fatalities', 'acled_event')
  
  acled_data <- merge(ts, acled_data,
                         by=c('district', 'year', 'month'), all.x=TRUE)
  
  acled_data[is.na(acled_data)] <- 0
  
  return(acled_data)
}

# ATTACKS on AID WORKERS
f_generate_aid_workers <- function(security_file, ts){
  attacks_aid_workers <- 
    rio::import(security_file) |>
    tibble::as_tibble() |>
    # remove top row as it has characters not related to the data
    dplyr::slice(-1) |>
    # drop duplicate rows
    dplyr::distinct(`Incident ID`, .keep_all = TRUE) |> 
    # clean the col names
    janitor::clean_names() |>
    # select relevant columns and rename for consistency
    dplyr::select(
      district, year, month,
      aidworkers_killed = total_killed,
      aidworkers_wounded = total_affected
    ) |>
    # replace empty space with NA
    dplyr::mutate_all(~ dplyr::na_if(., "")) |>
    # fill down the everything column
    # (its excel format with non-repeated distroct names)
    tidyr::fill(everything(), .direction = "down") |>
    # ensure variables arw in numeric format
    dplyr::mutate(
      aidworkers_killed = as.numeric(aidworkers_killed),
      aidworkers_wounded = as.numeric(aidworkers_wounded),
      year = as.numeric(year),
      month = as.numeric(month)
    ) |> tidyr::drop_na(district) |>
    # ensure district names are correct
    dplyr::mutate(district = mast::admin_match(district)) |>
    # get sum of all events and fatalities
    dplyr::group_by(district, year, month) |>
    dplyr::summarise(
      aidworkers_killed = sum(aidworkers_killed),
      aidworkers_wounded = sum(aidworkers_wounded), .groups = "drop"
    ) |>
    tidyr::drop_na() |> 
    tidyr::complete(year, month, district,
                    fill = list(
                      aidworkers_wounded = 0,
                      aidworkers_killed = 0
                    )
    ) |>
    # Replace all NAs with 0
    dplyr::mutate_at(c(
      "aidworkers_wounded",
      "aidworkers_killed"
    ), ~ tidyr::replace_na(., 0)) |>
    # sort the data by year, month, and district
    dplyr::arrange(district, year, month) |>
    dplyr::mutate(date = as.Date(paste(paste(year, '-', month, "-01", sep=""))))
  
  ts <- ts |>
    dplyr::filter(
      date >= min(attacks_aid_workers$date) &
        date <= max(attacks_aid_workers$date))
  
  attacks_aid_workers <- merge(ts, attacks_aid_workers,
                      by=c('district', 'date', 'year', 'month'), all.x=TRUE)
  
  attacks_aid_workers[is.na(attacks_aid_workers)] <- 0
  return(attacks_aid_workers)
}

