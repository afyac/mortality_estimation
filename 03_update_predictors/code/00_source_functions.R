################################################################################
#                                                                              #
# Goal:            Functions used for updating predictors                      #
#                                                                              #
# Written:         Mo Yusuf                                                    #
# Date written:    2023-08-02                                                  #
#                                                                              #
#                                                                              #
#                                                                              #
################################################################################

# Function unit testing the joining variables in datasets ----------------------
f_test_joins <- function(df) {
  
  # Import comparison data and select the 'district' column
  data <-  import("00_overall_data/som_admin2.xlsx") |> 
    dplyr::select(district, region, pcode)
  
  # Check if all unique region in 'df' also exist in 'data'
  if (!all(unique(df$region) %in% data$region)) {
    cat(crayon::red("Error: Region names are not valid.\n"))
  } else {
    cat(crayon::green("Success! Region names valid 🥳 \n"))
  }
  
  # Check if all unique districts in 'df' also exist in 'data'
  if (!all(unique(df$district) %in% data$district)) {
    cat(crayon::red("Error: District names are not valid.\n"))
  } else {
    cat(crayon::green("Success! District names valid 🥳 \n"))
  }
  
  # Check if all unique pcode in 'df' also exist in 'data'
  if (!all(unique(df$pcode) %in% data$pcode)) {
    cat(crayon::red("Error: pcode names are not valid.\n"))
  } else {
    cat(crayon::green("Success! pcode names valid 🥳 \n"))
  }
  
  # filter dates of the df being tested 
  df <- df |> 
    dplyr::select(district, year, month) |> 
    dplyr::mutate(date = as.Date(paste0(year, "-", month, "-01"))) |> 
    dplyr::arrange(district, year, month) |>  ungroup()
  
  start_date <- min(df$date)
  end_date <- max(df$date)
  
  # Create a grid of all combinations of 'district', 'year' and 'month' 
  # in the given range
  expand_data <- expand_grid(
    district = unique(data$district),
    year = 1980:2024, 
    month = 1:12) |> 
    dplyr::mutate(date = as.Date(paste0(year, "-", month, "-01"))) |> 
    dplyr::filter(date >= start_date & date <= end_date) |>
    dplyr::arrange(district, year, month)
  
  # Check if all rows of 'df' also exist in 'expand_data'
  check_rows <- anti_join(df, expand_data, 
                          by = c("district", "year", "month")) 
  
  if (nrow(check_rows) != 0) {
    cat(crayon::red("Error: Not all combinations of year, month and district exist in your data\n"))
  } else {
    cat(crayon::green("Success! All relevant combinations exist 🥳 \n"))
  }
}


# Function to get FSNAU dashboard data ---------------------------------------
f_fsnau_dashboard <- function(years = 2013:2023) {
  months <- substr(month.name, 1, 3)
  datis <- list()
  count <- 1
  
  pb <- progress::progress_bar$new(
    format = "  Downloading FSNAU [:bar] :percent eta: :eta",
    total = length(years) * 12, clear = FALSE, width = 60
  )
  
  for (y in years) {
    for (m in seq_along(months)) {
      pb$tick()
      url <- paste0(
        "https://dashboard.fsnau.org/dashboard/index/01-",
        months[m], "-", y
      )
      file <- xml2::read_html(url)
      tables <- rvest::html_nodes(file, "table")
      table1 <- rvest::html_table(tables[1], fill = TRUE, na.strings = "")
      
      datis[[count]] <- table1[[1]] |>
        dplyr::rename(district = .data$Districts) |>
        dplyr::mutate(m = m, y = y) |>
        dplyr::select(.data$district, m, y, tidyselect::everything())
      
      count <- count + 1
    }
  }
  
  # bind and then convert chars to nums
  res <- do.call(rbind, datis)
  res <- res |>
    dplyr::mutate(dplyr::across(.cols = 5:dplyr::last_col(), mast::chr_to_num))
  return(res)
}

# Function to get excel tabs into one dataframe --------------------------------

f_excel_sheets_to_long_df <- function(excel_path) {
  # Step 1: Get sheet names
  sheet_names <- readxl::excel_sheets(excel_path)
  
  # Step 2 and 3: Read each sheet and bind them together
  all_data <- purrr::map_dfr(sheet_names, ~ {
    readxl::read_excel(excel_path, sheet = .x) %>%
      dplyr::mutate(month = .x)
  })
  
  return(all_data)
}

# Function to process markets data -------------------------------------------
f_process_markets_data <- function(path) {
  # Identify raw data file names
  mfiles <- list.files(path, full.names = TRUE)
  
  # Read each file into a list
  mdat <- lapply(mfiles, read.csv)
  
  # Check consistency of column names
  mnames <- lapply(mdat, colnames)
  lapply(mnames, function(x) {
    which(!unique(unlist(mnames)) %in% x)
  })
  mnames[[1]][83]
  mnames[[9]][33]
  
  # Fix inconsistent column names
  mdat[[9]] <- mdat[[9]] |> dplyr::rename(WaterDrum = WaterDrum200L)
  
  # Row bind all datasets
  mdat <- do.call(rbind, mdat)
  
  return(mdat)
}

# Function to generate time units ---------------------------------------
f_generate_time_units <- function(y_analysis_start, y_analysis_end,
                                  m_analysis_end, m_analysis_start) {
  
  burn_out_period <- 0
  burn_in_period <- 1
  time_unit <- seq(1, ((y_analysis_end + burn_out_period - y_analysis_start + 
                          burn_in_period) * 12 + m_analysis_end - 
                         m_analysis_start + 1), 1)
  
  time_unit <- as.data.frame(time_unit)
  
  time_unit$year <- as.numeric(
    format(
      as.Date(seq(as.Date(paste(y_analysis_start - 1, 
                                "-", m_analysis_start, 
                                "-01", sep = "")),
                  as.Date(paste(y_analysis_end, "-", m_analysis_end, "-01", 
                                sep = "")),
                  by = "months"
      )), "%Y"))
  
  time_unit$month <- as.numeric(
    format(as.Date(seq(as.Date(paste(y_analysis_start - 1, "-", 
                                     m_analysis_start, "-01", sep = "")),
                       as.Date(paste(y_analysis_end, "-", 
                                     m_analysis_end, "-01", sep = "")),
                       by = "months"
    )), "%m"))
  return(time_unit)
}
