################################################################################
#                                                                              #
# Goal:            Update predictors                                           #
#                                                                              #
# Written:         Mo Yusuf                                                    #
# Date written:    2023-08-02                                                  #
#                                                                              #
#                                                                              #
#                                                                              #
################################################################################

# Prepare ----------------------------------------------------------------------

# get functions 
source("03_update_predictors/code/00_source_functions.R")

# get district names
district_lables <- rio::import("00_overall_data/som_admin2.xlsx") |>
  dplyr::select(region, district, pcode)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# ACLED #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                          Stops at:  June 2023                                #
#                          Source: ACLED                                       #
#                          Retrieval method: Manual                            #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Read and process the data
acled_data <- 
  import("03_update_predictors/data/acled_data_2023.csv") |>
  # get unique events only
  dplyr::distinct(event_id_cnty, .keep_all = TRUE) |>
  dplyr::mutate(
    # parse dates
    event_date = dmy(event_date),
    year = year(event_date),
    month = month(event_date),
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
  ) |> 
  # get sum of all events and fatilities
  dplyr::group_by(district, year, month) |>
  dplyr::summarise(
    acled_fatalities = sum(acled_fatalities),
    acled_event = sum(acled_event), .groups = "drop"
  ) |>
  # Complete the data frame by generating all combinations of year, month, and
  # district, filling non-existing entries with 0 in the specified column
  tidyr::complete(year, month, district, fill = list(
    acled_fatalities = 0,
    acled_event = 0
  )) |>
  # ensure district names are correct
  dplyr::mutate(district = mast::admin_match(district)) |>
  # remove duplicates
  dplyr::distinct() |>
  dplyr::left_join(district_lables, by = "district") |>
  dplyr::select(district, region, pcode, year, month, everything()) |> 
  dplyr::group_by(year) 

# check if all combinations  year, month and district exist in data
f_test_joins(acled_data)

# Write the processed data to a csv file
saveRDS(acled_data, "03_update_predictors/output/som_conflict_acled.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~# MALARIA, MEASLES & CHOLERA #~~~~~~~~~~~~~~~~~~~~~~#
#                          Stops at:  June 2023                                #
#                          Source: FSNAU                                       #
#                          Retrieval method: Automated                         #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# get FSNAU dataset
fsnau_data_forecasting <- f_fsnau_dashboard(2000:2023)

fsnau_data <- fsnau_data_forecasting |> filter(y >= 2013)

diseases_data <- 
  fsnau_data |>
  # remove duplicates
  dplyr::distinct() |>
  # select relevant columns and rename
  dplyr::select(
    district,
    year = y, month = m,
    malaria_cases = `Malaria Cases`,
    measles_cases = `Measles Cases`,
    cholera_cases = `AWD/cholera cases`
  ) |>
  # Complete the data frame by generating all combinations of year, month, and
  # district, filling non-existing entries with 0 in the specified column
  tidyr::complete(year, month, district,
                  fill = list(
                    malaria_cases = 0,
                    measles_cases = 0,
                    cholera_cases = 0
                  )
  ) |>
  # Replace all NAs with 0
  dplyr::mutate_at(c(
    "malaria_cases",
    "measles_cases",
    "cholera_cases"
  ), ~ replace_na(., 0)) |>
  # ensure district names are correct
  dplyr::mutate(district = mast::admin_match(district)) |>
  # sort the data by year, month, and district
  dplyr::arrange(district, year, month) |>
  dplyr::left_join(district_lables, by = "district") |>
  dplyr::select(district, region, pcode, year, month, everything())

# check if all combinations  year, month and district exist in data
f_test_joins(diseases_data)

# Write the processed data to a csv file
saveRDS(diseases_data, 
        "03_update_predictors/output/som_malaria_measles_cholera.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# Malaria #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                          Stops at:  July 2023                                #
#                          Source: MOH/WHO                                     #
#                          Retrieval method: Manual                            #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Import datasets
mal_df1 <- import( # old malaria data covering pre 2019
  "03_update_predictors/data/Malaria Data 2014-2018.xlsx")
mal_df2 <- import( # newish malaria data covering post 2019
  "03_update_predictors/data/Malaria_2019_sep_2023.xlsx")

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
  dplyr::summarise(malaria_cases = sum(malaria_cases), .groups = 'drop') |> 
  # # join region and district names
  dplyr::full_join(district_lables,
                   by = "district") |> 
  # # Complete the data frame by generating all combinations of year, month, and
  # # district, filling non-existing entries with 0 in the specified column
  complete(year = seq(2015, 2023),
           month = seq(1, 12),
           nesting(district, region, pcode)) |> 
  # # Replace all NAs in specified columns with 0
  dplyr::mutate(malaria_cases = replace_na(malaria_cases, 0)) %>%
  # # Remove duplicates
  dplyr::distinct() %>%
  # # Arrange by year, month, and then district
  dplyr::arrange(year, month, district) |> 
  filter(!is.na(year) | !is.na(month))


# check if all combinations  year, month and district exist in data
f_test_joins(malaria_data)

# Write the processed data to a csv file
saveRDS(malaria_data, "03_update_predictors/output/som_malaria_data.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# Measles #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                          Stops at:  July 2023                                #
#                          Source: MOH/WHO                                     #
#                          Retrieval method: Manual                            #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Import datasets sheet 1
measles_df1 <- import(
  "03_update_predictors/data/2015- Sept 2022 Measles Cases.xlsx", sheet = 1)
# Import datasets sheet 1
measles_df2 <- import(
  "03_update_predictors/data/2015- Sept 2022 Measles Cases.xlsx", sheet = 2)

# Set the month column in the md2 dataset
measles_df2 <- measles_df2 %>%
  mutate(
    month = lubridate::month(
      lubridate::ymd(paste0(Year, "-01-01")) +
        lubridate::weeks(as.integer(gsub("Week ", "", `WEEK NUMBER`)) - 1)
    )
  )

# get get cholera and measles dataset for 2023
measles_cholera_2023 <- f_excel_sheets_to_long_df(
  "03_update_predictors/data/Somalia health indicators data for Jan-Aug 2023.xlsx"
) |> 
  # Fill NA with the previous value
  fill(Region, .direction = "down") |> 
  # combine measles and choelra cases
  dplyr::mutate(
    cholera_cases = `Acute Diarhoea<5` + `Acute Diarrhoea>5`,
    measles_cases = `Measles<5` + `Measles>5`,
    year = 2023,
    # fix months 
    month = stringr::str_extract(month, regex(paste(month.abb, collapse = "|"), 
                                              ignore_case = TRUE)),
    month = ifelse(month %in% month.abb, match(month, month.abb), month)
  )  |> 
  dplyr::select(
    district = Districts, region = Region, year, month, cholera_cases, measles_cases) |> 
  # remove rows that are total
  filter(if_any(district, ~ !grepl("Total|TOTALS", .))) |> 
  # clean up region names
  mutate(region = trimws(region)) |> 
  mutate(region = str_replace(region, "Region", ""))   |> 
  mutate(
    region = case_when(
      str_detect(region, "Ayn") ~ "Togdher",
      str_detect(region, "Gardafuu|Karkar|Karkaar") ~ "Bari",
      str_detect(region, 'Galbeed') ~ 'Woqooyi Galbeed',
      str_detect(region, 'Sahil|South Mudug') ~ 'Mudug',
      TRUE ~ region
    ))  |> 
  mutate(
    district = trimws(district),
    district = str_replace(district, "District", ""),
    district = case_when(
      # district == "Laasqoray" ~ "Laasqoray",
      str_detect(district, "Hudun") ~ "Xudun",
      str_detect(district, "Bocame|Tukaraq") ~ "Laas Caanood",
      str_detect(district, "Hudun") ~ 'Saakow',
      str_detect(district, "Baligubadle") ~ 'Hargeysa',
      str_detect(district, "Bargal") ~ 'Iskushuban',
      str_detect(district, "UFEYN") ~ 'Bossaso',
      str_detect(district, "Xalin") ~ 'Taleex',
      str_detect(district, "Dangorayo") ~ 'Garoowe',
      str_detect(district, "Baran") ~ 'Laasqoray',
      str_detect(district, "Garadag") ~ 'Ceel Afweyn',
      str_detect(district, "Bardera") ~ 'Baardheere',
      str_detect(district, "Salagle") ~ 'Saakow',
      str_detect(district, "Bondheere|Bondere")  ~ "Banadir",
      str_detect(district, "Haradheere") ~ 'Xarardheere',
      TRUE ~ district)) |> 
  dplyr::mutate(
    district = mast::admin_match(district)) |> 
  # regroup the data by district, year, and month
  dplyr::group_by(district, year, month) |>
  # summarise the grouped data, calculating the sum of 'malaria_cases'
  dplyr::summarise(measles_cases = sum(measles_cases),
                   cholera_cases = sum(cholera_cases), 
                   .groups = 'drop') 

# Cleaning the measles dataset by processing two data frames,
measles_data <- measles_df1 %>% dplyr::select(District, Year, month, Total) %>%
  # rename columns for easier readability and consistency
  dplyr::rename(district = District, year = Year, month = month, 
                measles_cases = Total) %>%
  dplyr::mutate(
    month = ifelse(month %in% month.abb, 
                   match(month, month.abb), match(month, month.name)),
    month = as.integer(month)
  ) |> 
  # append another dataframe from
  dplyr::bind_rows(measles_df2 %>%
                     dplyr::mutate(Total = `< 5 Years` + `> 5 Years`) %>%
                     # select and rename District, Year, month, Total columns
                     dplyr::select(district = District, year = Year, 
                                   month, measles_cases = Total)) %>%
  # bind rows with the new datasets for 2023
  dplyr::bind_rows(measles_cholera_2023 |> select(-cholera_cases)) |> 
  # sort the rows based on district, year, and month
  dplyr::arrange(year, month, district) %>%
  # replace district names with standardised names using admin_match function
  dplyr::mutate(district = mast::admin_match(district)) %>%
  # group the rows by district, year, and month
  dplyr::group_by(district, year, month) %>%
  # calculate the sum of measles cases
  dplyr::summarise(measles_cases = sum(measles_cases),
                   .groups = 'drop') %>%
  # join region and district names
  dplyr::left_join(district_lables, by = "district") |> 
  # Complete the data frame by generating all combinations of year, month, and
  # district, filling non-existing entries with 0 in the specified column
  complete(year = seq(2015, 2023), 
           month = seq(1, 12), 
           nesting(district, region, pcode)) %>%
  # Replace all NAs in specified columns with 0
  dplyr::mutate(measles_cases = replace_na(measles_cases, 0)) %>%
  # Remove duplicates
  dplyr::distinct() %>%
  # Arrange by year, month, and then district
  dplyr::arrange(year, month, district)

# check if all combinations  year, month and district exist in data
f_test_joins(measles_data)

# Write the processed data to a csv file
saveRDS(measles_data, "03_update_predictors/output/som_measles_data.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~# ATTACK ON AID-WORKERS #~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                          Stops at:  May 2023                              #
#                          Source: ??                                          #
#                          Retrieval method: Manual.                           #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

attacks_aid_workers <- 
  import("03_update_predictors/data/security_incidents_2023.csv") |>
  as_tibble() |>
  # remove top row as it has characters not related to the data
  slice(-1) |>
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
  dplyr::mutate_all(~ na_if(., "")) |>
  # fill down the everything column
  # (its excel format with non-repeated distroct names)
  tidyr::fill(everything(), .direction = "down") |>
  # ensure variables arw in numeric format
  dplyr::mutate(
    aidworkers_killed = as.numeric(aidworkers_killed),
    aidworkers_wounded = as.numeric(aidworkers_wounded),
    year = as.numeric(year),
    month = as.numeric(month)
  ) |> drop_na(district) |>
  # ensure district names are correct
  dplyr::mutate(district = mast::admin_match(district)) |>
  # add all district names that dont exist in dataset
  dplyr::full_join(district_lables |> dplyr::select(district), 
                   by = "district") |>
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
  ), ~ replace_na(., 0)) |>
  # sort the data by year, month, and district
  dplyr::arrange(district, year, month) |>
  dplyr::left_join(district_lables, by = "district") |>
  dplyr::select(district, region, pcode, year, month, everything())

# check if all combinations  year, month and district exist in data
f_test_joins(attacks_aid_workers)

# Write the processed data to a csv file
saveRDS(attacks_aid_workers, 
        "03_update_predictors/output/som_attacks_aid_workers.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~# CLIMATE DATA #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                          Stops at:  June 2023                                #
#                          Source: FAO SWALIM (FSNAU)                          #
#                          Retrieval method: Automated                         #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Climate data from FSNAU given that SWALIM climate data is not up to date
# process climate data

climate_data <- fsnau_data |> 
  # select relevant columns and rename
  dplyr::select(
    district,
    year = y, month = m,
    cdi = `Combined Drought Index (CDI)`,
    rainfall = Rainfall
  ) |> 
  # impute mogadishu values
  dplyr::left_join(
    # impute cdi values for Mogadishu, this isn't typically calculated 
    # as its urban district and doesn't have vegetation, but we can assume 
    # its drought index is based on its neighbouring districts where water 
    # may come from
    fsnau_data |>
      dplyr::rename(year = y, month = m) |> 
      dplyr::filter(district %in% c("Balcad", "Afgooye")) |>
      dplyr::mutate(district = "Mogadishu") |>
      dplyr::group_by(district, year, month) |>
      dplyr::summarise(
        cdi = mean(`Combined Drought Index (CDI)`, na.rm = T)
      ), suffix = c("","_mog"),
    by = c("district", "month", "year")
  ) |> 
  dplyr::mutate(cdi = ifelse(is.na(cdi), cdi_mog, cdi)) |> 
  dplyr::select(-cdi_mog) |> 
  # rolling mean for rainfall
  dplyr::group_by(district) |>
  dplyr::arrange(year, month) |>
  # use three month rolling mean
  dplyr::mutate(
    rainfall_rollmean = zoo::rollmean(
      rainfall,
      k = 3,
      fill = NA, align = "right"
    )) |>
  ungroup() |>
  # give zero to values that are negative
  dplyr::mutate(rainfall_rollmean = ifelse(rainfall_rollmean < 0, 
                                           0, rainfall_rollmean)) |> 
  dplyr::select(district, year, month, rainfall_rollmean, cdi) |> 
  # ensure district names are correct
  dplyr::mutate(district = mast::admin_match(district)) |>
  # sort the data by year, month, and district
  dplyr::arrange(district, year, month) |>
  # remove duplicates
  dplyr::distinct() |>
  dplyr::left_join(district_lables, by = "district") |>
  dplyr::select(district, region, pcode, year, month, everything())

# check if all combinations  year, month and district exist in data
f_test_joins(climate_data)

# Write the processed data to a csv file
saveRDS(climate_data, "03_update_predictors/output/som_climate_data.rds")

#### ---- Same but for forecasting -- need data from 2000 to 2023
climate_data_forecasting <- fsnau_data_forecasting |> 
  # select relevant columns and rename
  dplyr::select(
    district,
    year = y, month = m,
    cdi = `Combined Drought Index (CDI)`,
    rainfall = Rainfall
  ) |> 
  # impute mogadishu values
  dplyr::left_join(
    # impute cdi values for Mogadishu, this isn't typically calculated 
    # as its urban district and doesn't have vegetation, but we can assume 
    # its drought index is based on its neighbouring districts where water 
    # may come from
    fsnau_data |>
      dplyr::rename(year = y, month = m) |> 
      dplyr::filter(district %in% c("Balcad", "Afgooye")) |>
      dplyr::mutate(district = "Mogadishu") |>
      dplyr::group_by(district, year, month) |>
      dplyr::summarise(
        cdi = mean(`Combined Drought Index (CDI)`, na.rm = T)
      ), suffix = c("","_mog"),
    by = c("district", "month", "year")
  ) |> 
  dplyr::mutate(cdi = ifelse(is.na(cdi), cdi_mog, cdi)) |> 
  dplyr::select(-cdi_mog) |> 
  # rolling mean for rainfall
  dplyr::group_by(district) |>
  dplyr::arrange(year, month) |>
  # use three month rolling mean
  dplyr::mutate(
    rainfall_rollmean = zoo::rollmean(
      rainfall,
      k = 3,
      fill = NA, align = "right"
    )) |>
  ungroup() |>
  # give zero to values that are negative
  dplyr::mutate(rainfall_rollmean = ifelse(rainfall_rollmean < 0, 
                                           0, rainfall_rollmean)) |> 
  dplyr::select(district, year, month, rainfall_rollmean, cdi) |> 
  # ensure district names are correct
  dplyr::mutate(district = mast::admin_match(district)) |>
  # sort the data by year, month, and district
  dplyr::arrange(district, year, month) |>
  # remove duplicates
  dplyr::distinct() |>
  dplyr::left_join(district_lables, by = "district") |>
  dplyr::select(district, region, pcode, year, month, everything())

# check if all combinations  year, month and district exist in data
f_test_joins(climate_data_forecasting)

# Write the processed data to a csv file
saveRDS(climate_data_forecasting, "03_update_predictors/output/som_climate_data_forecasting.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~# SAM ADMISSIONS DATA #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                          Stops at:  July 2023                                #
#                          Source: UNICEF                                      #
#                          Retrieval method: Manual                            #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

sam_data <- import("03_update_predictors/data/sam_treatment_updated.xlsx") |>
  # ensure district names are correct
  dplyr::mutate(district = mast::admin_match(district)) |>
  dplyr::rename(year = y, month = m) |>
  # ensure that we get aggregated month, year and district data
  dplyr::group_by(district, year, month) |> 
  dplyr::distinct(district, year, month, .keep_all = TRUE) |>  
  dplyr::summarise(sam_admissions = sum(sam_admissions), .groups = "drop") |> 
  # Complete the data frame by generating all combinations of year, month, and
  # district, filling non-existing entries with 0 in the specified column
  tidyr::complete(year, month, district) |> 
  # Replace all NAs with 0
  dplyr::mutate_at(c(
    "sam_admissions"
  ), ~ replace_na(., 0)) |> 
  dplyr::left_join(district_lables, by = "district") |>
  dplyr::select(district, region, pcode, year, month, sam_admissions) |> 
  # sort the data by year, month, and district
  dplyr::arrange(district, year, month) |>
  drop_na() |> 
  # lets join the latest SAMs data
  bind_rows(
    rio::import('03_update_predictors/data/SOMALIA_NUTRITION_SERVICES_DATABASE_Sept_2023.xlsx', sheet='DATA from ONA')|> 
      # select columns 
      # Pcode, Year, Month, and SAM from sam dataframe
      dplyr::select(
        district = District,
        year = Year, month = Month, 
        sam_admissions = SAM) |>
      # replace month and year with their corresponding numeric values
      dplyr::mutate(
        month = match(month, month.name),
        year = as.numeric(year)
      ) |>
      filter(!is.na(district)) |> 
      dplyr::mutate(district = mast::admin_match(district)) |> 
      # group the rows by district, year, and month
      dplyr::group_by(district, year, month) |>
      # calculate the sum of sam_admissions
      dplyr::summarise(sam_admissions = sum(sam_admissions), 
                       .groups = "drop") |> 
      # join district names
      dplyr::left_join(district_lables, by = "district") |> 
      dplyr::select(district, region, pcode, year, month, sam_admissions) |> 
      dplyr::arrange(year, month, district)
    
  ) |> 
  # for rows with zero sam_admissions, 
  # lets carry forward the last non-zero value (by ditrict and time)
  group_by(district) |>
  arrange(year, month,) |>
  # Replace 0 with NA to use tidyr::fill
  mutate(sam_admissions = if_else(sam_admissions == 0, NA_real_, sam_admissions)) |>
  # Fill NA with the previous value
  fill(sam_admissions, .direction = "down") |> 
  ungroup() |> 
  distinct(district, region, pcode, year, month, .keep_all = T)

# check if all combinations  year, month and district exist in data
f_test_joins(sam_data) 

# Write the processed data to a csv file
saveRDS(sam_data, "03_update_predictors/output/som_sam_admissions.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# MARKETS DATA #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                          Stops at:  July 2023                                #
#                          Source: FSNAU                                       #
#                          Retrieval method: Manual                            #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# source FC TOT cleaning script
source("03_update_predictors/code/01_markets_data_cleaning.R")

# remove unnecessary cols
markets_data <- markets_data_clean |>
  # remove unnecessary cols
  dplyr::select(-tm, -date, -admin0) |>
  dplyr::rename(year = y, month = m) |>
  # ensure district names are correct
  dplyr::mutate(district = mast::admin_match(district)) |>
  dplyr::arrange(district, year, month) |>
  # remove duplicates
  dplyr::distinct() |>
  dplyr::select(district, region, pcode, year, month, everything())

# check if all combinations  year, month and district exist in data
f_test_joins(markets_data)

# Write the processed data to a csv file
saveRDS(markets_data, "03_update_predictors/output/som_markets_tot_updated.rds")
