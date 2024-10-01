f_generate_market_data <- function(time_units){
  # source FC TOT cleaning script
  source("03_update_predictors/code/04_1_markets_data_cleaning.R")
  
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
    dplyr::select(district, region, pcode, year, month, everything()) |>
    dplyr::mutate(date = as.Date(paste(paste(year, '-', month, "-01", sep=""))))
  
  markets_data <- merge(time_units, markets_data,
                    by=c('district', 'date', 'year', 'month', 'region', 'pcode', 'reg_code'), all.x=TRUE)
  
  return(markets_data)
}
