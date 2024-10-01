# Climate data from FSNAU given that SWALIM climate data is not up to date
# process climate data

f_generate_climate_data <- function(fsnau_data_forecasting, ts){
  climate_data <- fsnau_data_forecasting |> 
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
      fsnau_data_forecasting |>
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
    dplyr::ungroup() |>
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
    dplyr::mutate(date = as.Date(paste(paste(year, '-', month, "-01", sep=""))))
  
  climate_data <- merge(ts, climate_data,
                               by=c('district', 'date', 'year', 'month'), all.x=TRUE)
  return(climate_data)
}