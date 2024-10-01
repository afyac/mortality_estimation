f_generate_sam_data <- function(list_file_sam, ts){
  district_labels <- ts |> 
    dplyr::select(district, region, pcode) |> 
    dplyr::distinct()
  
  sam_data <- rio::import(list_file_sam[1]) |>
    # ensure district names are correct
    dplyr::mutate(district = mast::admin_match(district)) |>
    dplyr::rename(year = y, month = m) |>
    # ensure that we get aggregated month, year and district data
    dplyr::group_by(district, year, month) |> 
    dplyr::distinct(district, year, month, .keep_all = TRUE) |>  
    dplyr::summarise(sam_admissions = sum(sam_admissions), .groups = "drop") |> 
    # Complete the data frame by generating all combinations of year, month, and
    # district, filling non-existing entries with 0 in the specified column
    tidyr::complete(year, month, district)|> 
    # Replace all NAs with 0
    dplyr::mutate_at(c(
      "sam_admissions"
    ), ~ tidyr::replace_na(., 0)) |> 
    dplyr::left_join(district_labels, by = "district") |>
    dplyr::select(district, region, pcode, year, month, sam_admissions) |> 
    # sort the data by year, month, and district
    dplyr::arrange(district, year, month) |>
    tidyr::drop_na() |> 
    # lets join the latest SAMs data
    dplyr::bind_rows(
      rio::import(list_file_sam[2], sheet='DATA from ONA')|> 
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
        dplyr::filter(!is.na(district)) |> 
        dplyr::mutate(district = mast::admin_match(district)) |> 
        # group the rows by district, year, and month
        dplyr::group_by(district, year, month) |>
        # calculate the sum of sam_admissions
        dplyr::summarise(sam_admissions = sum(sam_admissions), 
                         .groups = "drop") |> 
        # join district names
        dplyr::left_join(district_labels, by = "district") |> 
        dplyr::select(district, region, pcode, year, month, sam_admissions) |> 
        dplyr::arrange(year, month, district)
      
    ) |> 
    # for rows with zero sam_admissions, 
    # lets carry forward the last non-zero value (by ditrict and time)
    dplyr::group_by(district) |>
    dplyr::arrange(year, month,) |>
    # Replace 0 with NA to use tidyr::fill
    dplyr::mutate(sam_admissions = dplyr::if_else(sam_admissions == 0, NA_real_, sam_admissions)) |>
    # Fill NA with the previous value
    tidyr::fill(sam_admissions, .direction = "down") |> 
    dplyr::ungroup() |> 
    dplyr::distinct(district, region, pcode, year, month, .keep_all = T)
  
  sam_data <- merge(sam_data, ts, by=c("district", "region", "pcode", "year", "month"), 
                    all.y=TRUE)
  return(sam_data)
}

