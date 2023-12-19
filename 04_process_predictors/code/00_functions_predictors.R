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

# Function to create lags ------------------------------------------------------
f_create_lags <- function(data, vars, lags = 1:6) {
  data <- data |>
    mutate(date = as.Date(paste(year, month, "01", sep="-"), "%Y-%m-%d")) |>
    arrange(region, district, date)
  
  for (var in vars) {
    for (lag in lags) {
      data <- data |>
        group_by(region, district) |>
        mutate(!!paste0(var, "_lag", lag) := lag(.data[[var]], lag))
    }
  }
  data <- data |> ungroup()
  
  return(data)
}

# Function to categorise variables in data -------------------------------------

f_categorise <- function(data, breaks_and_labels) {
  
  # Function to apply the transformation
  cut_variable <- function(x, breaks, labels) {
    cut(x, breaks = breaks, labels = labels, right = FALSE, 
        include.lowest = TRUE)
  }
  
  # Function to select columns based on prefix and exclusion of "_scn"
  select_columns <- function(data, prefix) {
    col_names <- names(data)
    selected_cols <- col_names[startsWith(col_names, prefix) & 
                                 !str_detect(col_names, "_scn")]
    selected_cols
  }
  
  # Iterate over breaks_and_labels to apply the transformation
  data_transformed <- data
  
  for (prefix in names(breaks_and_labels)) {
    selected_columns <- select_columns(data_transformed, prefix)
    data_transformed <- data_transformed |>
      mutate(across(
        .cols = all_of(selected_columns),
        .fns = ~ cut_variable(.x, 
                              breaks = breaks_and_labels[[prefix]]$breaks, 
                              labels = breaks_and_labels[[prefix]]$labels),
        .names = "{.col}_cat"
      ))
  }
  
  # Check for missing values in newly created variables
  new_vars <- grep("_cat$", names(data_transformed), value = TRUE)
  if (any(sapply(data_transformed[new_vars], function(x) any(is.na(x))))) {
    message("Warning: Some variables have not been properly categorized and contain missing values.")
  }
  
  return(data_transformed)
}

# Function to plot the proportion of missing predictors -------------------------

f_plot_missing_data <- function(data) {
  
  data <- data |> 
    mutate(across(-Year, ~if_else(is.na(.), 0, 1))) |>
    group_by(Year) |>
    summarise_all(.funs = mean, na.rm = TRUE) |>
    pivot_longer(-Year, names_to="variable", values_to="value")
  
  # plot missing data
  plot <- ggplot(data = data, aes(x = Year, y = fct_rev(variable))) +
    geom_tile(aes(fill=value), colour = "grey80", show.legend = TRUE) +
    ylab("") +
    scale_fill_gradientn(colours = c("red", "green"),
                         values = rescale(c(0, 0.6, 1)),
                         labels = scales::percent,
                         limits = c(0, 1)) +
    facet_grid(~Year, space="free", scales="free", switch="x") +
    theme_bw() +
    theme(axis.title = element_text(colour="grey20")) +
    labs(fill = "% of district months with complete predictor data") +
    theme(strip.placement = "outside",
          strip.background = element_rect(fill=NA, colour="grey50"),
          panel.spacing = unit(0,"cm"), strip.text.y = element_text(angle = 0),
          strip.text.x = element_text(vjust=0)) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          legend.position = "top", legend.key.width = unit(2, "cm")) +
    guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))
  
  return(plot)
}
