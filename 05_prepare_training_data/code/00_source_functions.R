## Function to calculate surveys coverage
f_calc_days <- function(f_surveys_cov, f_df) {
  # select survey
  s <- subset(f_df, SurveyID == f_surveys_cov["surveyId"])
  tm_now <- as.integer(f_surveys_cov["time_unit"])
  c1 <- as.integer(s["time_unit_recall_start"]) - tm_now
  c2 <- as.integer(s["time_unit_recall_end"])- tm_now
  x1 <- 0
  
  # calculate proportion of the month's days that are covered by the survey's recall period
  if (c1 > 0) { x1 <- 0.0 }
  if (c1 == 0) { x1 <- (as.integer(s["days_in_month_start"]) - as.integer(s["day_start"]) ) /
    as.integer(s["days_in_month_start"]) }
  if (c1 < 0 & c2 > 0) { x1 <- 1.0 }
  if (c2 == 0) { x1 <- as.integer(s["day_end"]) / as.integer(s["days_in_month_end"]) }
  if (c2 < 0) { x1 <- 0.0 }
  
  return(x1)
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
