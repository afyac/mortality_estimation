#-----------------------
### INSTALL PACKAGES
# ----------------------
library(tidyverse)
library(rio)
library(ggpubr)
library(zoo)
library(ggplot2)

colours_f <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")

#-------------------------
### IMPORT DATA
#-------------------------

## Open hh_obs and metadata, extract the right column and merge both
hh_obs <-  rio::import('01_extract_smart_surveys/output/data_smart_survey_updated.csv')
hh_obs <- hh_obs[c('region', 'district', 'admin0', 'pcode', 'n', 'n_u5', 'n_died', 'n_died_u5', 
                   'p_time', 'p_time_u5', 'surveyId', 'qualityScore', 'hh_weights')]


metadata <- rio::import('01_extract_smart_surveys/output/metadata_updated.csv')
metadata <- metadata[c('SurveyID', 'LHZ', 'Start_Date', 'End_Date', 'Recall_Days', 'lshtm_cdr_est', 'lshtm_cdr_u5_est')]

data <- merge(hh_obs, metadata, by.x='surveyId', by.y='SurveyID', all.x=TRUE)

#--------------------------
###PLAY WITH TIMES
#--------------------------

## Generate time_units and calculate time unit start and end and also mid time period for the different houshold
y_start <- 2014
y_end <- 2023
m_start <- 1
m_end <- 12
burn_in_period <- 1 # by how many years to extend period backwards (useful for lags)
burn_out_period <- 0 # by how many years to extend period forward
when <- list(y_start, y_end, m_start, m_end, burn_in_period, burn_out_period)
names(when) <- (c("y_start", "y_end", "m_start", "m_end", "burn_in_period", "burn_out_period"))

time_units <- seq(1, (( when$y_end + when$burn_out_period - when$y_start + when$burn_in_period ) * 12 +
                        when$m_end - when$m_start + 1 ), 1)
time_unit_data <- data.frame(time_units)
time_unit_data$year <- floor((time_units + when$m_start - 2) / 12) + (when$y_start - when$burn_in_period)
time_unit_data$month <- (time_units + when$m_start - 1) - (time_unit_data $year  - (when$y_start - when$burn_in_period) ) * 12

# Add stard and end date into our dataset
data$start_year <- year(as.Date(data$Start_Date))
data$start_month <- month(as.Date(data$Start_Date))
data$end_year <- year(as.Date(data$End_Date))
data$end_month <- month(as.Date(data$End_Date))
data$recall_mid <- as.Date(data$End_Date) - data$Recall_Days/2
data$year_recall_mid <- year(data$recall_mid)
data$month_recall_mid <- month(data$recall_mid)

data <- merge(data, time_unit_data, by.x = c('start_year', 'start_month'), by.y=c('year', 'month'), all.x=TRUE)
colnames(data)[length(colnames(data))] <- 'time_unit_start'
data <- merge(data, time_unit_data, by.x = c('end_year', 'end_month'), by.y=c('year', 'month'), all.x=TRUE)
colnames(data)[length(colnames(data))] <- 'time_unit_end'
data <- merge(data, time_unit_data, by.x = c('year_recall_mid', 'month_recall_mid'), by.y=c('year', 'month'), all.x=TRUE)
colnames(data)[length(colnames(data))] <- 'time_unit_recall_mid'

#Select only those with year > 2014
data <- data[which(data$year_recall_mid > 2014),]

#-------------------------
## PLOT CDR OVER YEAR AND OVERALL COUNTRY
#-------------------------

f_plot_smart_survey_dr_overall <- function(resp_var, data){
  # Which response variable?
  if (resp_var %in% c("cdr", "n_died")) {resp_var <- "n_died"}
  if (resp_var %in% c("cdr_u5", "n_died_u5")) {resp_var <- "n_died_u5"}
  
  # Depending on which age group...
  if (resp_var == "n_died") {
    p_time_var <- 'p_time'
    # plot label
    dr_lab <- "Crude death rate - SMART SURVEYS INFORMATION (per 10,000 person-days)"
  }
  if (resp_var == "n_died_u5") {
    p_time_var <- 'p_time_u5'
    # plot label
    dr_lab <- "Under 5 years death rate - SMART SURVEYS INFORMATION (per 10,000 child-days)"
  }
  
  # Aggregate to country-month
  df_plot <- aggregate(data[, c(resp_var, p_time_var)], 
                       by = list(time_unit_recall_mid = data$time_unit_recall_mid), FUN = sum)
  
  # Compute death rate and add dates
  df_plot$dr <- df_plot[, c(resp_var)] * 10000 / df_plot[, c(p_time_var)]
  df_plot <- merge(df_plot, unique(data[, c("time_unit_recall_mid", "year_recall_mid", "month_recall_mid")]),
                   by = "time_unit_recall_mid", all.x = TRUE)
  
  df_plot$date <- ymd(paste(df_plot$year_recall_mid, df_plot$month_recall_mid, "1", sep = "-"))  
  
  #...................................
  ## Generate plot with running mean
  # Plot 1 - free y axis
  plot1 <- ggplot(data = df_plot, aes(x = date, y = dr)) + 
    geom_point(alpha = 0.3, size = 3, colour = colours_f[6]) +
    geom_line(aes(y = zoo::rollmean(dr, k = 3, fill = NA, align = "right")),
              alpha = 0.7, linewidth = 1, colour = colours_f[7]) +
    scale_y_continuous(dr_lab) +  
    scale_x_date("Year", date_breaks = "1 year", date_labels = "%Y") +
    theme_bw()
  
  ggsave(paste('01_extract_smart_surveys/vizualisation/output/som_', resp_var, '_smart_survey_overall.jpg', sep=''),
         dpi = "print", units = "cm", width = 30, height = 30)
}

f_plot_smart_survey_dr_overall('cdr', data)
f_plot_smart_survey_dr_overall('cdr_u5', data)


