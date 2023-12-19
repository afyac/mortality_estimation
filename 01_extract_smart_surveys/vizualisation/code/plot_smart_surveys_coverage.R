#-----------------------
### INSTALL PACKAGES
# ----------------------
library(tidyverse)
library(rio)

#--------------------------
## FUNCTIONS
#-------------------------
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


admin2 <- rio::import('00_overall_data/som_admin2.xlsx')
admin2 <- subset(admin2, select=c('district', 'region'))

##--- CALCULATE COVERAGE 
metadata_coverage <- metadata[c('SurveyID', 'Start_Date', 'End_Date')]

data_coverage <- merge(hh_obs[c('p_time', 'surveyId', 'district', 
                                'qualityScore', 'hh_weights')], metadata_coverage, by.x=c('surveyId'), by.y=c('SurveyID'))

time_unit_data <- f_generate_time_units(
  y_analysis_start = 2010, y_analysis_end = 2024,
  m_analysis_start = 1, m_analysis_end = 12
)

time_unit_data$date <- as.Date(paste0(time_unit_data$year, "-", time_unit_data$month, "-01"))
data_coverage$start_date <- as.Date(paste0(year(data_coverage$Start_Date), "-", 
                                           month(data_coverage$Start_Date), "-01"))
data_coverage$end_date <- as.Date(paste0(year(data_coverage$End_Date), "-", 
                                         month(data_coverage$End_Date), "-01"))

data_coverage <- merge(data_coverage, time_unit_data[c('time_unit', 'date')], 
                       by.x=c('start_date'), by.y=c('date'), all.x=TRUE)
colnames(data_coverage)[ncol(data_coverage)] <- 'start_time_unit'

data_coverage <- merge(data_coverage, time_unit_data[c('time_unit', 'date')], 
                       by.x=c('end_date'), by.y=c('date'), all.x=TRUE)
colnames(data_coverage)[ncol(data_coverage)] <- 'end_time_unit'


data_coverage <- data.frame(data.table::setDT(data_coverage)[ , list(surveyId = surveyId, p_time=p_time,
                                                         district = district, qualityScore= qualityScore, hh_weights=hh_weights,
                                                         time_unit = seq(start_time_unit, end_time_unit)), by = 1:nrow(data_coverage)])

data_coverage$data_availability <- data_coverage$p_time*data_coverage$qualityScore*data_coverage$hh_weights
data_coverage$data_availability <- data_coverage$data_availability/max(data_coverage$data_availability)

data_coverage <- merge(data_coverage, admin2, by.x = c('district'), by.y=c('district'), all.x = TRUE)
data_coverage <- aggregate(data_coverage[c('data_availability', 'hh_weights')], 
                           by=list(data_coverage$district, data_coverage$time_unit), FUN=sum)
colnames(data_coverage) <- c('district', 'time_unit', 'data_availability', 'hh_weights')
admin2 <- dplyr::cross_join(admin2, time_unit_data)
data_coverage <- merge(data_coverage, admin2[c('district', 'time_unit', 'region', 'year', 'month')], 
                       by.x = c('district', 'time_unit'), by.y=c('district', 'time_unit'), all.y = TRUE)
data_coverage['data_availability'][which(is.na(data_coverage$data_availability)), ] <- 0
data_coverage['hh_weights'][which(is.na(data_coverage$hh_weights)), ] <- 0
time_unit_data <- subset(time_unit_data, year >= 2015 & year < 2024)
data_coverage <- subset(data_coverage, year >= 2015 & year <2024)

plot <- ggplot(data_coverage, aes(x = time_unit, y = district) )
plot <- plot + geom_tile(aes(fill=data_availability), colour = "grey80", show.legend = FALSE) +
  scale_x_continuous("month, year", expand=c(0,0), breaks = unique(time_unit_data[, c("time_unit", "month")])[, "time_unit"], 
                     labels = time_unit_data$month ) +
  scale_y_discrete(unique(admin2$district), expand=c(0,0) ) +
  scale_fill_gradientn(colours = c("grey90", "yellow", "red"),
                       values = c(0, 0.0000001, 1 )) +
  facet_grid(region~year, space="free", scales="free", switch="x") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill=NA, colour="white"),
        panel.spacing=unit(0,"cm"), strip.text.y = element_text(angle = 0))

plot
ggsave('01_extract_smart_surveys/vizualisation/output/som_coverage.png', height = 35, width = 30, units = "cm", dpi = "print")

data_coverage_region <- aggregate(data_coverage$data_availability,
                                  by=list(region=data_coverage$region, 
                                          time_unit=data_coverage$time_unit, year=data_coverage$year, month=data_coverage$month),
                                  FUN=sum)
colnames(data_coverage_region)[length(data_coverage_region)] <- 'data_availability'
plot <- ggplot(data_coverage_region, aes(x = time_unit, y = region) )
plot <- plot + geom_tile(aes(fill=data_availability), colour = "grey80", show.legend = FALSE) +
  scale_x_continuous("month, year", expand=c(0,0), breaks = unique(time_unit_data[, c("time_unit", "month")])[, "time_unit"], 
                     labels = time_unit_data$month) +
  scale_y_discrete(unique(admin2$region), expand=c(0,0)) +
  scale_fill_gradientn(colours = c("grey90", "yellow", "red"),
                       values = c(0, 0.0000001, 1 )) +
  facet_grid(region~year, space="free", scales="free", switch="both") +
  theme_bw() + theme(strip.text.y = element_blank(),
                     strip.background = element_rect(colour = 'black', fill='white'),
                     panel.spacing = unit(0, 'cm'), 
                     axis.text.x = element_blank(),
                     strip.text.x = element_text(angle=90),
                     axis.title = element_blank(),
                     axis.title.y = element_blank(),
                     axis.ticks = element_blank())

plot
ggsave('01_extract_smart_surveys/vizualisation/output/som_coverage_region.png', height = 35, width = 30, units = "cm", dpi = "print")