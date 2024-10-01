#-----------------------
### INSTALL PACKAGES
# ----------------------
library(tidyverse)
library(rio)

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
y_start <- 2013
y_end <- 2024
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

#------------------------
### PLOT SMART SURVEYS VISUALIZATION
#------------------------

## Define type of SMART Surveys
data$type_survey <- 'admin2'
data[grepl('lhz', data$surveyId),]$type_survey <- 'lhz'

#Select only those with year > 2014
data <- data[which(data$year_recall_mid > 2014),]

###Plot Frequency SMART SURVEYS
data_met <-  data[match(unique(data$surveyId), data$surveyId),]
data_count <- as.data.frame(table(data_met[,c('year_recall_mid', 'type_survey')])) 

plot_smart_surveys <- ggplot(data_count, aes(x=as.character(year_recall_mid), y=Freq, fill=c(type_survey))) +geom_bar(stat = "identity") + theme_bw() +
  theme(axis.text.x = element_text(size=24, angle=45), axis.title.y = element_text(size=24),axis.text.y = element_text(size=28), legend.position="top", legend.title = element_text(size=28),
        legend.text = element_text(size=28), axis.title.x = element_blank()) +ylab('Number of SMART Surveys') +
  scale_fill_manual(name ='Type of Survey', labels = c("District", 'Livelihood Zone'), values =c("#56B4E9","#CC79A7"))
ggsave('01_extract_smart_surveys/vizualisation/output/som_frequency_smart_survey_year.jpg',
       dpi = "print", units = "cm", width = 30, height = 30)

### Plot CDR - type of surveys 
data <- data[which(data$year_recall_mid > 2014),]
data_admin2 <- data[which(data$type_survey == 'admin2'),]
plot_cdr <- ggplot(data, aes(y=lshtm_cdr_u5_est, x =as.character( year_recall_mid), fill=c(type_survey)))+
  geom_boxplot() + theme_bw() +
  theme(axis.text.x = element_text( size=28, angle=45, margin = margin(t = 30)), axis.title.y = element_text(size=24),axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=20), legend.position="top", legend.title = element_text(size=28),
        legend.text = element_text(size=28), axis.title.x = element_blank())+ 
  scale_y_continuous('Crude death rate under 5 (per 10.000 person-days)' , limits = c(0, NA))  +
  scale_fill_manual(name ='Type of Survey', labels = c("District", 'Livelihood Zone'), values =c("#56B4E9","#CC79A7"))
ggsave('01_extract_smart_surveys/vizualisation/output/som_cdr_u5_per_year_type_survey.png',
       dpi = "print", units = "cm", width = 30, height = 30)

data <- data[which(data$year_recall_mid > 2014),]
data_admin2 <- data[which(data$type_survey == 'admin2'),]
plot_cdr <- ggplot(data, aes(y=lshtm_cdr_est, x =as.character( year_recall_mid), fill=c(type_survey)))+
  geom_boxplot() + theme_bw() +
  theme(axis.text.x = element_text( size=28, angle=45, margin = margin(t = 30)), axis.title.y = element_text(size=24), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=20), legend.position="top", legend.title = element_text(size=28),
        legend.text = element_text(size=28), axis.title.x = element_blank())+ 
  scale_y_continuous('Crude death rate (per 10.000 person-days)' , limits = c(0, NA))  +
  scale_fill_manual(name ='Type of Survey', labels = c("District", 'Livelihood Zone'), values =c("#56B4E9","#CC79A7"))
ggsave('01_extract_smart_surveys/vizualisation/output/som_cdr_per_year_type_survey.png',
       dpi = "print", units = "cm", width = 30, height = 30)

### Plot CDR - type of idps
data <- data[which(data$year_recall_mid > 2014),]
data_admin2 <- data[which(data$type_survey == 'admin2'),]
plot_cdr <- ggplot(data, aes(y=lshtm_cdr_est, x =as.character( year_recall_mid), fill=c(LHZ)))+
  geom_boxplot() + theme_bw() + theme(axis.text.x = element_text( size=28, angle=45, margin = margin(t = 30)), axis.ticks.x = element_blank(),
                                      axis.title.y = element_text(size=24),axis.text.y = element_text(size=20), legend.position="top", legend.title = element_text(size=28),
        legend.text = element_text(size=28), axis.title.x = element_blank())+ 
  scale_y_continuous('Crude death rate (per 10.000 person-days)' , limits = c(0, NA))  +
  scale_fill_manual(name ='Surveyed Population', labels = c("IDP", 'Others'), values =c("#E69F00","#D55E00")) 
ggsave('01_extract_smart_surveys/vizualisation/output/som_cdr_per_year_type_idp.jpg',
       dpi = "print", units = "cm", width = 30, height = 30)

### Plot CDR U5 - type of idps
data <- data[which(data$year_recall_mid > 2014),]
data_admin2 <- data[which(data$type_survey == 'admin2'),]
plot_cdr <- ggplot(data, aes(y=lshtm_cdr_u5_est, x =as.character( year_recall_mid), fill=c(LHZ)))+
  geom_boxplot() + theme_bw() +
  theme(axis.text.x = element_text(size=28, angle=45, margin = margin(t = 30)), axis.title.y = element_text(size=24), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=20), legend.position="top", legend.title = element_text(size=28),
        legend.text = element_text(size=28), axis.title.x = element_blank())+ 
  scale_y_continuous('Crude death rate under 5 (per 10.000 person-days)' , limits = c(0, NA))  +
  scale_fill_manual(name ='Surveyed Population', labels = c("IDP", 'Others'), values =c("#E69F00","#D55E00")) 
ggsave('01_extract_smart_surveys/vizualisation/output/som_cdr_u5_per_year_type_idp.jpg',
       dpi = "print", units = "cm", width = 30, height = 30)



### Plot Quality Score distribution
plot_freq <- ggplot(data_met, aes(qualityScore)) + geom_histogram(fill ="#56B4E9", color='black') + ylab('Frequency') + xlab('Survey Quality Score') + theme_bw()
ggsave('01_extract_smart_surveys/vizualisation/output/som_hist_qualityscore.jpg',
       dpi = "print")

### Plot Recall days
plot_freq <- ggplot(data_met, aes(Recall_Days)) + geom_histogram(fill ="#009E73", color='black') + ylab('Frequency') + xlab('Survey Recall Period Range') + theme_bw()
ggsave('01_extract_smart_surveys/vizualisation/output/som_hist_recall_days.jpg',
       dpi = "print")

### Plot sample size
sample_size <- aggregate(data$n, by=list(surveyId = data$surveyId), FUN=sum)
plot_freq <- ggplot(sample_size, aes(x)) + geom_histogram(fill ="#CC79A7", color='black') + ylab('Frequency') + xlab('Survey Sample Size') + theme_bw()
ggsave('01_extract_smart_surveys/vizualisation/output/som_hist_sample_size.jpg',
       dpi = "print")
