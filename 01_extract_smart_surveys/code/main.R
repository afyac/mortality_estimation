
#----------------------
## MAIN PART
#----------------------
##Directory path
dir_path <- paste(dirname(dirname(rstudioapi::getSourceEditorContext()$path)), '/', sep="")


#---------------------
## PLEASE BEFORE ANYTHING MODIFY THE DATE HERE 
#---------------------
y_start <- 2014
y_end <- 2024
m_start <- 1
m_end <- 07
burn_in_period <- 1 # by how many years to extend period backwards (useful for lags)
burn_out_period <- 0 # by how many years to extend period forward
WHEN <- list(y_start, y_end, m_start, m_end, burn_in_period, burn_out_period)
names(WHEN) <- (c("y_start", "y_end", "m_start", "m_end", "burn_in_period", "burn_out_period"))

#---------------------
## PLEASE BEFORE ANYTHING MODIFY THE SETTLEMENT WE ARE USING 
#---------------------
settlement_file <- read.csv(paste(dir_path, '../00_overall_data/som_settlement.csv', sep=""))
admin2_file <- rio::import(paste(dir_path, '../00_overall_data/som_admin2.xlsx', sep=""))


#---------------------
##Load Codes
#---------------------

##Contains generate_folders function
source(paste(dir_path, 'code/folders.R', sep=""), echo = TRUE)
## Contains sort_surveys function
source(paste(dir_path, 'code/sort_rename.R', sep=""), echo = TRUE)
##Needed to generate metadata and plausibility score (reproduction of ENA Software)
source(paste(dir_path, 'code/metadata.R', sep=""), echo = TRUE)
source(paste(dir_path, 'code/plausibility_score.R', sep=""), echo = TRUE)
##Contains admin2_survey_to_csv function
source(paste(dir_path, 'code/admin2_to_csv.R', sep=""), echo = TRUE)
##Contains individual_output_update function
source(paste(dir_path, 'code/individual_update.R', sep=""), echo = TRUE)
##Contains aggregate_survey_to_csv function
source(paste(dir_path, 'code/aggregate_to_csv.R', sep=""), echo = TRUE)
##Contains aggregate_output_update function
source(paste(dir_path, 'code/aggregate_update.R', sep=""), echo = TRUE)
##Contains lhz_survey_to_csv function
source(paste(dir_path, 'code/lhz_to_csv.R', sep=""), echo = TRUE)
##Contains lhz_output_update function
source(paste(dir_path, 'code/lhz_update.R', sep=""), echo = TRUE)
##update_district
source(paste(dir_path, 'code/update_district_name.R', sep=""), echo=TRUE)
##produce final file
source(paste(dir_path, 'code/produce_final.R', sep=""), echo=TRUE)
#---------------------
##Steps
#--------------------

#Step0 -- Generate folders (lhz, admin2) if they are not available
generate_folders(dir_path)

#Step 1 -- Sort surveys into different categories: admin2/lhz and aggregate/individual
sort_surveys(dir_path)

# Step 2 -- Generate csv and metadata for individual surveys
admin2_survey_to_csv(dir_path)
individual_output_update(dir_path)

#Step 3 -- Generate csv and metadata for aggregate surveys
aggregate_survey_to_csv(dir_path)
aggregate_output_update(dir_path)

#Step 4 -- Generate csv and metadata for lhz -- individual surveys
lhz_survey_to_csv(dir_path)
lhz_output_update(dir_path)

#Step 5 -- AGGREGATE + LHZ -- Impossible because no access to Clusters settlement
######

#Step 6 -- Update District name on the output to match with the one in next steps and correct dates and check redundancy
#+ add pcode and modify region name and district one using OCHA 
#+ rename columns dataset
update_district(dir_path)

#Step7 -- Produce Final file used for the next steps (pop reconstruction)
produce_final_file(dir_path)

