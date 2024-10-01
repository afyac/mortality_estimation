################################################################################
#                                                                              #
# Goal:            Update predictors                                           #
#                                                                              #
# Documentation: Read documenation/som_predictors_description.pdf              #
#                                                                              #
#                                                                              #
################################################################################

# Prepare ----------------------------------------------------------------------

# Define strata of Somalia
admin2 <- mast::som_admin2 |>
  dplyr::select(region, district, reg_code, pcode) |>
  dplyr::mutate(
    district = mast::admin_match(district) ##check if we have 74 districts
  )

# get FSNAU dataset
fsnau_data_forecasting <- mast::fsnau_dashboard(years = 2013:2024)

# Generate time series
time_units <- mast::f_gen_ts(admin2_f = admin2, name_col = 'district', y_end = 2024, m_end = 7)

# Find dataset names
datasets <- rio::import('03_update_predictors/data/SOM_TO_BE_MODIFIED.xlsx')
acled_dataset <- datasets[datasets$Type == 'ACLED', ]$File_Name
sam_dataset <- datasets[datasets$Type == 'SAM', ]$File_Name
aid_workers_dataset <- datasets[datasets$Type == 'AID_WORKERS', ]$File_Name

# Health Data ------------------------------------------------------------------
source("03_update_predictors/code/01_som_health_data.R")

## Malaria WHO
malaria_who <- f_generate_malaria_who_dataset(c('03_update_predictors/data/Malaria Data 2014-2018.xlsx', 
                                                '03_update_predictors/data/Malaria_2019_dec_2023.xlsx'), 
                                              time_units)
saveRDS(malaria_who, 
        paste("03_update_predictors/output/som_malaria_who.rds"))

## Malaria FSNAU
malaria_fsnau <- f_generate_fsnau_malaria(fsnau_data_forecasting, time_units)
saveRDS(malaria_fsnau, 
        paste("03_update_predictors/output/som_malaria_fsnau.rds"))

## Measles WHO
measles_who <- f_generate_measles_who('03_update_predictors/data/2015- Sept 2022 Measles Cases.xlsx',
                                      time_units)
saveRDS(measles_who, 
        paste("03_update_predictors/output/som_measles_who.rds"))

## Measles FSNAU
measles_fsnau <- f_generate_measles_fsnau(fsnau_data_forecasting, time_units)

saveRDS(measles_fsnau, 
        paste("03_update_predictors/output/som_measles_fsnau.rds"))

## Cholera FSNAU
cholera_fsnau <- f_generate_cholera_fsnau(fsnau_data_forecasting, time_units)

saveRDS(cholera_fsnau, 
        paste("03_update_predictors/output/som_cholera_fsnau.rds"))

## Immunization WHO
immunization_who <- f_generate_immunization_who('03_update_predictors/data/IMMUNIZATION_DATA.xlsx', time_units)
saveRDS(immunization_who, 
        paste("03_update_predictors/output/som_immunization_who.rds"))

# Humanitarian Presence Data ---------------------------------------------------
source("03_update_predictors/code/02_som_hum_presence_data.R")

acled_data <- f_generate_acled_data(paste("03_update_predictors/data/", acled_dataset, ".csv", sep=""), time_units)
saveRDS(acled_data, 
        paste("03_update_predictors/output/som_acled_data.rds"))

aid_workers <- f_generate_aid_workers(paste("03_update_predictors/data/", aid_workers_dataset ,".csv", sep=""), time_units)
saveRDS(aid_workers, 
        paste("03_update_predictors/output/som_aid_workers_data.rds"))

# CLIMATE Data -----------------------------------------------------------------
source("03_update_predictors/code/03_som_climate_data.R")
climate_data <- f_generate_climate_data(fsnau_data_forecasting, time_units)
saveRDS(climate_data, 
        paste("03_update_predictors/output/som_climate_data.rds"))

# MARKET Data ------------------------------------------------------------------
source("03_update_predictors/code/04_som_market_data.R")
ts <- time_units
markets_data <- f_generate_market_data(time_units)
saveRDS(markets_data, 
        paste("03_update_predictors/output/som_markets_data.rds"))

# SAM/MAM Data -----------------------------------------------------------------
source("03_update_predictors/code/05_som_sam_data.R")
list_sam <- c("03_update_predictors/data/sam_treatment_updated.xlsx",
              paste('03_update_predictors/data/', sam_dataset , '.xlsx', sep=""))
sam_data <- f_generate_sam_data(list_sam, time_units)
saveRDS(sam_data, 
        paste("03_update_predictors/output/som_sam_data.rds"))