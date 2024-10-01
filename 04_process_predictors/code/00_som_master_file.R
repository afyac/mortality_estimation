################################################################################
#                                                                              #
# Goal:            Run this file to perform data processing                    #
#                                                                              #
################################################################################

# PLEASE BEFORE ANYTHING CHANGE OR CHECKS THE DATE  ----------------------------
END_DATE_ANALYSIS <- "2024-06-01"

## Launch the two following codes to generate the complete dataset
source('04_process_predictors/code/00_som_breaks_and_labels.R')
source('04_process_predictors/code/01_som_complete_rate_predictors.R')
source('04_process_predictors/code/02_som_trans_norm_cat_predictors.R')
