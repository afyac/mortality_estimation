################################################################################
#                                                                              #
# Goal:            Predicting data preparation and merging with predictors     #
#                                                                              #
# Written:         Mo Yusuf                                                    #
# Date written:    2023-08-04                                                  #
#                                                                              #
# Reviewer:        xx xx                                                       #
# Date reviewed:   2023-08-xx                                                  #
#                                                                              #
#                                                                              #
################################################################################

# Prepare ----------------------------------------------------------------------

# update the predicting data
predicting_data <- rio::import('04_process_predictors/output/som_predictors_data_complete.rds') |>
  filter(year > '2014')


# save it
# save the training data
predicting_data |>
  saveRDS("06_prepare_predicting_data/output/som_predicting_data.rds")
