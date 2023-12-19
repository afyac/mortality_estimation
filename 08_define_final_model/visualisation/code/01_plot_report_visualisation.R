# Load required libraries
library(tidyverse)
library(sf)
library(rio)
library(grid)
library(mast)
library(ggplot2)
library(ggpubr)
library(zoo)

# source functions
source('08_define_final_model/visualisation/code/00_source_function.R')
source('08_define_final_model/code/00_source_functions.R')

# Load data - complete one 
res_data <-  rio::import('08_define_final_model/output/som_final_toll_dr.rds')
res_data_u5 <- rio::import('08_define_final_model/output/som_final_toll_dr_u5.rds')

# Plot country results
# overall
f_country_dr_plot(res_data, resp_var='cdr')

# under 5
f_country_dr_plot(res_data_u5, resp_var = 'cdr_u5')

# Plot Map
# First need to aggreaget the data per district and select the date
# Load boostrapping 
boostrapping_results <-  rio::import('08_define_final_model/output/som_boostraping_data.rds') |>
  filter(date >= '2023-01-01' & date <= '2023-09-01')
boostrapping_results_u5 <- rio::import('08_define_final_model/output/som_boostraping_data_u5.rds')|>
  filter(date >= '2023-01-01' & date <= '2023-09-01')

# Generate the results with mean, low and upper data per district
final_res_dist <- f_generate_final_results(boostrapping_results, agg_level = 'district', 
                                      nb_boostrap = 1000, resp_var = 'cdr')
final_res_dist_u5 <- f_generate_final_results(boostrapping_results_u5, agg_level = 'district', 
                                           nb_boostrap = 1000, resp_var = 'cdr_u5')

# Generate the map
f_cdr_map(final_res_dist, 'cdr')
f_cdr_map(final_res_dist_u5, 'cdr_u5')


## Plot Map with CI

boostrapping_results <-  rio::import('08_define_final_model/output/som_boostraping_data.rds')
boostrapping_results_u5 <- rio::import('08_define_final_model/output/som_boostraping_data_u5.rds')

f_plot_country_plot_with_boostrap(boostrapping_results, 'glm_res_final_pop_runs', resp_var='n_died', 
                                  start_date_cf_a = "2015-01-01", 
                                  end_date_cf_a = "2016-06-01", start_date_cf_b = "2020-01-01", 
                                  end_date_cf_b = "2021-06-01", cf_b_label = "2015-16 counterfactual", 
                                  cf_a_label = "2020 counterfactual")

f_plot_country_plot_with_boostrap(boostrapping_results_u5, 'glm_res_final_u5_pop_runs', resp_var='n_died_u5', 
                                  start_date_cf_a = "2015-01-01", 
                                  end_date_cf_a = "2016-06-01", start_date_cf_b = "2020-01-01", 
                                  end_date_cf_b = "2021-06-01", cf_b_label = "2015-16 counterfactual", 
                                  cf_a_label = "2020 counterfactual")


# Generate results for Banadir
## TO BE DONE