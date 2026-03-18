END_DATE_ANALYSIS <- '2024-06-01'
# Load data - complete one 
res_data <-  rio::import('08_define_final_model/output/som_final_toll_dr.rds')
res_data_u5 <- rio::import('08_define_final_model/output/som_final_toll_dr_u5.rds')

# Plot country results
# overall
samrat::f_country_dr_plot(res_data, resp_var='cdr')

# under 5
samrat::f_country_dr_plot(res_data_u5, resp_var = 'cdr_u5')

# Plot Map
# First need to aggreaget the data per district and select the date
# Load boostrapping 
boostrapping_results <-  rio::import('08_define_final_model/output/som_boostraping_data.rds') |>
  dplyr::filter(date >= '2023-01-01' & date <= END_DATE_ANALYSIS)
boostrapping_results_u5 <- rio::import('08_define_final_model/output/som_boostraping_data_u5.rds')|>
  dplyr::filter(date >= '2023-01-01' & date <= END_DATE_ANALYSIS)

## Plot Map with CI
boostrapping_results <-  rio::import('08_define_final_model/output/som_boostraping_data.rds')
boostrapping_results_u5 <- rio::import('08_define_final_model/output/som_boostraping_data_u5.rds')

path_output <- '08_define_final_model/visualisation/output/'

plot1 <- f_cdr_map(boostrapping_results, '2017-18' ,'cdr')


plot2 <- f_plot_country_plot_with_boostrap(boostrapping_results_u5, name_output ='glm_res_final_u5_pop_runs', resp_var='n_died_u5', 
                                                   start_date_cf_a = "2015-01-01",path_output = '08_define_final_model/visualisation/output/',
                                                   end_date_cf_a = "2016-06-01", start_date_cf_b = "2020-01-01", 
                                                   end_date_cf_b = "2021-06-01", cf_b_label = "2015 counterfactual", 
                                                   cf_a_label = "2020 counterfactual")

# Combine the images side by side with labels
combined_plot <- cowplot::plot_grid(
  plot1, plot2, 
  labels = c("a", "b"),  # Labels for the images
  label_size = 20,
  label_colour = "black",# Vertical adjustment of the labels
  ncol = 1,         # Font size of the labels
  label_x = 0.10,           # Horizontal adjustment of the labels
  label_y = 0.95
)

# Save or display the combined image
ggplot2::ggsave("08_define_final_model/visualisation/output/som_figure6_paper.png", combined_plot, width = 15, height = 15)

