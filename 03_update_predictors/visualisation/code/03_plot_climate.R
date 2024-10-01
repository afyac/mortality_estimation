#### CLIMATE -------------------------------------------------------------------
## Plot ------------------------------------------------------------------------

climate_data <- rio::import('03_update_predictors/output/som_climate_data.rds')

# Rainfall Rollmean
plot_rainfall <- ggplot2::ggplot() + 
  ggplot2::geom_line(data=climate_data, ggplot2::aes(x=date, y = rainfall_rollmean), color='steelblue') +
  ggplot2::facet_wrap(~district, scales = "free_y") + 
  ggplot2::theme_bw()
ggplot2::ggsave("03_update_predictors/visualisation/output/som_rainfall_rollmean.png", 
                bg = 'white', width = 16, height = 12)

# CDI
plot_cdi <- ggplot2::ggplot() + 
  ggplot2::geom_line(data=climate_data, ggplot2::aes(x=date, y = cdi), color='steelblue') +
  ggplot2::facet_wrap(~district, scales = "free_y")+ 
  ggplot2::theme_bw()
ggplot2::ggsave("03_update_predictors/visualisation/output/som_cdi.png", 
                bg = 'white', width = 16, height = 12)

climate_boxplot <- ggplot2::ggplot(climate_data, ggplot2::aes(x=as.factor(year), y=cdi)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Combined Drough Index') +
  ggplot2::xlab('Year') + 
  ggplot2::ggtitle('Box plot of the Combined Drough Index per year. Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_cdi_boxplot_year.png", bg = 'white',
                width = 16, height = 12)