#### SAM DATA  --------------------------------------------------------------
## Plot ------------------------------------------------------------------------

sam_data <- rio::import('03_update_predictors/output/som_sam_data.rds')

# SAM --------------------------------------------------------------------------
plot_market_data <- ggplot2::ggplot() + 
  ggplot2::geom_line(data=sam_data, 
                     ggplot2::aes(x=date, y = sam_admissions), 
                     color='steelblue') +
  ggplot2::facet_wrap(~district, scales = "free_y") + 
  ggplot2::theme_bw() + 
  ggplot2::ggtitle('SAM ADMISSIONS')

ggplot2::ggsave("03_update_predictors/visualisation/output/som_sam_admissions.png", 
                bg = 'white', width = 16, height = 12)

sam_boxplot <- ggplot2::ggplot(sam_data, ggplot2::aes(x=as.factor(year), y=sam_admissions)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Number SAM Admissions') +
  ggplot2::xlab('Year') + 
  ggplot2::ggtitle('Box plot of the SAM Admission cases per year. Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_sam_boxplot_year.png", bg = 'white',
                width = 16, height = 12)