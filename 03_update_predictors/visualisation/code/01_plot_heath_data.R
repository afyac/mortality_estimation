### MALARIA --------------------------------------------------------------------
#### Comparison WHO/FSNAU
malaria_fsnau <- rio::import('03_update_predictors/output/som_malaria_fsnau.rds')
malaria_who <- rio::import('03_update_predictors/output/som_malaria_who.rds')

## Comparison FSNAU and WHO ones
plot_fsnau_who <- ggplot2::ggplot() + ggplot2::geom_line(data=malaria_fsnau, ggplot2::aes(x=date, y = malaria_cases), color='red') + 
  ggplot2::geom_line(data=malaria_who, ggplot2::aes(x=date,y=malaria_cases), color = "blue") + 
  ggplot2::facet_wrap(~district, scales = "free_y")

ggplot2::ggsave("03_update_predictors/visualisation/output/som_malaria_who_fsnau.png", bg = 'white',
                width = 16, height = 12)

plot_fsnau_who <- ggplot2::ggplot() + ggplot2::geom_line(data=malaria_fsnau, ggplot2::aes(x=date, y = malaria_cases), color='red') + 
  ggplot2::geom_line(data=malaria_who, ggplot2::aes(x=date,y=malaria_cases), color = "blue") + 
  ggplot2::facet_wrap(~region, scales = "free_y")

ggplot2::ggsave("03_update_predictors/visualisation/output/som_malaria_who_fsnau_region.png", bg = 'white',
                width = 16, height = 12)

## BOXPLOT FSNAU
malaria_fsnau <- rio::import('03_update_predictors/output/som_malaria_fsnau.rds')
malaria_boxplot <- ggplot2::ggplot(malaria_fsnau, ggplot2::aes(x=as.factor(year), y=malaria_cases)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Number Malaria cases per 100.000') +
  ggplot2::xlab('Year') + 
  ggplot2::ggtitle('BBox plot of the number of malaria cases per year. Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_malaria_boxplot_year.png", bg = 'white',
                width = 16, height = 12)

malaria_boxplot <- ggplot2::ggplot(malaria_fsnau, ggplot2::aes(x=as.factor(month), y=malaria_cases)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Number Malaria cases per 100.000') +
  ggplot2::xlab('Month') + 
  ggplot2::ggtitle('BBox plot of the number of malaria cases per month Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_malaria_boxplot_month.png", bg = 'white',
                width = 16, height = 12)

### MEASLES --------------------------------------------------------------------
#### Plot Comparisons WHO/FSNAU

measles_fsnau <- rio::import('03_update_predictors/output/som_measles_fsnau.rds')
measles_who <- rio::import('03_update_predictors/output/som_measles_who.rds')

plot_fsnau_who <- ggplot2::ggplot() + ggplot2::geom_line(data=measles_fsnau, ggplot2::aes(x=date, y = measles_cases), color='red') +
  ggplot2::geom_line(data=measles_who, ggplot2::aes(x=date,y=measles_cases), color = "blue") +
  ggplot2::facet_wrap(~district, scales = "free_y")

ggplot2::ggsave("03_update_predictors/visualisation/output/som_measles_who_fsnau.png", bg = 'white',
       width = 16, height = 12)

plot_fsnau_who <- ggplot2::ggplot() + ggplot2::geom_line(data=measles_fsnau, ggplot2::aes(x=date, y = measles_cases), color='red') +
  ggplot2::geom_line(data=measles_who, ggplot2::aes(x=date,y=measles_cases), color = "blue") +
  ggplot2::facet_wrap(~region, scales = "free_y")

ggplot2::ggsave("03_update_predictors/visualisation/output/som_measles_who_fsnau_region.png", bg = 'white',
       width = 16, height = 12)

## BOXPLOT FSNAU
measles_fsnau <- rio::import('03_update_predictors/output/som_measles_fsnau.rds')
measles_boxplot <- ggplot2::ggplot(measles_fsnau, ggplot2::aes(x=as.factor(year), y=measles_cases)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Number Measles cases per 100.000') +
  ggplot2::xlab('Year') + 
  ggplot2::ggtitle('Box plot of the number of measles cases per year. Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_measles_boxplot_year.png", bg = 'white',
                width = 16, height = 12)

measles_boxplot <- ggplot2::ggplot(measles_fsnau, ggplot2::aes(x=as.factor(month), y=measles_cases)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Number Measles cases per 100.000') +
  ggplot2::xlab('Month') + 
  ggplot2::ggtitle('Box plot of the number of measles cases per month Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_measles_boxplot_month.png", bg = 'white',
                width = 16, height = 12)

### CHOLERA --------------------------------------------------------------------
#### Plot Over Years

cholera_fsnau <- rio::import('03_update_predictors/output/som_cholera_fsnau.rds')
plot_fsnau_who <- ggplot2::ggplot() + ggplot2::geom_line(data=cholera_fsnau, ggplot2::aes(x=date, y = cholera_cases), color='steelblue') +
  ggplot2::facet_wrap(~district, scales = "free_y")

ggplot2::ggsave("03_update_predictors/visualisation/output/som_cholera_fsnau.png", bg = 'white',
       width = 16, height = 12)

plot_fsnau_who <- ggplot2::ggplot() + ggplot2::geom_line(data=cholera_fsnau, ggplot2::aes(x=date, y = cholera_cases), color='steelblue') +
  ggplot2::facet_wrap(~region, scales = "free_y")

ggplot2::ggsave("03_update_predictors/visualisation/output/som_cholera_who_fsnau_region.png", bg = 'white',
       width = 16, height = 12)

## BOXPLOT FSNAU
cholera_fsnau <- rio::import('03_update_predictors/output/som_cholera_fsnau.rds')
cholera_boxplot <- ggplot2::ggplot(cholera_fsnau, ggplot2::aes(x=as.factor(year), y=cholera_cases)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Number Cholera cases per 100.000') +
  ggplot2::xlab('Year') + 
  ggplot2::ggtitle('Box plot of the number of cholera cases per year. Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_cholera_boxplot_year.png", bg = 'white',
                width = 16, height = 12)

cholera_boxplot <- ggplot2::ggplot(cholera_fsnau, ggplot2::aes(x=as.factor(month), y=cholera_cases)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Number Cholera cases per 100.000') +
  ggplot2::xlab('Month') + 
  ggplot2::ggtitle('Box plot of the number of cholera cases per month Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_cholera_boxplot_month.png", bg = 'white',
                width = 16, height = 12)


### IMMUNIZATION ---------------------------------------------------------------
#### Plot

immunization_measles <- rio::import('03_update_predictors/output/som_immunization_who.rds')|> 
  dplyr::filter(Antegin %in% c('Measles1', 'Measles2'))

plot_immunization <- ggplot2::ggplot() + ggplot2::geom_line(data=immunization_measles, ggplot2::aes(x=date, y = doses)) +
  ggplot2::facet_wrap(~district, scales = "free_y")

ggplot2::ggsave("03_update_predictors/visualisation/output/som_immunization_measles.png", bg = 'white',
                width = 16, height = 12)
