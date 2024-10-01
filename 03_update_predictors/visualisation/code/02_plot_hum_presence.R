### HUMAN PRESENCE -------------------------------------------------------------

### ACLED DATA FATALITIES ------------------------------------------------------
acled_data <- rio::import('03_update_predictors/output/som_acled_data.rds')
plot_acled_data <- ggplot2::ggplot() + 
  ggplot2::geom_line(data=acled_data, 
                     ggplot2::aes(x=date, y = acled_fatalities), 
                     color='steelblue') +
  ggplot2::facet_wrap(~district, scales = "free_y") + 
  ggplot2::theme_bw() + 
  ggplot2::ggtitle('ACLED Fatalities')
ggplot2::ggsave("03_update_predictors/visualisation/output/som_acled_fatalities.png", bg = 'white',
                width = 16, height = 12)

acled_boxplot <- ggplot2::ggplot(acled_data, ggplot2::aes(x=as.factor(year), y=acled_fatalities)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Number Insecurity Fatalities') +
  ggplot2::xlab('Year') + 
  ggplot2::ggtitle('Box plot of the number of Insecurity Fatalities per year. Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_acled_fatilities_boxplot_year.png", bg = 'white',
                width = 16, height = 12)

### ACLED DATA FATALITIES ------------------------------------------------------
acled_data <- rio::import('03_update_predictors/output/som_acled_data.rds')
plot_acled_data <- ggplot2::ggplot() + 
  ggplot2::geom_line(data=acled_data, 
                     ggplot2::aes(x=date, y = acled_event), 
                     color='steelblue') +
  ggplot2::facet_wrap(~district, scales = "free_y") + 
  ggplot2::theme_bw() + 
  ggplot2::ggtitle('ACLED EVENTS')
ggplot2::ggsave("03_update_predictors/visualisation/output/som_acled_event.png", bg = 'white',
                width = 16, height = 12)

acled_boxplot <- ggplot2::ggplot(acled_data, ggplot2::aes(x=as.factor(year), y=acled_fatalities)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Number Insecurity Events') +
  ggplot2::xlab('Year') + 
  ggplot2::ggtitle('Box plot of the number of Insecurity Events per year. Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_acled_event_boxplot_year.png", bg = 'white',
                width = 16, height = 12)

### AID WORKERS DATA WOUNDED ---------------------------------------------------
aidworkers_data <- rio::import('03_update_predictors/output/som_aid_workers_data.rds')
plot_acled_data <- ggplot2::ggplot() + 
  ggplot2::geom_line(data=aidworkers_data, 
                     ggplot2::aes(x=date, y = aidworkers_wounded), 
                     color='steelblue') +
  ggplot2::facet_wrap(~district, scales = "free_y") + 
  ggplot2::theme_bw() + 
  ggplot2::ggtitle('Aid Workers Wounded')
ggplot2::ggsave("03_update_predictors/visualisation/output/som_aidworkers_wounded.png", bg = 'white',
                width = 16, height = 12)

aidworkers_boxplot <- ggplot2::ggplot(aidworkers_data, ggplot2::aes(x=as.factor(year), y=aidworkers_wounded)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Number Aid Workers Wounded') +
  ggplot2::xlab('Year') + 
  ggplot2::ggtitle('Box plot of the number of Aid Workers Wounded per year. Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_aidworker_wounded_boxplot_year.png", bg = 'white',
                width = 16, height = 12)

### AID WORKERS DATA KILLED ----------------------------------------------------
aidworkers_data <- rio::import('03_update_predictors/output/som_aid_workers_data.rds')
plot_acled_data <- ggplot2::ggplot() + 
  ggplot2::geom_line(data=aidworkers_data, 
                     ggplot2::aes(x=date, y = aidworkers_killed), 
                     color='steelblue') +
  ggplot2::facet_wrap(~district, scales = "free_y") + 
  ggplot2::theme_bw() + 
  ggplot2::ggtitle('Aid Workers Killed')
ggplot2::ggsave("03_update_predictors/visualisation/output/som_aidworkers_killed.png", bg = 'white',
                width = 16, height = 12)

aidworkers_boxplot <- ggplot2::ggplot(aidworkers_data, ggplot2::aes(x=as.factor(year), y=aidworkers_killed)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Number Aid Workers Killed') +
  ggplot2::xlab('Year') + 
  ggplot2::ggtitle('Box plot of the number of Aid Workers Killed per year. Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_aidworker_killed_boxplot_year.png", bg = 'white',
                width = 16, height = 12)

