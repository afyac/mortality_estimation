#### MARKET DATA  --------------------------------------------------------------
## Plot ------------------------------------------------------------------------

market_data <- rio::import('03_update_predictors/output/som_markets_data.rds')

# TOT WAGE CEREAL SMOOTH -------------------------------------------------------
plot_market_data <- ggplot2::ggplot() + 
  ggplot2::geom_line(data=market_data, 
                     ggplot2::aes(x=date, y = tot_wage_cereal_smooth), 
                     color='steelblue') +
  ggplot2::facet_wrap(~district, scales = "free_y") + 
  ggplot2::theme_bw() + 
  ggplot2::ggtitle('TOT WAGE CEREAL')

ggplot2::ggsave("03_update_predictors/visualisation/output/som_tot_wage_cereal_smooth.png", 
                bg = 'white', width = 16, height = 12)

# TOT GOAT CEREAL SMOOTH -------------------------------------------------------
plot_market_data <- ggplot2::ggplot() + 
  ggplot2::geom_line(data=market_data, 
                     ggplot2::aes(x=date, y = tot_goat_cereal_smooth), 
                     color='steelblue') +
  ggplot2::facet_wrap(~district, scales = "free_y") + 
  ggplot2::theme_bw() + 
  ggplot2::ggtitle('TOT GOAT CEREAL')

ggplot2::ggsave("03_update_predictors/visualisation/output/som_tot_goat_cereal_smooth.png", 
                bg = 'white', width = 16, height = 12)

market_boxplot <- ggplot2::ggplot(market_data, ggplot2::aes(x=as.factor(year), y=tot_wage_cereal_smooth)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Terms of trade purchasing power index (Cereal/Wage)') +
  ggplot2::xlab('Year') + 
  ggplot2::ggtitle('Box plot of the Terms of trade purchasing power index (Cereal/Wage) per year. Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_market_wage_cereal_boxplot_year.png", bg = 'white',
                width = 16, height = 12)

market_boxplot <- ggplot2::ggplot(market_data, ggplot2::aes(x=as.factor(year), y=tot_goat_cereal_smooth)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Terms of trade purchasing power index (Goat/Cereal)') +
  ggplot2::xlab('Year') + 
  ggplot2::ggtitle('Box plot of the Terms of trade purchasing power index (Goat/Cereal) per year. Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_market_goat_cereal_boxplot_year.png", bg = 'white',
                width = 16, height = 12)

market_boxplot <- ggplot2::ggplot(market_data, ggplot2::aes(x=as.factor(year), y=water_price)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::theme_bw() + 
  ggplot2::ylab('Water price') +
  ggplot2::xlab('Year') + 
  ggplot2::ggtitle('Box plot of the Water Price per year. Each point corresponds to a district.') +
  ggplot2::theme(axis.text.x=ggplot2::element_text(size=15), axis.text.y = ggplot2::element_text(size=15), 
                 axis.title =ggplot2::element_text(siz=17), title = ggplot2::element_text(size=20))
ggplot2::ggsave("03_update_predictors/visualisation/output/som_water_price_boxplot_year.png", bg = 'white',
                width = 16, height = 12)