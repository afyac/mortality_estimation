# Function to plot countrywide death rate evolution ---------------------------

f_country_dr_plot <- function(res_data, resp_var='cdr'){
      
      # Preparations ---------------------------------------------------------------
      # Load color 
      colours_f <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                     "#0072B2", "#D55E00", "#CC79A7")
          
      # Which response variable?
      if (resp_var %in% c("cdr", "n_died")) {resp_var <- "cdr"}
      if (resp_var %in% c("cdr_u5", "n_died_u5")) {resp_var <- "cdr_u5"}
      
      # Depending on which age group...
      if (resp_var == "cdr") {
        # plot label
        dr_lab <- "Estimated crude death rate (per 10,000 person-days)"
        annot <- 'overall'
        dr <- 'dr_mean'
      }
      
      if (resp_var == "cdr_u5") {
        # plot label
        dr_lab <- "Estimated under 5 years death rate (per 10,000 child-days)"
        annot <- 'u5'
        dr <- 'dr_mean_u5'
      }
      
      # Plot 1 - free y axis
      plot1 <- ggplot2::ggplot(data = res_data, ggplot2::aes(x = date, y = .data[[dr]])) + 
        ggplot2::geom_point(alpha = 0.3, size = 3, colour = colours_f[6]) +
        ggplot2::geom_line(ggplot2::aes(y = zoo::rollmean(.data[[dr]], k = 3, fill = NA, align = "right")),
                  alpha = 0.7, linewidth = 1, colour = colours_f[7]) +
        ggplot2::scale_y_continuous(dr_lab) +  
        ggplot2::scale_x_date("Year", date_breaks = "1 year", date_labels = "%Y") +
        ggplot2::theme_bw()
      
      # Plot 2 - y axis intersects at 0
      plot2 <- ggplot2::ggplot(data = res_data, ggplot2::aes(x = date, y = .data[[dr]])) + 
        ggplot2::geom_point(alpha = 0.3, size = 3, colour = colours_f[6]) +
        ggplot2::geom_line(ggplot2::aes(y = zoo::rollmean(.data[[dr]], k = 3, fill = NA, align = "right")),
                  alpha = 0.7, linewidth = 1, colour = colours_f[7]) +
        ggplot2::scale_y_continuous(dr_lab, limits = c(0, NA)) +  
        ggplot2::scale_x_date("Year", date_breaks = "1 year", date_labels = "%Y") +
        ggplot2::theme_bw()
      
      # Combine plots and save
      plot <- ggpubr::ggarrange(plot2, plot1, nrow = 2, align = "v")
      
      # save plot
      ggplot2::ggsave(paste("08_define_final_model/visualisation/output/som_country_level_", annot, '.png', sep=""),
             width = 10, height = 8, dpi = 320)
}

# Function to produce plot for CDR at district level ---------------------------

f_cdr_map <- function(data, resp_var) {
  suppressWarnings({
    if (resp_var == "cdr") {
      leg_label <- "Mean Crude Death Rate 2022-23 \n(per 10,000 person-days)"
      dr <- 'dr_mean'
      annot <- 'overall'
    } else if (resp_var == "cdr_u5") {
      leg_label <- "Mean Under 5y Death Rate 2022-23 \n(per 10,000 person-days)"
      dr <- 'dr_mean_u5'
      annot <- 'under5'
    } else {
      stop("Invalid type provided!")
    }
    
    # prepare dataset for plotting
    mast_data <- mast::som_shp$adm2[, c('OBJECTID_1', 'admin2RefN')]
    colnames(mast_data) <- c('dist_id', 'district', 'geometry')
    data <- merge(mast_data, data , by=c('district'), all.y = TRUE)
    data$dist_2 <- ifelse(data$dist_id < 10, paste0(data$dist_id, ": ", data$district), paste0(data$dist_id, ": ", data$district))
    data$dist_2 <- factor(data$dist_2[order(unique(data$dist_id))], levels = data$dist_2[order(unique(data$dist_id))])
    
    # Create the base plot
    p <- ggplot2::ggplot(data) +
      ggplot2::geom_sf(ggplot2::aes(color = dist_2, fill = .data[[dr]])) +
      ggplot2::geom_sf_text(ggplot2::aes(label = dist_id), color = "grey20") +
      ggplot2::geom_sf(fill = NA, color = "black", data = mast::som_shp$adm1, show.legend = FALSE, lwd = 0.6) +
      ggplot2::geom_rect(xmin = 44.5, ymin = 1, xmax = 46.5, ymax = 3, fill = NA, colour = "black", size = 0.6) +
      ggplot2::ylim(-2, 13) +
      ggplot2::xlim(41, 51) +
      ggplot2::theme_bw() +
      ggplot2::labs(color = "Districts", y = NULL, x = NULL) +
      ggplot2::scale_fill_gradient2(
        low = "#0055cc", high = "#cc0000", 
        # trans = "sqrt",
        guide = ggplot2::guide_colorbar(
          barwidth = 11, title.position = "top",
          title.vjust = 1, title.hjust = 0.37
        ),
        name = leg_label,
        midpoint = median(data[[dr]])
      ) +
      ggplot2::theme(
        legend.position = "right",
        legend.background = ggplot2::element_rect(color = NA, fill = NA, size = 0.2),
        panel.grid = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = "12"),
        legend.title = ggplot2::element_text(size = "12"),
        legend.direction = "horizontal"
      ) +
      ggplot2::scale_color_manual(
        values = rep(c("black"), 74),
        guide = ggplot2::guide_legend(
          title.position = "top", title.hjust = 0.1,
          override.aes = list(
            color = "#00000000",
            fill = "#00000000"
          ), nrow = 37
        )
      )
    
    # Create a version with just the fill legend and save as a grob
    p_fill <- p + ggplot2::guides(color = FALSE)
    
    g <- ggplot2::ggplotGrob(p_fill)
    
    # Create another version with just the color legend
    p_color <- p + ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold")
    ) + ggplot2::guides(fill = FALSE)
    
    # Add the fill legend into the color plot at a specific location
    p_color <- p_color + ggplot2::annotation_custom(
      grob = g$grobs[[which(g$layout$name == "guide-box")]],
      xmin = 1, xmax = 84.75, ymin = 0.935, ymax = 24.5
    )
    
    p_color <- p_color |>
      cowplot::ggdraw() +
      cowplot::draw_plot(
        {
          p_color +
            ggplot2::coord_sf(
              xlim = c(44.5, 46),
              ylim = c(1.5, 3),
              expand = FALSE
            ) +
            ggplot2::theme(
              legend.position = "none", axis.ticks = ggplot2::element_blank(),
              axis.text = ggplot2::element_blank()
            )
        },
        x = 0.40,
        y = 0.009,
        width = 0.26,
        height = 0.26
      )
    
  ggplot2::ggsave(paste0("08_define_final_model/visualisation/output/som_out_cdr_map_", annot, ".png"),
         dpi = "print", width = 30, height = 30, units = "cm", scale = 0.97
  )
  })
  return(p_color)
}


