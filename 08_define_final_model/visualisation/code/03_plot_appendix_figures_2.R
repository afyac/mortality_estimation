f_cdr_map <- function(data, date_, resp_var,
                      dir_output = '08_define_final_model/visualisation/output') {
  suppressWarnings({
    if (resp_var == "cdr") {
      leg_label <- paste0("crude death rate ", date_, "\n(per 10,000 person-days)")
      dr <- 'dr_mean'
      annot <- 'overall'
    } else if (resp_var == "cdr_u5") {
      leg_label <- "Mean Under 5y Death Rate 2023-24 \n(per 10,000 person-days)"
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
    
    p2 <- ggplot2::ggplot(data) +
      theme_bw(base_family = "serif") +
      geom_sf(aes(fill = .data[[dr]])) +
      geom_sf(
        fill = NA,
        color = "black",
        data = mast::som_shp$adm1,
        show.legend = FALSE,
        lwd = 0.6
      ) +
      ylim(-2, 13) +
      xlim(41, 51) +
      scale_fill_gradient(
        low = "white",
        high = "#8B4000",
        limits = c(0, 1.5),
        name = leg_label
      ) +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)
      )
    
    # p2 <- ggplot2::ggplot(data) +
    #   theme_bw(base_family = "serif") +
    #   ggplot2::geom_sf(ggplot2::aes(fill = dr_mean)) +
    #   # ggplot2::geom_sf_text(ggplot2::aes(label = dist_id), color = "grey20") +
    #   ggplot2::geom_sf(fill = NA, color = "black", data = mast::som_shp$adm1, show.legend = FALSE, lwd = 0.6) +
    #   #ggplot2::geom_rect(xmin = 44.5, ymin = 1, xmax = 46.5, ymax = 3, fill = NA, colour = "black", size = 0.6) +
    #   ggplot2::ylim(-2, 13) +
    #   ggplot2::xlim(41, 51) +
    #   ggplot2::theme_bw() +
    #   ggplot2::labs(color = "Districts", y = NULL, x = NULL) +
    #   ggplot2::scale_fill_gradient2(
    #     low = "white",   
    #     #mid = "#ffc966", # light grey or yellowish neutral
    #     high = "#8B4000",
    #     #midpoint = 0.25, 
    #     limits = c(0, 1.5),
    #     # trans = "sqrt",
    #     guide = ggplot2::guide_colorbar(
    #       barwidth = 11, title.position = "top",
    #       title.vjust = 1, title.hjust = 0.37
    #     ),
    #     name = leg_label
    #   ) +
    #   ggplot2::theme(
    #     legend.position = "top",
    #     legend.background = ggplot2::element_blank(),
    #     panel.grid = ggplot2::element_blank(),
    #     legend.text = ggplot2::element_text(size = 12),
    #     legend.title = ggplot2::element_text(size = 12),
    #     panel.background = ggplot2::element_blank(),
    #     legend.direction = "horizontal"
    #   ) +
    #   ggplot2::scale_color_manual(
    #     values = rep(c("black"), 74),
    #     guide = ggplot2::guide_legend(
    #       title.position = "top", title.hjust = 0.1,
    #       override.aes = list(
    #         color = "#00000000",
    #         fill = "#00000000"
    #       ), nrow = 37
    #     )
    #   )
    return(p2)
    # Create a version with just the fill legend and save as a grob
    p_fill <- p2 + ggplot2::guides(color = FALSE)
    
    g <- ggplot2::ggplotGrob(p_fill)
    
    # Create another version with just the color legend
    p_color <- p2 + ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold")
    ) + ggplot2::guides(fill = FALSE)
    
    
    # Add the fill legend into the color plot at a specific location
    p_color <- p_color + ggplot2::annotation_custom(
      grob = g$grobs[[which(g$layout$name == "guide-box-right")]],
      xmin = 2, xmax = 84.75, ymin = 0.5, ymax = 24.5
    )
    return(p_color)
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
        x = 0.60,
        y = 0.009,
        width = 0.26,
        height = 0.26, 
      )
    
    # ggplot2::ggsave(paste0(dir_output, "/som_out_cdr_map_", annot, ".png"),
    #                 dpi = "print", width = 30, height = 35, units = "cm"
    # )
  })
  
}

boostrapping_results <- rio::import('08_define_final_model/output/som_boostraping_data.rds') |>
  dplyr::filter(year > 2016 & year <2019)
data <- samrat::f_generate_final_results(boostrapping_results, 
                                 agg_level = 'district', 
                                 nb_boostrap = 1000, resp_var = 'cdr')

plot1 <- f_cdr_map(data, '2017-19', 'cdr')


boostrapping_results <- rio::import('08_define_final_model/output/som_boostraping_data.rds') |>
  dplyr::filter(year > 2022 & year <2024)
data_ <- samrat::f_generate_final_results(boostrapping_results, 
                                         agg_level = 'district', 
                                         nb_boostrap = 1000, resp_var = 'cdr')

plot2 <- f_cdr_map(data_, '2022-24', 'cdr')

# Combine the images side by side with labels
combined_plot <- plot1 + plot2 + plot_layout(widths = c(1, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "top",
    legend.margin = margin(b = -400), 
    plot.tag = element_text(margin = margin(b = -300))
  )

# Save or display the combined image
ggplot2::ggsave("08_define_final_model/visualisation/output/som_figure3_next_paper.png", combined_plot, width = 15, height = 15)


