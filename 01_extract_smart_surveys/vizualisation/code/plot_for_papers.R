## Then merge both to create a single image
# Load the cowplot package
library(cowplot)

# Load the two images
img1 <- cowplot::ggdraw() + cowplot::draw_image("01_extract_smart_surveys/vizualisation/output/som_cdr_per_year_type_idp.jpg")
img2 <- cowplot::ggdraw() + cowplot::draw_image("01_extract_smart_surveys/vizualisation/output/som_cdr_u5_per_year_type_idp.jpg")
# Combine the images side by side with labels
combined_plot <- plot_grid(
  img1, img2, 
  labels = c("a", "b"),  # Labels for the images
  label_size = 12,
  label_x = 0.05,          # Horizontal adjustment of the labels
  label_y = 0.95,
  ncol = 2,               # Number of columns (side by side)
  align = "v",            # Align images vertically
  vjust = 1, rel_widths = c(1, 1.5)             # Number of columns (side by side)
)

# Save or display the combined image
ggplot2::ggsave("01_extract_smart_surveys/vizualisation/output/som_figure1_paper.png", combined_plot, width = 10, height = 5)



# Load the two images
img1 <- cowplot::ggdraw() + cowplot::draw_image("01_extract_smart_surveys/vizualisation/output/som_plot_map.png")
img2 <- cowplot::ggdraw() + cowplot::draw_image("01_extract_smart_surveys/vizualisation/output/som_coverage_region.png")
library(patchwork)

combined_plot <- img1 + img2 +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(tag_levels = "a")

# Save or display the combined image
ggplot2::ggsave("01_extract_smart_surveys/vizualisation/output/som_figure2_paper.png", combined_plot, width = 10, height = 5)
