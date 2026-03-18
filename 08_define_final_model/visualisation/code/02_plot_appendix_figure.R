library(ggplot2)
library(dplyr)

df_u5_norm <- rio::import('08_define_final_model/output/som_final_toll_dr_u5.rds')
df_u5_incr_25 <- df_u5_norm |> dplyr::mutate(toll_mean_u5= toll_mean_u5*1.25,
                                                toll_low_u5 = toll_low_u5 *1.25,
                                                toll_up_u5  = toll_up_u5 *1.25)
df_u5_incr_50 <- df_u5_norm |> dplyr::mutate(toll_mean_u5= toll_mean_u5*1.5,
                                             toll_low_u5 = toll_low_u5 *1.5,
                                             toll_up_u5  = toll_up_u5 *1.5)

df_norm <- rio::import('08_define_final_model/output/som_final_toll_dr.rds')
df_incr_25 <- df_norm |> dplyr::left_join(df_u5_incr_25, by='date') |>
  dplyr::mutate(toll_mean = toll_mean + toll_mean_u5*0.25,
                toll_low = toll_low + toll_low_u5*0.25,
                toll_up = toll_up + toll_up_u5*0.25,
                )
df_incr_50 <-  df_norm |> dplyr::left_join(df_u5_incr_25, by='date') |>
  dplyr::mutate(toll_mean = toll_mean + toll_mean_u5*0.5,
                toll_low = toll_low + toll_low_u5*0.5,
                toll_up = toll_up + toll_up_u5*0.5,
  )

# Example: add a scenario column to each dataset
df_u5_norm <- df_u5_norm |> dplyr::mutate(scenario = "U5DR from surveys",
                                          age_group = 'u5')|>
  dplyr::rename(excess_death = toll_mean_u5, 
                lower = toll_low_u5, 
                upper = toll_up_u5)
df_u5_incr_25 <- df_u5_incr_25 |> dplyr::mutate(scenario = "Increased of 25%",
                                                age_group = 'u5')|>
  dplyr::rename(excess_death = toll_mean_u5, 
                lower = toll_low_u5, 
                upper = toll_up_u5)
df_u5_incr_50 <- df_u5_incr_50 |> dplyr::mutate(scenario = "Increased of 50%",
                                                age_group = 'u5')|>
  dplyr::rename(excess_death = toll_mean_u5, 
                lower = toll_low_u5, 
                upper = toll_up_u5)

df_norm <- df_norm |> dplyr::mutate(scenario = "U5DR from surveys",
                                    age_group = 'all') |>
  dplyr::rename(excess_death = toll_mean, 
                lower = toll_low, 
                upper = toll_up)
df_incr_25 <- df_incr_25 |> dplyr::mutate(scenario = "Increased of 25%",
                                          age_group = 'all')|>
  dplyr::rename(excess_death = toll_mean, 
                lower = toll_low, 
                upper = toll_up)
df_incr_50 <- df_incr_50 |> dplyr::mutate(scenario = "Increased of 50%",
                                          age_group = 'all')|>
  dplyr::rename(excess_death = toll_mean, 
                lower = toll_low, 
                upper = toll_up)

# Combine datasets
df_all <- bind_rows(df_norm, df_incr_25, df_incr_50, 
                    df_u5_norm, df_u5_incr_25, df_u5_incr_50)

# Summarize each dataset to get total excess deaths and CIs
df_summary <- df_all |>
  dplyr::mutate(year = lubridate::year(date)) |>
  dplyr::filter(year == 2023) |>
  group_by(age_group, scenario) |>
  summarise(
    excess_death = sum(excess_death),
    lower = sum(lower),
    upper = sum(upper),
    .groups = "drop"
  )

# Relabel scenarios for legend
df_summary <- df_summary |>
  mutate(scenario = factor(scenario,
                           levels = c("U5DR from surveys",
                                      "Increased of 25%",
                                      "Increased of 50%"),
                           labels = c("Main estimate",
                                      "25% underreporting of under 5y deaths",
                                      "50% underreporting of under 5y deaths")))

df_summary <- df_summary |>
  group_by(age_group) |>
  mutate(diff_from_main = excess_death - excess_death[scenario == "Main estimate"])

# Plot
plot <- ggplot(df_summary |> dplyr::filter(scenario != "Main estimate"),
       aes(x = age_group, y = diff_from_main, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(x = "Age group",
       y = "Difference in excess deaths compared to main estimates",
       fill = "Sensitivity assumption") +
  scale_fill_manual(values = c(
    "25% underreporting of under 5y deaths" = "#E69F00",
    "50% underreporting of under 5y deaths" = "#D55E00"
  ))+
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c("All ages", "Under 5")) +
  theme_bw(base_family = "serif") +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 14))


# Save figure
ggsave('08_define_final_model/visualisation/output/excess_deaths_comparison.png',
       plot = plot, width = 25, height = 12, units = "cm", dpi = 300, bg = "white")
