#Plot treatment maps by cohort

# ----------------------------
# Apply first treatment date to all municipios in san pedro sula and distrito central
# ----------------------------

#merged <- merged %>%
#  mutate(first_treatment = ifelse(Municipio == "san pedro sula" | Municipio == "distrito central", "6_12_2022", first_treatment)) %>%
#  mutate(t_day = ifelse(Municipio == "san pedro sula" | Municipio == "distrito central", 6, t_day)) %>%
#  mutate(t_month = ifelse(Municipio == "san pedro sula" | Municipio == "distrito central", 12, t_month)) %>%
#  mutate(t_year = ifelse(Municipio == "san pedro sula" | Municipio == "distrito central", 2022, t_year))

# ----------------------------
#Define single treatments
# ----------------------------

#merged <- merged %>%
#  mutate(treat_1 = ifelse(Municipio == "san pedro sula" | Municipio == "distrito central", 1, 0)) %>%
#  mutate(treat_2 = ifelse(!is.na(first_treatment) & t_day == 6 & t_month == 1 & t_year == 2023, 1, 0)) %>%
#  mutate(treat_3 = ifelse(!is.na(first_treatment) & t_day == 20 & t_month == 2 & t_year == 2023, 1, 0)) %>%
#  mutate(treat_4 = ifelse(!is.na(first_treatment) & t_day == 19 & t_month == 8 & t_year == 2023, 1, 0)) %>%
#  mutate(treat_5 = ifelse(!is.na(first_treatment) & t_day == 1 & t_month == 1 & t_year == 2024, 1, 0)) %>%
#  mutate(treat_6 = ifelse(!is.na(first_treatment) & t_day == 19 & t_month == 2 & t_year == 2024, 1, 0))

# ----------------------------
# Label treatment cohorts
# ----------------------------

merged <- merged %>%
  mutate(
    treat_count = treat_1 + treat_2 + treat_3 + treat_4 + treat_5 + treat_6,
    treat_group = case_when(
      treat_count >= 2 ~ "Multiple",
      treat_1 == 1 ~ "Group 1: 2022-12-06",
      treat_2 == 1 ~ "Group 2: 2023-01-06",
      treat_3 == 1 ~ "Group 3: 2023-02-20",
      treat_4 == 1 ~ "Group 4: 2023-08-19",
      treat_5 == 1 ~ "Group 5: 2024-01-01",
      treat_6 == 1 ~ "Group 6: 2024-02-19",
      TRUE ~ "Untreated"
    )
  )

# ----------------------------
# Define cumulative map indicators
# ----------------------------

merged <- merged %>%
  mutate(
    show_1     = treat_1 >= 1,
    show_12    = treat_1 + treat_2 >= 1,
    show_123   = treat_1 + treat_2 + treat_3 >= 1,
    show_1234  = treat_1 + treat_2 + treat_3 + treat_4 >= 1,
    show_12345 = treat_1 + treat_2 + treat_3 + treat_4 + treat_5 >= 1,
    show_total = treat_1 + treat_2 + treat_3 + treat_4 + treat_5 + treat_6 >= 1
  )

# ----------------------------
# Map theme
# ----------------------------

map_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.key.size = grid::unit(0.25, "cm"),
    legend.spacing.y = grid::unit(0.1, "cm"),
    plot.title = element_text(hjust = 0.5)
  )

# ----------------------------
# Treatment cohort colors
# ----------------------------

cohort_colors <- c(
  "Untreated" = "white",
  "Group 1: 2022-12-06" = "#1f77b4",
  "Group 2: 2023-01-06" = "#ff7f0e",
  "Group 3: 2023-02-20" = "#2ca02c",
  "Group 4: 2023-08-19" = "#d62728",
  "Group 5: 2024-01-01" = "#9467bd",
  "Group 6: 2024-02-19" = "#8c564b",
  "Multiple" = "#000000"
)

# ----------------------------
# Function to plot treatment maps
# ----------------------------

plot_treat_map <- function(df, show_var, title = NULL) {
  
  df_plot <- df %>%
    mutate(
      map_fill = ifelse(.data[[show_var]], treat_group, "Untreated"),
      map_fill = factor(map_fill, levels = names(cohort_colors))
    )
  
  ggplot(df_plot) +
    geom_sf(
      aes(fill = map_fill),
      color = "black",
      linewidth = 0.02
    ) +
    scale_fill_manual(
      values = cohort_colors,
      breaks = setdiff(names(cohort_colors), "Multiple"),
      drop = FALSE
    ) +
    labs(
      title = title,
      fill = "Treatment cohort"
    ) +
    coord_sf(
      xlim = c(-90.2, -82.5),
      ylim = c(12.8, 17.5),
      expand = FALSE
    ) +
    map_theme +
    theme(
      legend.position = c(0.98, 0.02),
      legend.justification = c(1, 0)
    )
}

# ----------------------------
# Generate maps
# ----------------------------

treat_map_1 <- plot_treat_map(merged, "show_1")
treat_map_12 <- plot_treat_map(merged, "show_12")
treat_map_123 <- plot_treat_map(merged, "show_123")
treat_map_1234 <- plot_treat_map(merged, "show_1234")
treat_map_12345 <- plot_treat_map(merged, "show_12345")
treat_map_total <- plot_treat_map(merged, "show_total")

# ----------------------------
# Save maps
# ----------------------------

ggsave(
  filename = file.path(output, "maps", "treat_map_1.png"),
  plot = treat_map_1,
  width = 6,
  height = 4,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(output, "maps", "treat_map_12.png"),
  plot = treat_map_12,
  width = 6,
  height = 4,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(output, "maps", "treat_map_123.png"),
  plot = treat_map_123,
  width = 6,
  height = 4,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(output, "maps", "treat_map_1234.png"),
  plot = treat_map_1234,
  width = 6,
  height = 4,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(output, "maps", "treat_map_12345.png"),
  plot = treat_map_12345,
  width = 6,
  height = 4,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(output, "maps", "treat_map_total.png"),
  plot = treat_map_total,
  width = 6,
  height = 4,
  dpi = 300,
  bg = "white"
)

rm(adm_unmatched, correct, merged, treat, treat_unmatched,
   cohort_colors, map_theme, plot_treat_map, treat_map_1, 
   treat_map_12, treat_map_123, treat_map_1234, treat_map_12345, treat_map_total)

print("Treatment maps created")
print("0_treatment_maps.R run completed")
