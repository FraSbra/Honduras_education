# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Groups summary stats
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#----------------------
#Load data
#----------------------
df <- read.csv(file.path(data, "processed", "csv", "center_level.csv")) #Treatment data

#Remove "Second date" variable
df$second_date <- NULL

#Snap for inspecting df
snap <- head(df, 1000)
  
#----------------------
# Collapse at center x T_group level
#----------------------

# Group by treatment group and collapse (FOR ALL 3x3 OUTCOMES)
# Generate mean dropout rate and 95% confidence interval

df_collapsed <- df |> #For total dropout
  filter(!is.na(T_group)) |>
  group_by(T_group, year_school) |>
  summarise(
    n_diff = sum(!is.na(diff)),
    mean_diff = if_else(n_diff > 0, mean(diff, na.rm = TRUE), NA_real_),
    se_diff = if_else(n_diff > 1, sd(diff, na.rm = TRUE) / sqrt(n_diff), NA_real_),
    ci_lower_diff = if_else(
      n_diff > 1,
      mean_diff - qt(0.975, df = n_diff - 1) * se_diff,
      NA_real_
    ),
    ci_upper_diff = if_else(
      n_diff > 1,
      mean_diff + qt(0.975, df = n_diff - 1) * se_diff,
      NA_real_
    ),
    n_dr = sum(!is.na(dr)),
    mean_dr = if_else(n_dr > 0, mean(dr, na.rm = TRUE), NA_real_),
    se_dr = if_else(n_dr > 1, sd(dr, na.rm = TRUE) / sqrt(n_dr), NA_real_),
    ci_lower_dr = if_else(
      n_dr > 1,
      mean_dr - qt(0.975, df = n_dr - 1) * se_dr,
      NA_real_
    ),
    ci_upper_dr = if_else(
      n_dr > 1,
      mean_dr + qt(0.975, df = n_dr - 1) * se_dr,
      NA_real_
    ),
    n_canc = sum(!is.na(canc)),
    mean_canc = if_else(
      n_canc > 0,
      mean(canc, na.rm = TRUE),
      NA_real_
    ),
    se_canc = if_else(
      n_canc > 1,
      sd(canc, na.rm = TRUE) / sqrt(n_canc),
      NA_real_
    ),
    ci_lower_canc = if_else(
      n_canc > 1,
      mean_canc - qt(0.975, df = n_canc - 1) * se_canc,
      NA_real_
    ),
    ci_upper_canc = if_else(
      n_canc > 1,
      mean_canc + qt(0.975, df = n_canc - 1) * se_canc,
      NA_real_
    ),
    .groups = "drop"
  )

df_w_collapsed <- df |> # For female dropout
  filter(!is.na(T_group)) |>
  group_by(T_group, year_school) |>
  summarise(
    n_diff = sum(!is.na(diff_w)),
    mean_diff = if_else(n_diff > 0, mean(diff_w, na.rm = TRUE), NA_real_),
    se_diff = if_else(n_diff > 1, sd(diff_w, na.rm = TRUE) / sqrt(n_diff), NA_real_),
    ci_lower_diff = if_else(
      n_diff > 1,
      mean_diff - qt(0.975, df = n_diff - 1) * se_diff,
      NA_real_
    ),
    ci_upper_diff = if_else(
      n_diff > 1,
      mean_diff + qt(0.975, df = n_diff - 1) * se_diff,
      NA_real_
    ),

    n_dr = sum(!is.na(dr_w)),
    mean_dr = if_else(n_dr > 0, mean(dr_w, na.rm = TRUE), NA_real_),
    se_dr = if_else(n_dr > 1, sd(dr_w, na.rm = TRUE) / sqrt(n_dr), NA_real_),
    ci_lower_dr = if_else(
      n_dr > 1,
      mean_dr - qt(0.975, df = n_dr - 1) * se_dr,
      NA_real_
    ),
    ci_upper_dr = if_else(
      n_dr > 1,
      mean_dr + qt(0.975, df = n_dr - 1) * se_dr,
      NA_real_
    ),

    n_canc = sum(!is.na(canc_w)),
    mean_canc = if_else(n_canc > 0, mean(canc_w, na.rm = TRUE), NA_real_),
    se_canc = if_else(n_canc > 1, sd(canc_w, na.rm = TRUE) / sqrt(n_canc), NA_real_),
    ci_lower_canc = if_else(
      n_canc > 1,
      mean_canc - qt(0.975, df = n_canc - 1) * se_canc,
      NA_real_
    ),
    ci_upper_canc = if_else(
      n_canc > 1,
      mean_canc + qt(0.975, df = n_canc - 1) * se_canc,
      NA_real_
    ),

    .groups = "drop"
  )

df_m_collapsed <- df |> # For male dropout
  filter(!is.na(T_group)) |>
  group_by(T_group, year_school) |>
  summarise(
    n_diff = sum(!is.na(diff_m)),
    mean_diff = if_else(n_diff > 0, mean(diff_m, na.rm = TRUE), NA_real_),
    se_diff = if_else(n_diff > 1, sd(diff_m, na.rm = TRUE) / sqrt(n_diff), NA_real_),
    ci_lower_diff = if_else(
      n_diff > 1,
      mean_diff - qt(0.975, df = n_diff - 1) * se_diff,
      NA_real_
    ),
    ci_upper_diff = if_else(
      n_diff > 1,
      mean_diff + qt(0.975, df = n_diff - 1) * se_diff,
      NA_real_
    ),

    n_dr = sum(!is.na(dr_m)),
    mean_dr = if_else(n_dr > 0, mean(dr_m, na.rm = TRUE), NA_real_),
    se_dr = if_else(n_dr > 1, sd(dr_m, na.rm = TRUE) / sqrt(n_dr), NA_real_),
    ci_lower_dr = if_else(
      n_dr > 1,
      mean_dr - qt(0.975, df = n_dr - 1) * se_dr,
      NA_real_
    ),
    ci_upper_dr = if_else(
      n_dr > 1,
      mean_dr + qt(0.975, df = n_dr - 1) * se_dr,
      NA_real_
    ),

    n_canc = sum(!is.na(canc_m)),
    mean_canc = if_else(n_canc > 0, mean(canc_m, na.rm = TRUE), NA_real_),
    se_canc = if_else(n_canc > 1, sd(canc_m, na.rm = TRUE) / sqrt(n_canc), NA_real_),
    ci_lower_canc = if_else(
      n_canc > 1,
      mean_canc - qt(0.975, df = n_canc - 1) * se_canc,
      NA_real_
    ),
    ci_upper_canc = if_else(
      n_canc > 1,
      mean_canc + qt(0.975, df = n_canc - 1) * se_canc,
      NA_real_
    ),

    .groups = "drop"
  )

# Same, but one group alone at the time with controls
for (x in 1:6) {
  assign(
    paste0("df_collapsed_", x),
    df |>
      filter(T_group == .env$x | T_group == 0) |>
      group_by(T_group, year_school) |>
      summarise(
        n_diff = sum(!is.na(diff)),
        mean_diff = if_else(n_diff > 0, mean(diff, na.rm = TRUE), NA_real_),
        se_diff = if_else(n_diff > 1, sd(diff, na.rm = TRUE) / sqrt(n_diff), NA_real_),
        ci_lower_diff = if_else(
          n_diff > 1,
          mean_diff - qt(0.975, df = n_diff - 1) * se_diff,
          NA_real_
        ),
        ci_upper_diff = if_else(
          n_diff > 1,
          mean_diff + qt(0.975, df = n_diff - 1) * se_diff,
          NA_real_
        ),

        n_dr = sum(!is.na(dr)),
        mean_dr = if_else(n_dr > 0, mean(dr, na.rm = TRUE), NA_real_),
        se_dr = if_else(n_dr > 1, sd(dr, na.rm = TRUE) / sqrt(n_dr), NA_real_),
        ci_lower_dr = if_else(
          n_dr > 1,
          mean_dr - qt(0.975, df = n_dr - 1) * se_dr,
          NA_real_
        ),
        ci_upper_dr = if_else(
          n_dr > 1,
          mean_dr + qt(0.975, df = n_dr - 1) * se_dr,
          NA_real_
        ),

        n_canc = sum(!is.na(canc)),
        mean_canc = if_else(
          n_canc > 0,
          mean(canc, na.rm = TRUE),
          NA_real_
        ),
        se_canc = if_else(
          n_canc > 1,
          sd(canc, na.rm = TRUE) / sqrt(n_canc),
          NA_real_
        ),
        ci_lower_canc = if_else(
          n_canc > 1,
          mean_canc - qt(0.975, df = n_canc - 1) * se_canc,
          NA_real_
        ),
        ci_upper_canc = if_else(
          n_canc > 1,
          mean_canc + qt(0.975, df = n_canc - 1) * se_canc,
          NA_real_
        ),

        .groups = "drop"
      )
  )
}

# For female dropout
for (x in 1:6) {
  assign(
    paste0("df_w_collapsed_", x),
    df |>
      filter(T_group == .env$x | T_group == 0) |>
      group_by(T_group, year_school) |>
      summarise(
        n_diff = sum(!is.na(diff_w)),
        mean_diff = if_else(n_diff > 0, mean(diff_w, na.rm = TRUE), NA_real_),
        se_diff = if_else(n_diff > 1, sd(diff_w, na.rm = TRUE) / sqrt(n_diff), NA_real_),
        ci_lower_diff = if_else(
          n_diff > 1,
          mean_diff - qt(0.975, df = n_diff - 1) * se_diff,
          NA_real_
        ),
        ci_upper_diff = if_else(
          n_diff > 1,
          mean_diff + qt(0.975, df = n_diff - 1) * se_diff,
          NA_real_
        ),

        n_dr = sum(!is.na(dr_w)),
        mean_dr = if_else(n_dr > 0, mean(dr_w, na.rm = TRUE), NA_real_),
        se_dr = if_else(n_dr > 1, sd(dr_w, na.rm = TRUE) / sqrt(n_dr), NA_real_),
        ci_lower_dr = if_else(
          n_dr > 1,
          mean_dr - qt(0.975, df = n_dr - 1) * se_dr,
          NA_real_
        ),
        ci_upper_dr = if_else(
          n_dr > 1,
          mean_dr + qt(0.975, df = n_dr - 1) * se_dr,
          NA_real_
        ),

        n_canc = sum(!is.na(canc_w)),
        mean_canc = if_else(
          n_canc > 0,
          mean(canc_w, na.rm = TRUE),
          NA_real_
        ),
        se_canc = if_else(
          n_canc > 1,
          sd(canc_w, na.rm = TRUE) / sqrt(n_canc),
          NA_real_
        ),
        ci_lower_canc = if_else(
          n_canc > 1,
          mean_canc - qt(0.975, df = n_canc - 1) * se_canc,
          NA_real_
        ),
        ci_upper_canc = if_else(
          n_canc > 1,
          mean_canc + qt(0.975, df = n_canc - 1) * se_canc,
          NA_real_
        ),

        .groups = "drop"
      )
  )
}

# For male dropout
for (x in 1:6) {
  assign(
    paste0("df_m_collapsed_", x),
    df |>
      filter(T_group == .env$x | T_group == 0) |>
      group_by(T_group, year_school) |>
      summarise(
        n_diff = sum(!is.na(diff_m)),
        mean_diff = if_else(n_diff > 0, mean(diff_m, na.rm = TRUE), NA_real_),
        se_diff = if_else(n_diff > 1, sd(diff_m, na.rm = TRUE) / sqrt(n_diff), NA_real_),
        ci_lower_diff = if_else(
          n_diff > 1,
          mean_diff - qt(0.975, df = n_diff - 1) * se_diff,
          NA_real_
        ),
        ci_upper_diff = if_else(
          n_diff > 1,
          mean_diff + qt(0.975, df = n_diff - 1) * se_diff,
          NA_real_
        ),

        n_dr = sum(!is.na(dr_m)),
        mean_dr = if_else(n_dr > 0, mean(dr_m, na.rm = TRUE), NA_real_),
        se_dr = if_else(n_dr > 1, sd(dr_m, na.rm = TRUE) / sqrt(n_dr), NA_real_),
        ci_lower_dr = if_else(
          n_dr > 1,
          mean_dr - qt(0.975, df = n_dr - 1) * se_dr,
          NA_real_
        ),
        ci_upper_dr = if_else(
          n_dr > 1,
          mean_dr + qt(0.975, df = n_dr - 1) * se_dr,
          NA_real_
        ),

        n_canc = sum(!is.na(canc_m)),
        mean_canc = if_else(
          n_canc > 0,
          mean(canc_m, na.rm = TRUE),
          NA_real_
        ),
        se_canc = if_else(
          n_canc > 1,
          sd(canc_m, na.rm = TRUE) / sqrt(n_canc),
          NA_real_
        ),
        ci_lower_canc = if_else(
          n_canc > 1,
          mean_canc - qt(0.975, df = n_canc - 1) * se_canc,
          NA_real_
        ),
        ci_upper_canc = if_else(
          n_canc > 1,
          mean_canc + qt(0.975, df = n_canc - 1) * se_canc,
          NA_real_
        ),

        .groups = "drop"
      )
  )
}

#----------------------
# Plot mean dropouts
#----------------------

#Plot (line plot) mean with confidence interval for each group
#3x3 plots, total, male and female + the 3 outcomes

group_colors <- c(
  "0" = "black",
  "1" = "red",
  "2" = "blue",
  "3" = "green",
  "4" = "orange",
  "5" = "purple",
  "6" = "brown"
)

group_labels <- c(
  "0" = "Control",
  "1" = "Treatment 1",
  "2" = "Treatment 2",
  "3" = "Treatment 3",
  "4" = "Treatment 4",
  "5" = "Treatment 5",
  "6" = "Treatment 6"
)



outcomes <- c("diff", "dr", "canc")
ci_lower <- c("ci_lower_diff", "ci_lower_dr", "ci_lower_canc")
ci_upper <- c("ci_upper_diff", "ci_upper_dr", "ci_upper_canc")

for (x in 1:3) {

  out <- outcomes[[x]]
  cil <- ci_lower[[x]]
  ciu <- ci_upper[[x]]

  p <- df_collapsed |> #Total dropout plot
  ggplot(
    aes(
      x = year_school,
      y = .data[[paste0("mean_", out)]],
      color = factor(T_group),
      fill = factor(T_group),
      group = T_group
    )
  ) +
  geom_ribbon(
    aes(ymin = .data[[cil]], ymax = .data[[ciu]]),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 0.8) +
  geom_point() +
  scale_color_manual(
    name = "Treatment group",
    values = group_colors,
    labels = group_labels
  ) +
  scale_fill_manual(
    values = group_colors,
    guide = "none"
  ) +
  theme_test() +
  xlab("Year") +
  ylab("Mean Dropout")


  ggsave(
    file.path(output, "figures", "summary_stats", "mean_outcome", paste0("mean_", out , "_all.jpeg")),
    plot = p,
    device = "jpeg",
    path = NULL,
    scale = 1,
    width = NA,
    height = NA
  )
  


  p <- df_w_collapsed |> #Female dropout plot
  ggplot(
    aes(
      x = year_school,
      y = .data[[paste0("mean_", out)]],
      color = factor(T_group),
      fill = factor(T_group),
      group = T_group
    )
  ) +
  geom_ribbon(
    aes(ymin = .data[[cil]], ymax = .data[[ciu]]),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 0.8) +
  geom_point() +
  scale_color_manual(
    name = "Treatment group",
    values = group_colors,
    labels = group_labels
  ) +
  scale_fill_manual(
    values = group_colors,
    guide = "none"
  ) +
  theme_test() +
  xlab("Year") +
  ylab("Mean Dropout (Female)")

  ggsave(
  file.path(output, "figures", "summary_stats", "mean_outcome", paste0("mean_", out , "_female_all.jpeg")),
  plot = p,
  device = "jpeg",
  path = NULL,
  scale = 1,
  width = NA,
  height = NA
  )



  p <- df_m_collapsed |> #male dropout plot
  ggplot(
    aes(
      x = year_school,
      y = .data[[paste0("mean_", out)]],
      color = factor(T_group),
      fill = factor(T_group),
      group = T_group
    )
  ) +
  geom_ribbon(
    aes(ymin = .data[[cil]], ymax = .data[[ciu]]),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 0.8) +
  geom_point() +
  scale_color_manual(
    name = "Treatment group",
    values = group_colors,
    labels = group_labels
  ) +
  scale_fill_manual(
    values = group_colors,
    guide = "none"
  ) +
  theme_test() +
  xlab("Year") +
  ylab("Mean Dropout (Male)")

  ggsave(
  file.path(output, "figures", "summary_stats", "mean_outcome", paste0("mean_", out , "_male_all.jpeg")),
  plot = p,
  device = "jpeg",
  path = NULL,
  scale = 1,
  width = NA,
  height = NA
)

}

#Same, loop for each group + controls only
#6 x 3 plots in total
datasets <- mget(paste0("df_collapsed_", 1:6))
datasets_w <- mget(paste0("df_w_collapsed_", 1:6))
datasets_m <- mget(paste0("df_m_collapsed_", 1:6))

outcomes <- c("diff", "dr", "canc")
ci_lower <- c("ci_lower_diff", "ci_lower_dr", "ci_lower_canc")
ci_upper <- c("ci_upper_diff", "ci_upper_dr", "ci_upper_canc")

total <- list(
  datasets = datasets,
  datasets_w = datasets_w,
  datasets_m = datasets_m
)

for (group in names(total)) {

  dat <- total[[group]]

  y_label <- switch(
    group,
    datasets = "Mean Dropout",
    datasets_w = "Mean Dropout (Female)",
    datasets_m = "Mean Dropout (Male)"
  )

  file_label <- switch(
    group,
    datasets = "total",
    datasets_w = "female",
    datasets_m = "male"
  )

  for (i in seq_along(dat)) {

    x <- dat[[i]]

    for (l in 1:3) {

      out <- outcomes[[l]]
      cil <- ci_lower[[l]]
      ciu <- ci_upper[[l]]


    p <- x |>
      ggplot(
        aes(
          x = year_school,
          y = .data[[out]],
          color = factor(T_group),
          fill = factor(T_group),
          group = T_group
        )
      ) +
      geom_ribbon(
        aes(ymin = .data[[cil]], ymax = .data[[ciu]]),
        alpha = 0.2,
        color = NA
      ) +
      geom_line(linewidth = 0.8) +
      geom_point() +
      scale_color_manual(
        name = "Treatment group",
        values = group_colors,
        labels = group_labels
      ) +
      scale_fill_manual(
        values = group_colors,
        guide = "none"
      ) +
      theme_test() +
      xlab("Year") +
      ylab(y_label)

    print(p)

    ggsave(
      file.path(
        output, "figures", "summary_stats", "mean_outcome",
        paste0("mean_", out, "_", file_label, "_", i, ".jpeg")
      ),
      plot = p,
      device = "jpeg",
      scale = 1,
      width = NA,
      height = NA
    )
    
    }
  }
}

#----------------------------
# Plot dropout distribution
#----------------------------

suffixes <- c("", "_w", "_m")

for (x in suffixes) { #Loops over all types of plots, for dr, dr_w e dr_m

  var <- paste0("diff", x)

  p <- df |> #Density plot with no 0
  dplyr::filter(.data[[var]] != 0) |>
  ggplot(
    aes(
      x = .data[[var]],
      color = factor(T_group),
      fill = factor(T_group)
    )
  ) +
  geom_density(
    alpha = 0.25,
    linewidth = 1,
    adjust = 0.5
  ) +
  scale_color_manual(
    name = "Treatment group",
    values = group_colors,
    labels = group_labels
  ) +
  scale_fill_manual(
    values = group_colors,
    guide = "none"
  ) +
  theme_test() +
  xlab("Total Dropout rate") +
  ylab("Density")
  
  print(p)

  ggsave(
  file.path(output, "figures", "summary_stats", "outcome_distribution", paste0("density_", var, "_no_zeros.jpeg")),
  plot = p,
  device = "jpeg",
  path = NULL,
  scale = 1,
  width = NA,
  height = NA
  )



  p <- df |> #Density plot with no 0, max dr = 0.5
  dplyr::filter(.data[[var]] != 0) |>
  ggplot(
    aes(
      x = .data[[var]],
      color = factor(T_group),
      fill = factor(T_group)
    )
  ) +
  geom_density(
    alpha = 0.25,
    linewidth = 1,
    adjust = 0.5
  ) +
  scale_color_manual(
    name = "Treatment group",
    values = group_colors,
    labels = group_labels
  ) +
  scale_fill_manual(
    values = group_colors,
    guide = "none"
  ) +
  theme_test() +
    coord_cartesian(
    xlim = c(0, 0.5)) +
  xlab("Total Dropout rate") +
  ylab("Density")
  
  print(p)

  ggsave(
  file.path(output, "figures", "summary_stats", "outcome_distribution", paste0("density_", var, "_no_zeros_max05.jpeg")),
  plot = p,
  device = "jpeg",
  path = NULL,
  scale = 1,
  width = NA,
  height = NA
  )




  p <- df |> #Histogram with 0
  ggplot(
    aes(
      x = .data[[var]],
      color = factor(T_group),
      fill = factor(T_group)
    )
  ) +
  geom_histogram(
    binwidth = 0.025,
    position = "stack",
    alpha = 0.5,
    boundary = 0
  ) +
  scale_color_manual(
    name = "Treatment group",
    values = group_colors,
    labels = group_labels
  ) +
  scale_fill_manual(
    values = group_colors,
    guide = "none"
  ) +
  theme_test() +
  xlab("Total Dropout rate") +
  ylab("Count")
  
  print(p)

  ggsave(
  file.path(output, "figures", "summary_stats", "outcome_distribution", paste0("histogram_", var, ".jpeg")),
  plot = p,
  device = "jpeg",
  path = NULL,
  scale = 1,
  width = NA,
  height = NA
  )




  p <- df |> #Histogram with no 0
  dplyr::filter(.data[[var]] != 0) |>
  ggplot(
    aes(
      x = .data[[var]],
      color = factor(T_group),
      fill = factor(T_group)
    )
  ) +
  geom_histogram(
    binwidth = 0.025,
    position = "stack",
    alpha = 0.5,
    boundary = 0
  ) +
  scale_color_manual(
    name = "Treatment group",
    values = group_colors,
    labels = group_labels
  ) +
  scale_fill_manual(
    values = group_colors,
    guide = "none"
  ) +
  theme_test() +
  xlab("Total Dropout rate") +
  ylab("Count")
  
  print(p)

ggsave(
  file.path(output, "figures", "summary_stats", "outcome_distribution", paste0("histogram_", var, "_no_zeros.jpeg")),
  plot = p,
  device = "jpeg",
  path = NULL,
  scale = 1,
  width = NA,
  height = NA
  )




  p <- df |> #Histogram with no 0, max dr = 0.5
  dplyr::filter(.data[[var]] != 0) |>
  ggplot(
    aes(
      x = .data[[var]],
      color = factor(T_group),
      fill = factor(T_group)
    )
  ) +
  geom_histogram(
    binwidth = 0.025,
    position = "stack",
    alpha = 0.5,
    boundary = 0
  ) +
  scale_color_manual(
    name = "Treatment group",
    values = group_colors,
    labels = group_labels
  ) +
  scale_fill_manual(
    values = group_colors,
    guide = "none"
  ) +
  theme_test() +
  coord_cartesian(
    xlim = c(0, 0.5)) +
  xlab("Total Dropout rate") +
  ylab("Count")
  
  print(p)

  ggsave(
  file.path(output, "figures", "summary_stats", "outcome_distribution", paste0("histogram_", var, "_no_zeros_max05.jpeg")),
  plot = p,
  device = "jpeg",
  path = NULL,
  scale = 1,
  width = NA,
  height = NA
  )




  p <- df |> #Histogram with no 0, min dr = 0.5
  dplyr::filter(.data[[var]] != 0) |>
  ggplot(
    aes(
      x = .data[[var]],
      color = factor(T_group),
      fill = factor(T_group)
    )
  ) +
  geom_histogram(
    binwidth = 0.025,
    position = "stack",
    alpha = 0.5,
    boundary = 0
  ) +
  scale_color_manual(
    name = "Treatment group",
    values = group_colors,
    labels = group_labels
  ) +
  scale_fill_manual(
    values = group_colors,
    guide = "none"
  ) +
  theme_test() +
  coord_cartesian(
    xlim = c(0.5, 1),
    ylim = c(0, 150)) +
  xlab("Total Dropout rate") +
  ylab("Count")
  
  print(p)

  ggsave(
  file.path(output, "figures", "summary_stats", "outcome_distribution", paste0("histogram_", var, "_no_zeros_min05.jpeg")),
  plot = p,
  device = "jpeg",
  path = NULL,
  scale = 1,
  width = NA,
  height = NA
  )
}

#----------------------------
# Summary Stats - means and p-value of the difference test
#----------------------------

#--------------------------------------------------
# Summary statistics for 2021
#--------------------------------------------------

# First table: dropout rates by treatment group

table_1 <- df |>
  filter(
    year_school == 2021,
    !is.na(T_group)
  ) |>
  select(T_group, diff, diff_w, diff_m) |>
  pivot_longer(
    cols = c(diff, diff_w, diff_m),
    names_to = "variable",
    values_to = "value"
  ) |>
  group_by(T_group, variable) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(
    T_group,
    match(variable, c("dr", "dr_w", "dr_m"))
  )

table_1

export_latex_table(
  data = table_1,
  file = file.path(output, "tables", "table_1.tex"),
  caption = "Dropout rates by treatment group, 2021",
  label = "dropout-summary-2021",
  digits = 3,
  column_names = c(
    "Treatment group",
    "Outcome",
    "Mean",
    "Minimum",
    "Maximum",
    "Std. dev.",
    "Median",
    "25th percentile",
    "75th percentile"
  ),
  align = c("l", "l", rep("r", 7)),
  scale_down = TRUE,
  notes = paste(
    "Statistics are calculated at the center level."
  )
)

# Second table: comparison between groups 0 and 3

table_ <- vector("list", 6)

for (x in 1:6) {

  table_[[x]] <- df |>
    filter(
      year_school == 2021,
      T_group %in% c(0, x)
    ) |>
    select(
      T_group,
      diff,
      diff_w,
      diff_m,
      initial_registration,
      initial_registration_w,
      initial_registration_m,
      public,
      urban
    ) |>
    pivot_longer(
      cols = -T_group,
      names_to = "variable",
      values_to = "value"
    ) |>
    group_by(variable) |>
    summarise(
      mean_control = mean(
        value[T_group == 0],
        na.rm = TRUE
      ),
      mean_treated = mean(
        value[T_group == x],
        na.rm = TRUE
      ),
      p_value = t.test(
        value[T_group == 0],
        value[T_group == x]
      )$p.value,
      .groups = "drop"
    ) |>
    arrange(
      match(
        variable,
        c(
          "diff",
          "diff_w",
          "diff_m",
          "initial_registration",
          "initial_registration_w",
          "initial_registration_m",
          "public",
          "urban"
        )
      )
    )

  print(paste0("table ", x))
  print(table_[[x]])

  export_latex_table(
  data = table_[[x]],
  file = file.path(output, "tables", paste0("summary_stats_", x, ".tex")),
  caption = paste0("Summary Statistics, 2021: Control vs. Treatment Group ", x),
  label = paste0("summary-stat-2021-", x),
  digits = 3,
  column_names = c(
    "Outcome",
    "Control Mean",
    "Treatment Mean",
    "p-value"
  ),
  align = c("l", "r", "r", "r"),
  scale_down = TRUE,
  notes = paste0(
    "Statistics are calculated at the center level. ",
    "Treatment group ", x, " is compared with the control group."
  )
)

}




#TO DO
# - export summary tables in latex
# - loops for also other tables
# - summary stats by group, for treatment years
# - graphs of only groups of interests
# - Adjust graphs --> praticamente vorrei che aumentasse lo spazio sulle x (i.e. rendere il png un rettangoloo orizzontale e non verticale)
# - trasforma grafici mean dropout rate in loops
#     cosi da fare in una volta sola 3x6 grafici
#Might need to exclude schools with 100% dropout in some year --> i guess it means the school closed. Check if appears the following year.


#Ho fatto loop per grafici, ora va fatto per le tabelle di summary stats
  