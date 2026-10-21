
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# CALLAWAY & SANT'ANNA (2021)
# Static and Dynamic ATT
# Doubly Robust with covariates
# Municipality-clustered standard errors
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

library(dplyr)
library(did)
library(ggplot2)


# --------------------------------
# 0. LOAD DATA
# --------------------------------

df_all <- read.csv(
  file.path(data, "processed", "csv", "center_level.csv")
)

snap <- head(df_all, 1000)


# --------------------------------
# 1. DEFINE COVARIATES
# --------------------------------

covariates <- c(
  "mobiliario",
  "servicios_basicos",
  "amenazas_naturales",
  "amenazas_sociales",
  "hidrosanitaria",
  "infraestructura",
  "indice_precariedad",
  "urban"
)

# IMPORTANT:
# Replace with the actual municipality ID variable.

municipality_var <- "ID.Municipio"


# --------------------------------
# 2. DEFINE OUTCOMES
# --------------------------------

outcomes <- c(
  "Total"  = "dr",
  "Male"   = "dr_m",
  "Female" = "dr_w"
)


# --------------------------------
# 3. DATA CHECKS
# --------------------------------

required_vars <- c(
  "center_code",
  "year_school",
  "T_group",
  municipality_var,
  unname(outcomes),
  covariates
)

missing_vars <- setdiff(
  required_vars,
  names(df_all)
)

if (length(missing_vars) > 0) {
  stop(
    "Missing variables: ",
    paste(missing_vars, collapse = ", ")
  )
}

stopifnot(
  all(vapply(
    df_all[covariates],
    is.numeric,
    logical(1)
  ))
)

stopifnot(
  is.numeric(df_all$year_school)
)

stopifnot(
  2022 %in% df_all$year_school
)

# One observation per center and year
if (anyDuplicated(
  df_all[c("center_code", "year_school")]
) > 0) {
  stop("Duplicate center-year observations detected.")
}

# Municipality identifiers must be available
if (anyNA(df_all[[municipality_var]])) {
  stop("Missing municipality identifiers detected.")
}

# Check that each center belongs to one municipality
municipality_check <- df_all |>
  group_by(center_code) |>
  summarise(
    n_municipalities = n_distinct(
      .data[[municipality_var]]
    ),
    .groups = "drop"
  )

if (any(municipality_check$n_municipalities != 1)) {
  stop("Some centers belong to multiple municipalities.")
}


# --------------------------------
# 4. DEFINE COVARIATE FORMULA
# --------------------------------

# In did::att_gt(), time-invariant covariates
# enter directly in xformla.
#
# No manual interaction with year is needed.

x_formula <- reformulate(covariates)

print(x_formula)


# --------------------------------
# 5. GRAPH SETTINGS
# --------------------------------

plot_colors <- c(
  "Total"  = "#1B9E77",
  "Male"   = "#7570B3",
  "Female" = "#D95F02"
)

plot_position <- position_dodge(
  width = 0.50
)


# --------------------------------
# 6. FUNCTION: EXTRACT EVENT STUDY
# --------------------------------

# Extract dynamic coefficients and
# 95% simultaneous confidence bands
# from the official did::aggte() output.

extract_event_study <- function(model, label) {

  critical_value <- model$crit.val.egt

  if (
    length(critical_value) != 1 ||
    !is.finite(critical_value)
  ) {
    stop("Invalid critical value for confidence bands.")
  }

  tibble(
    event_time = model$egt,
    estimate = model$att.egt,
    std_error = model$se.egt,
    ci_low = estimate - critical_value * std_error,
    ci_high = estimate + critical_value * std_error,
    outcome = label
  ) |>
    filter(
      is.finite(event_time),
      is.finite(estimate),
      is.finite(ci_low),
      is.finite(ci_high)
    )
}


# --------------------------------
# 7. STORE RESULTS
# --------------------------------

results_cs <- list()

event_study_data <- list()

combined_plots <- list()


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# LOOP: TREATMENT GROUPS 2, 3, 4
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

for (x in 2:4) {

  cat("\n")
  cat("====================================\n")
  cat("CALLAWAY & SANT'ANNA\n")
  cat("TREATMENT GROUP:", x, "\n")
  cat("====================================\n")


  # --------------------------------
  # 8. PREPARE DATA
  # --------------------------------

  df_x <- df_all |>
    filter(
      T_group %in% c(0, x)
    ) |>
    mutate(

      # First treatment year:
      # 2023 for treated centers
      # 0 for never-treated centers

      first_treat = if_else(
        T_group == x,
        2023L,
        0L
      )

    )


  # --------------------------------
  # 9. SAMPLE INFORMATION
  # --------------------------------

  cat("\nNumber of centers:\n")
  print(n_distinct(df_x$center_code))

  cat("\nNumber of municipalities:\n")
  print(n_distinct(df_x[[municipality_var]]))

  cat("\nTreatment groups:\n")
  print(table(df_x$T_group))


  # --------------------------------
  # 10. INITIALIZE RESULTS
  # --------------------------------

  group_key <- as.character(x)

  results_cs[[group_key]] <- list()

  group_event_data <- list()


  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # LOOP OVER OUTCOMES
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  for (outcome_label in names(outcomes)) {

    outcome_var <- outcomes[[outcome_label]]

    cat("\n")
    cat("------------------------------------\n")
    cat("Outcome:", outcome_label, "\n")
    cat("------------------------------------\n")


    # --------------------------------
    # 11. CALLAWAY & SANT'ANNA
    # --------------------------------

    set.seed(
      1234 + 100 * x +
        match(outcome_label, names(outcomes))
    )

    cs_att <- did::att_gt(

      # Outcome
      yname = outcome_var,

      # Time variable
      tname = "year_school",

      # Panel identifier
      idname = "center_code",

      # First treatment year
      gname = "first_treat",

      # Covariates
      xformla = x_formula,

      # Dataset
      data = df_x,

      # Panel structure
      panel = TRUE,

      # Use balanced panel
      allow_unbalanced_panel = FALSE,

      # Comparison group
      control_group = "nevertreated",

      # Doubly Robust estimator
      est_method = "dr",

      # Event-study reference:
      # year immediately before treatment
      base_period = "universal",

      # No anticipation
      anticipation = 0,

      # Center ID + municipality clustering
      # Center-level clustering is implicit;
      # municipality is the additional level.
      clustervars = c(
        "center_code",
        municipality_var
      ),

      # Clustered multiplier bootstrap
      bstrap = TRUE,

      # Simultaneous confidence bands
      cband = TRUE,

      # Bootstrap replications
      biters = 1000,

      # Significance level
      alp = 0.05

    )


    # --------------------------------
    # 12. STATIC / OVERALL ATT
    # --------------------------------

    cs_overall <- did::aggte(
      cs_att,
      type = "simple"
    )


    # --------------------------------
    # 13. DYNAMIC ATT / EVENT STUDY
    # --------------------------------

    cs_dynamic <- did::aggte(
      cs_att,
      type = "dynamic"
    )


    # --------------------------------
    # 14. PRINT RESULTS
    # --------------------------------

    cat("\nOVERALL ATT\n")

    print(
      summary(cs_overall)
    )

    cat("\nDYNAMIC ATT\n")

    print(
      summary(cs_dynamic)
    )


    # --------------------------------
    # 15. STORE ESTIMATION OBJECTS
    # --------------------------------

    results_cs[[group_key]][[outcome_label]] <- list(
      att_gt = cs_att,
      overall = cs_overall,
      dynamic = cs_dynamic
    )


    # --------------------------------
    # 16. INDIVIDUAL EVENT STUDY
    # --------------------------------

    # Official plotting function
    # from the did package.

    p_individual <- did::ggdid(

      cs_dynamic,

      xlab = "Years relative to treatment",

      ylab = "Effect on dropout rate",

      title = paste0(
        "Callaway-Sant'Anna: Group ",
        x,
        " - ",
        outcome_label
      ),

      ref_line = 0,

      legend = FALSE

    ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(
          face = "bold"
        )
      )

    print(p_individual)


    # --------------------------------
    # 17. EXTRACT COEFFICIENTS
    # --------------------------------

    group_event_data[[outcome_label]] <-
      extract_event_study(
        cs_dynamic,
        outcome_label
      )

  }


  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # 18. COMBINED EVENT-STUDY GRAPH
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  plot_df <- bind_rows(
    group_event_data
  ) |>
    mutate(
      outcome = factor(
        outcome,
        levels = c(
          "Total",
          "Male",
          "Female"
        )
      )
    )


  # --------------------------------
  # 19. CREATE COMBINED PLOT
  # --------------------------------

  p_combined <- ggplot(
    plot_df,
    aes(
      x = event_time,
      y = estimate,
      color = outcome,
      group = outcome
    )
  ) +

    # Zero treatment effect
    geom_hline(
      yintercept = 0,
      color = "grey50",
      linetype = "dashed",
      linewidth = 0.5
    ) +

    # Reference year: 2022
    geom_vline(
      xintercept = -1,
      color = "grey40",
      linetype = "dotted",
      linewidth = 0.6
    ) +

    # Confidence intervals
    geom_errorbar(
      aes(
        ymin = ci_low,
        ymax = ci_high
      ),
      width = 0.12,
      linewidth = 0.65,
      position = plot_position
    ) +

    # Coefficient estimates
    geom_point(
      size = 2.5,
      position = plot_position
    ) +

    # Colors
    scale_color_manual(
      values = plot_colors
    ) +

    # Event time axis
    scale_x_continuous(
      breaks = sort(
        unique(plot_df$event_time)
      )
    ) +

    # Labels
    labs(
      title = paste0(
        "Callaway-Sant'Anna: Treatment Group ",
        x
      ),
      subtitle = paste0(
        "Doubly Robust estimates; ",
        "95% simultaneous confidence bands"
      ),
      x = "Years relative to treatment",
      y = "Effect on dropout rate",
      color = NULL
    ) +

    # Theme
    theme_minimal(base_size = 12) +

    theme(
      plot.title = element_text(
        face = "bold",
        size = 14
      ),
      plot.subtitle = element_text(
        size = 10,
        color = "grey40"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      axis.text = element_text(
        color = "grey40"
      )
    )


  # --------------------------------
  # 20. DISPLAY COMBINED PLOT
  # --------------------------------

  print(p_combined)


  # --------------------------------
  # 21. STORE PLOT AND COEFFICIENTS
  # --------------------------------

  combined_plots[[group_key]] <- p_combined

  event_study_data[[group_key]] <- plot_df

}
