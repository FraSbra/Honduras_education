
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# CALLAWAY & SANT'ANNA (2021)
# STAGGERED DIFFERENCE-IN-DIFFERENCES
#
# Groups 2, 3, 4: treated in 2023
# Groups 5, 6:    treated in 2024
# Group 0:        never treated
# Group 1:        excluded
#
# Doubly Robust
# Municipality-clustered inference
# Static and Dynamic ATT
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

library(dplyr)
library(did)
library(ggplot2)


# ====================================
# 0. SETTINGS
# ====================================

municipality_var <- "ID.Municipio"

# Control group remains T_group == 0
control_type <- "nevertreated"

# Bootstrap settings
n_bootstrap <- 1000

# Run pooled estimation
run_pooled <- TRUE

# Run separate estimations for groups 2-6
run_group_specific <- TRUE

# Show individual event-study plots
show_individual_plots <- TRUE


# ====================================
# 1. LOAD DATA
# ====================================

df_all <- read.csv(
  file.path(
    data,
    "processed",
    "csv",
    "center_level.csv"
  )
)


# ====================================
# 2. DEFINE COVARIATES
# ====================================

# Exclude public because of the
# collinearity previously identified.

covariates <- c(
  "indice_precariedad",
  "urban"
)

x_formula <- reformulate(covariates)


# ====================================
# 3. DEFINE OUTCOMES
# ====================================

outcomes <- c(
  "Total"  = "dr",
  "Male"   = "dr_m",
  "Female" = "dr_w"
)


# ====================================
# 4. DATA PREPARATION
# ====================================

df_cs <- df_all |>
  filter(
    T_group %in% c(0, 2, 3, 4, 5, 6)
  ) |>
  mutate(

    # First treatment year

    first_treat = case_when(
      T_group == 0 ~ 0L,
      T_group %in% c(2, 3, 4) ~ 2023L,
      T_group %in% c(5, 6) ~ 2024L,
      TRUE ~ NA_integer_
    )

  )


# ====================================
# 5. DATA CHECKS
# ====================================

required_vars <- c(
  "center_code",
  "year_school",
  "T_group",
  "first_treat",
  municipality_var,
  unname(outcomes),
  covariates
)

missing_vars <- setdiff(
  required_vars,
  names(df_cs)
)

if (length(missing_vars) > 0) {
  stop(
    "Missing variables: ",
    paste(missing_vars, collapse = ", ")
  )
}

stopifnot(
  all(
    vapply(
      df_cs[covariates],
      is.numeric,
      logical(1)
    )
  )
)

stopifnot(
  is.numeric(df_cs$year_school)
)

stopifnot(
  all(c(2022, 2023, 2024) %in%
        df_cs$year_school)
)

# Check unique center-year observations

if (
  anyDuplicated(
    df_cs[c("center_code", "year_school")]
  ) > 0
) {
  stop("Duplicate center-year observations.")
}

# Check municipality identifiers

if (
  anyNA(df_cs[[municipality_var]])
) {
  stop("Missing municipality identifiers.")
}

# Each center must belong to one municipality

municipality_check <- df_cs |>
  group_by(center_code) |>
  summarise(
    n_municipalities = n_distinct(
      .data[[municipality_var]]
    ),
    .groups = "drop"
  )

if (
  any(municipality_check$n_municipalities != 1)
) {
  stop("Some centers belong to multiple municipalities.")
}


# ====================================
# 6. DESCRIPTIVE INFORMATION
# ====================================

cat("\n============================\n")
cat("STAGGERED DID DATA\n")
cat("============================\n")

cat("\nTreatment group mapping:\n")

print(
  df_cs |>
    distinct(T_group, first_treat) |>
    arrange(T_group)
)

cat("\nCenters by treatment group:\n")

print(
  df_cs |>
    group_by(T_group, first_treat) |>
    summarise(
      centers = n_distinct(center_code),
      municipalities = n_distinct(
        .data[[municipality_var]]
      ),
      .groups = "drop"
    )
)


# ====================================
# 7. GRAPH SETTINGS
# ====================================

plot_colors <- c(
  "Total"  = "#1B9E77",
  "Male"   = "#7570B3",
  "Female" = "#D95F02"
)

plot_position <- position_dodge(
  width = 0.50
)


# ====================================
# 8. EXTRACT EVENT-STUDY COEFFICIENTS
# ====================================

extract_event_study <- function(model, label) {

  critical_value <- model$crit.val.egt

  if (
    length(critical_value) != 1 ||
    !is.finite(critical_value)
  ) {
    stop(
      "Invalid confidence-band critical value."
    )
  }

  tibble(
    event_time = model$egt,
    estimate = model$att.egt,
    std_error = model$se.egt,
    ci_low = estimate -
      critical_value * std_error,
    ci_high = estimate +
      critical_value * std_error,
    outcome = label
  ) |>
    filter(
      is.finite(event_time),
      is.finite(estimate),
      is.finite(ci_low),
      is.finite(ci_high)
    )
}


# ====================================
# 9. COMBINED COEFFICIENT PLOT
# ====================================

make_combined_plot <- function(
    model_results,
    plot_title
) {

  plot_df <- bind_rows(
    lapply(
      names(outcomes),
      function(label) {

        extract_event_study(
          model_results[[label]]$dynamic,
          label
        )

      }
    )
  ) |>
    mutate(
      outcome = factor(
        outcome,
        levels = names(outcomes)
      )
    )

  p <- ggplot(
    plot_df,
    aes(
      x = event_time,
      y = estimate,
      color = outcome,
      group = outcome
    )
  ) +

    geom_hline(
      yintercept = 0,
      color = "grey50",
      linetype = "dashed"
    ) +

    geom_vline(
      xintercept = -1,
      color = "grey50",
      linetype = "dotted"
    ) +

    geom_errorbar(
      aes(
        ymin = ci_low,
        ymax = ci_high
      ),
      width = 0.12,
      linewidth = 0.65,
      position = plot_position
    ) +

    geom_point(
      size = 2.5,
      position = plot_position
    ) +

    scale_color_manual(
      values = plot_colors
    ) +

    scale_x_continuous(
      breaks = seq(
        floor(min(c(-1, plot_df$event_time))),
        ceiling(max(c(-1, plot_df$event_time)))
      )
    ) +

    labs(
      title = plot_title,
      subtitle = paste0(
        "Doubly Robust ATT; ",
        "95% simultaneous confidence bands"
      ),
      x = "Years relative to treatment",
      y = "Effect on dropout rate",
      color = NULL
    ) +

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
      legend.direction = "horizontal"
    )

  return(
    list(
      plot = p,
      data = plot_df
    )
  )
}


# ====================================
# 10. ESTIMATION FUNCTION
# ====================================

estimate_cs <- function(
    df_input,
    outcome_var,
    seed
) {

  # Remove non-finite observations
  # for the outcome and covariates

  model_vars <- c(
    outcome_var,
    covariates
  )

  df_model <- df_input |>
    filter(
      if_all(
        all_of(model_vars),
        ~ is.finite(.x)
      )
    )

  cat("\nRows before missing-value removal:",
      nrow(df_input), "\n")

  cat("Rows after missing-value removal:",
      nrow(df_model), "\n")

  set.seed(seed)


  # --------------------------------
  # ATT(g,t): Callaway-Sant'Anna
  # --------------------------------

  att <- did::att_gt(

    yname = outcome_var,

    tname = "year_school",

    idname = "center_code",

    gname = "first_treat",

    xformla = x_formula,

    data = df_model,

    panel = TRUE,

    allow_unbalanced_panel = FALSE,

    control_group = control_type,

    est_method = "dr",

    base_period = "universal",

    anticipation = 0,

    # Municipality-level clustering
    # Center-level clustering is implicit
    clustervars = c(
      "center_code",
      municipality_var
    ),

    bstrap = TRUE,

    cband = TRUE,

    biters = n_bootstrap,

    alp = 0.05

  )


  # --------------------------------
  # Check ATT(g,t) estimates
  # --------------------------------

  bad_att <- which(
    !is.finite(att$att)
  )

  if (length(bad_att) > 0) {

    print(
      data.frame(
        cohort = att$group[bad_att],
        year = att$t[bad_att],
        ATT = att$att[bad_att]
      )
    )

    stop(
      "Some ATT(g,t) estimates are missing. ",
      "Check collinearity and overlap before aggregation."
    )
  }


  # --------------------------------
  # Overall ATT
  # --------------------------------

  overall <- did::aggte(
    att,
    type = "simple"
  )


  # --------------------------------
  # Dynamic ATT
  # --------------------------------

  dynamic <- did::aggte(
    att,
    type = "dynamic"
  )


  # --------------------------------
  # Group / cohort ATT
  # --------------------------------

  cohort <- did::aggte(
    att,
    type = "group"
  )


  return(
    list(
      att = att,
      overall = overall,
      dynamic = dynamic,
      cohort = cohort
    )
  )

}


# ====================================
# 11. FUNCTION: RUN ALL OUTCOMES
# ====================================

run_cs_analysis <- function(
    df_input,
    analysis_name,
    seed_offset,
    show_cohort_plot = FALSE
) {

  models <- list()

  for (label in names(outcomes)) {

    outcome_var <- outcomes[[label]]

    cat("\n")
    cat("====================================\n")
    cat(analysis_name, "\n")
    cat("Outcome:", label, "\n")
    cat("====================================\n")


    # --------------------------------
    # Estimate
    # --------------------------------

    fit <- estimate_cs(
      df_input = df_input,
      outcome_var = outcome_var,
      seed = 20261019 +
        seed_offset +
        match(label, names(outcomes))
    )

    models[[label]] <- fit


    # --------------------------------
    # Print overall ATT
    # --------------------------------

    cat("\nOVERALL ATT\n")

    print(
      summary(fit$overall)
    )


    # --------------------------------
    # Print dynamic ATT
    # --------------------------------

    cat("\nDYNAMIC ATT\n")

    print(
      summary(fit$dynamic)
    )


    # --------------------------------
    # Individual event-study plot
    # --------------------------------

    if (show_individual_plots) {

      p_individual <- did::ggdid(
        fit$dynamic,
        title = paste0(
          analysis_name,
          " - ",
          label
        ),
        xlab = "Years relative to treatment",
        ylab = "Effect on dropout rate",
        ref_line = 0,
        legend = FALSE
      ) +
        theme_minimal(base_size = 12)

      print(p_individual)

    }


    # --------------------------------
    # Cohort ATT plot
    # (only for pooled analysis)
    # --------------------------------

    if (show_cohort_plot) {

      cat("\nATT BY TREATMENT COHORT\n")

      print(
        summary(fit$cohort)
      )

      p_cohort <- did::ggdid(
        fit$cohort,
        title = paste0(
          "ATT by Treatment Cohort - ",
          label
        ),
        xlab = "First treatment year",
        ylab = "Average treatment effect",
        ref_line = 0
      ) +
        theme_minimal(base_size = 12)

      print(p_cohort)

    }

  }


  # --------------------------------
  # Combined event-study plot
  # --------------------------------

  combined <- make_combined_plot(
    model_results = models,
    plot_title = analysis_name
  )

  print(combined$plot)


  return(
    list(
      models = models,
      combined_plot = combined$plot,
      event_data = combined$data
    )
  )

}


# ====================================
# 12. POOLED STAGGERED DID
# ====================================

# All treated groups 2-6 together
# with 2023 and 2024 treatment cohorts.

results_pooled <- NULL

if (run_pooled) {

  cat("\n")
  cat("####################################\n")
  cat("POOLED STAGGERED DID: GROUPS 2-6\n")
  cat("####################################\n")

  results_pooled <- run_cs_analysis(

    df_input = df_cs,

    analysis_name =
      "Callaway-Sant'Anna: Pooled Groups 2-6",

    seed_offset = 1000,

    show_cohort_plot = TRUE

  )

}


# ====================================
# 13. GROUP-SPECIFIC DID
# ====================================

# Each group compared with T_group = 0.
#
# Groups 2,3,4: treatment in 2023
# Groups 5,6: treatment in 2024

results_by_group <- list()

if (run_group_specific) {

  for (x in 2:6) {

    # --------------------------------
    # Select treated group and controls
    # --------------------------------

    df_x <- df_cs |>
      filter(
        T_group %in% c(0, x)
      )

    cat("\n")
    cat("####################################\n")
    cat("TREATMENT GROUP:", x, "\n")
    cat("####################################\n")

    cat("\nTreatment timing:\n")

    print(
      df_x |>
        distinct(T_group, first_treat) |>
        arrange(T_group)
    )


    # --------------------------------
    # Estimate all outcomes
    # --------------------------------

    results_by_group[[as.character(x)]] <-
      run_cs_analysis(

        df_input = df_x,

        analysis_name = paste0(
          "Callaway-Sant'Anna: Treatment Group ",
          x
        ),

        seed_offset = 100 * x,

        show_cohort_plot = FALSE

      )

  }

}


# ====================================
# 14. RESULTS AVAILABLE IN MEMORY
# ====================================

cat("\n")
cat("====================================\n")
cat("ALL ESTIMATIONS COMPLETED\n")
cat("====================================\n")
