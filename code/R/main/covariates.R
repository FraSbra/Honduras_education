
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# TWFE (static and dynamic)
# With baseline covariates × year FE
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



# --------------------------------
# 0. Load data
# --------------------------------

df_all <- read.csv(
  file.path(data, "processed", "csv", "center_level.csv")
)

snap <- head(df_all, 1000)


# --------------------------------
# Define covariates
# --------------------------------

# REPLACE WITH ACTUAL VARIABLE NAMES

covariates <- c(
  "mobiliario",
  "servicios_basicos",
  "amenazas_naturales",
  "amenazas_sociales",
  "hidrosanitaria",
  "infraestructura",
  "indice_precariedad",
  "public",
  "urban"
)

# Check variables
stopifnot(all(covariates %in% names(df_all)))

# This specification assumes numeric covariates
stopifnot(
  all(vapply(df_all[covariates], is.numeric, logical(1)))
)

stopifnot(2022 %in% df_all$year_school)


# --------------------------------
# Construct covariate interactions
# --------------------------------

# Each time-invariant covariate is interacted
# with year dummies.
#
# Example:
# i(year_school, cov1, ref = 2022)
#
# Reference year: 2022

covariate_terms <- paste0(
  "i(year_school, ",
  covariates,
  ", ref = 2022)"
)


# --------------------------------
# Function to construct formulas
# --------------------------------

make_formula <- function(outcome, dynamic = FALSE) {

  if (dynamic) {

    treatment_term <-
      "i(event_time, treated_group, ref = -1)"

  } else {

    treatment_term <- "treated"

  }

  rhs <- paste(
    c(treatment_term, covariate_terms),
    collapse = " + "
  )

  as.formula(
    paste0(
      outcome, " ~ ",
      rhs,
      " | center_code + year_school"
    )
  )
}


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# LOOP: Treatment Groups 2, 3, 4
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

for (x in 2:4) {

  # --------------------------------
  # Prepare data
  # --------------------------------

  df_x <- df_all |>
    filter(
      T_group == 0 | T_group == .env$x
    ) |>
    mutate(
      treated = ifelse(
        T_group == .env$x & year_school >= 2023,
        1, 0
      ),
      treated_group = if_else(
        T_group == .env$x,
        1L, 0L
      ),
      event_time = year_school - 2023
    )


  # --------------------------------
  # 1. TWFE WITH COVARIATES
  # --------------------------------

  twfe <- feols(
    make_formula("dr"),
    data = df_x,
    cluster = ~center_code
  )

  twfe_w <- feols(
    make_formula("dr_w"),
    data = df_x,
    cluster = ~center_code
  )

  twfe_m <- feols(
    make_formula("dr_m"),
    data = df_x,
    cluster = ~center_code
  )


  # --------------------------------
  # 2. DYNAMIC TWFE WITH COVARIATES
  # --------------------------------

  dynamic_twfe <- feols(
    make_formula("dr", dynamic = TRUE),
    data = df_x,
    cluster = ~center_code
  )

  dynamic_twfe_w <- feols(
    make_formula("dr_w", dynamic = TRUE),
    data = df_x,
    cluster = ~center_code
  )

  dynamic_twfe_m <- feols(
    make_formula("dr_m", dynamic = TRUE),
    data = df_x,
    cluster = ~center_code
  )


  # --------------------------------
  # 3. Print regression results
  # --------------------------------

  cat("\n")
  cat("====================================\n")
  cat("TREATMENT GROUP:", x, "\n")
  cat("====================================\n")

  cat("\nSTATIC TWFE WITH COVARIATES\n")

  print(
    etable(
      twfe,
      twfe_m,
      twfe_w
    )
  )

  cat("\nDYNAMIC TWFE WITH COVARIATES\n")

  print(
    etable(
      dynamic_twfe,
      dynamic_twfe_m,
      dynamic_twfe_w
    )
  )


  # --------------------------------
  # 4. Individual event-study graphs
  # --------------------------------

  # i.select = 1 selects the treatment
  # event-study interaction, excluding
  # the covariate × year interactions.

  iplot(
    dynamic_twfe,
    i.select = 1,
    xlab = "Years relative to treatment",
    ylab = "Effect on dropout rate",
    main = paste0(
      "Dynamic TWFE: Treatment Group ",
      x,
      " (Adjusted)"
    ),
    ref.line = -1
  )

  iplot(
    dynamic_twfe_w,
    i.select = 1,
    xlab = "Years relative to treatment",
    ylab = "Effect on dropout rate",
    main = paste0(
      "Dynamic TWFE (W): Treatment Group ",
      x,
      " (Adjusted)"
    ),
    ref.line = -1
  )

  iplot(
    dynamic_twfe_m,
    i.select = 1,
    xlab = "Years relative to treatment",
    ylab = "Effect on dropout rate",
    main = paste0(
      "Dynamic TWFE (M): Treatment Group ",
      x,
      " (Adjusted)"
    ),
    ref.line = -1
  )


  # --------------------------------
  # 5. Merged event-study graph
  # --------------------------------

  plot_colors <- c(
    "#1B9E77",
    "#7570B3",
    "#D95F02"
  )

  plot_symbols <- c(16, 16, 16)

  old_par <- par(
    mar = c(7, 5, 4, 1),
    bty = "n",
    las = 1,
    tcl = 0,
    col.axis = "grey40",
    cex.axis = 0.9
  )

  iplot(
    objects = list(
      dynamic_twfe,
      dynamic_twfe_m,
      dynamic_twfe_w
    ),
    i.select = 1,
    col = plot_colors,
    pch = plot_symbols,
    pt.cex = 0.9,
    sep = 0.07,
    ci_level = 0.95,
    ci.width = "0.4%",
    ci.lwd = 1.2,
    grid = TRUE,
    grid.par = list(
      col = "grey92",
      lty = 1,
      lwd = 1
    ),
    zero.par = list(
      col = "grey50",
      lty = 2,
      lwd = 1
    ),
    ref.line = -1,
    ref.line.par = list(
      col = "grey40",
      lty = 3,
      lwd = 1
    ),
    xlab = "Years relative to treatment",
    ylab = "Effect on dropout rate",
    main = ""
  )


  # --------------------------------
  # Title and subtitle
  # --------------------------------

  mtext(
    paste0(
      "Dynamic TWFE: Treatment Group ",
      x,
      " (Adjusted)"
    ),
    side = 3,
    line = 2,
    adj = 0,
    font = 2,
    cex = 1.1
  )

  mtext(
    "Event-study coefficients; 95% confidence intervals",
    side = 3,
    line = 0.6,
    adj = 0,
    cex = 0.9
  )


  # --------------------------------
  # Legend
  # --------------------------------

  legend(
    "bottom",
    inset = c(0, -0.55),
    legend = c(
      "Total",
      "Male",
      "Female"
    ),
    col = plot_colors,
    pch = plot_symbols,
    horiz = TRUE,
    bty = "n",
    xpd = NA,
    cex = 0.9
  )

  par(old_par)

}
