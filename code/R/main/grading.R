# TWFE by grade
df <- read.csv(file.path(data, "processed", "csv", "grade_level.csv"))

outcomes <- c("dr", "dr_w", "dr_m")

grade_order <- c(
  "primer grado - prebasica",
  "segundo grado - prebasica",
  "tercer grado - prebasica",
  "primer grado",
  "segundo grado",
  "tercer grado",
  "cuarto grado",
  "quinto grado",
  "sexto grado",
  "septimo grado",
  "octavo grado",
  "noveno grado",
  "decimo grado",
  "undecimo grado",
  "duodecimo grado"
)

# Identify grades present in the data
grades <- df |>
  distinct(grade) |>
  filter(!is.na(grade)) |>
  mutate(
    grade = factor(grade, levels = grade_order)
  ) |>
  arrange(grade)

models <- list()
results <- list()

for (i in seq_len(nrow(grades))) {

  grade_i <- as.character(grades$grade[i])

  df_x <- df |>
    filter(
      grade == .env$grade_i,
      T_group %in% c(0, 3)
    ) |>
    mutate(
      treated = as.integer(T_group == 3 & year_school >= 2023)
    )

  models[[grade_i]] <- list()

  for (outcome in outcomes) {

    result_key <- paste(grade_i, outcome, sep = " / ")

    fit_result <- tryCatch({

      twfe <- feols(
        as.formula(
          paste(outcome, "~ treated | center_code + year_school")
        ),
        data = df_x,
        cluster = ~center_code
      )

      if (!"treated" %in% names(coef(twfe))) {
        stop("Treatment coefficient could not be estimated.")
      }

      ci <- confint(twfe, parm = "treated", level = 0.95)

      list(
        model = twfe,
        estimate = unname(coef(twfe)["treated"]),
        conf_low = ci[1, 1],
        conf_high = ci[1, 2],
        estimation_failed = FALSE,
        failure_reason = NA_character_
      )

    }, error = function(e) {

      warning(
        paste(result_key, ":", conditionMessage(e)),
        call. = FALSE
      )

      list(
        model = NULL,
        estimate = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        estimation_failed = TRUE,
        failure_reason = conditionMessage(e)
      )
    })

    models[[grade_i]][outcome] <- list(fit_result$model)

    results[[result_key]] <- data.frame(
      grade = grade_i,
      outcome = outcome,
      estimate = fit_result$estimate,
      conf_low = fit_result$conf_low,
      conf_high = fit_result$conf_high,
      estimation_failed = fit_result$estimation_failed,
      failure_reason = fit_result$failure_reason
    )
  }
}

# Combine results
coef_by_grade <- bind_rows(results) |>
  mutate(
    grade = factor(grade, levels = grade_order)
  )

# Plot
coef_plot <- ggplot(
  coef_by_grade |> filter(!estimation_failed),
  aes(
    x = grade,
    y = estimate,
    colour = outcome,
    group = outcome
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey50"
  ) +
  geom_pointrange(
    aes(ymin = conf_low, ymax = conf_high),
    position = position_dodge(width = 0.45)
  ) +
  scale_x_discrete(
    limits = grade_order,
    labels = setNames(
      as.character(1:15),
      grade_order
    ),
    drop = FALSE
  ) +
  scale_colour_manual(
    values = c(
      dr = "#0072B2",
      dr_w = "#D55E00",
      dr_m = "#009E73"
    ),
    breaks = outcomes,
    labels = c("Total", "Female", "Male")
  ) +
  labs(
    x = "Grade",
    y = "TWFE treatment coefficient",
    colour = "Outcome",
    caption = "Grade 10 corresponds to approximately age 12"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1)
  )

print(coef_plot)

# Inspect failed estimations
failed_estimations <- coef_by_grade |>
  filter(estimation_failed)

print(failed_estimations)
