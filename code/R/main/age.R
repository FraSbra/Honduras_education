#TWFE by age


#TWFE by age
df <- read.csv(file.path(data, "processed", "csv", "age_level.csv"))


#Histogram of total enrollment per age (per vedere i problemi oltre i 25 anni)
df %>%
  filter(age_students > 75) %>%
  group_by(age_students) %>%
  summarise(
    total_registration = sum(initial_registration, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = age_students, y = total_registration)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Total Initial Registration by Age",
    x = "Age",
    y = "Number of Enrolled Students"
  ) +
  theme_minimal()




ages <- 3:20
outcomes <- c("dr", "dr_w", "dr_m")

models <- list()
results <- list()

for (age in ages) {

  df_x <- df |>
    filter(
      age_students == .env$age,
      T_group %in% c(0, 3)
    ) |>
    mutate(
      treated = as.integer(T_group == 3 & year_school >= 2023)
    )

  models[[as.character(age)]] <- list()

  for (outcome in outcomes) {

    # Same specification, changing only the outcome
    twfe <- feols(
      as.formula(paste(outcome, "~ treated | center_code + year_school")),
      data = df_x,
      cluster = ~center_code
    )

    # Store the regression
    models[[as.character(age)]][[outcome]] <- twfe

    # Store its treatment coefficient and 95% confidence interval
    ci <- confint(twfe, parm = "treated", level = 0.95)

    results[[paste(age, outcome, sep = "_")]] <- data.frame(
      age_students = age,
      outcome = outcome,
      estimate = unname(coef(twfe)["treated"]),
      conf_low = ci[1, 1],
      conf_high = ci[1, 2]
    )
  }
}

coef_by_age <- bind_rows(results)

coef_plot <- ggplot(
  coef_by_age,
  aes(
    x = age_students,
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
  scale_x_continuous(breaks = ages, minor_breaks = NULL) +
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
    x = "Age of students",
    y = "TWFE treatment coefficient",
    colour = "Outcome"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(coef_plot)
