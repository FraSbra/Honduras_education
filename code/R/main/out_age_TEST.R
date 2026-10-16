#Load data
df <- read.csv(file.path(data, "processed", "csv", "enrollment_treatment_merged.csv"))

#----------------------------
# Remove centers flagged as problematic. Will address this in future, or as robustness
#----------------------------

#Variable flags
df_no_flagged_centers <- df %>%
  filter_out(
	(Departamento_problem == 1) | (Municipio_problem == 1) | (admin_type_problem == 1) | (area_type_problem == 1)
  )

#Balancing
df_balanced <- df_no_flagged_centers %>%
  filter_out(
	flagged_center == 1
  )



#Assign theoretical age based on school grade
df_balanced <- df_balanced %>%
  mutate(
    theory_age = match(grade, grade_order) + 2 #Under the assumption that primer grado equals age 6 (if you look at the data)
  )

#Check age difference
df_balanced <- df_balanced %>%
  mutate(
    age_gap = age_students - theory_age
  )


#----------------------------
# Collapse to grade x age level
#----------------------------

first_vars <- c(
	"ID.Municipio",
	"Municipio",
	"ID.Departamento",
	"Departamento",
	"first_treatment",
	"t_day",
	"t_month",
	"t_year",
	"treat_1",
	"treat_2",
	"treat_3",
	"treat_4",
	"treat_5",
	"treat_6",
	"admin_type",
	"area_type",
  "theory_age",
  "age_gap"
)

sum_vars <- c(
	"initial_registration_w",
	"initial_registration_m",
	"initial_registration",
	"dropout_w",
	"dropout_m",
	"dropout",
	"cancellations_w",
	"cancellations_m",
	"cancellations",
	"final_registration_w",
	"final_registration_m",
	"final_registration",
	"repetition_w",
	"repetition_m",
	"repetition"
)

#Main Dataset
df_gap_level <- collap(
	df_balanced,
	~ year_school + center_code + grade + age_students,
	custom = list(
      ffirst = first_vars, #take first value
	    fsum = sum_vars     #take sum
	  )
  )

#----------------------------
# Create Variables for Analysis
#----------------------------
#Create dropout rate at school x year level
#and create treatment (T) status, 1 = treated
#and create groups (T_group), 0 = control and then ordered as the actual treatment

datasets <- list(
  df_gap_level = df_gap_level
)

datasets <- lapply(datasets, function(x) {
  x %>%
    mutate(
      diff = if_else( #Total dropout rate
        initial_registration == 0,
        NA_real_,
        (initial_registration - final_registration) / initial_registration
    ),
    diff_w = if_else( 
      initial_registration_w == 0,
      NA_real_,
      (initial_registration_w - final_registration_w) / initial_registration_w
    ),
    diff_m = if_else(
      initial_registration_m == 0,
      NA_real_,
      (initial_registration_m - final_registration_m) / initial_registration_m
    ),
		dr = if_else( #Total dropout rate
      initial_registration == 0,
      NA_real_,
      (dropout) / initial_registration
    ),
    dr_w = if_else( 
      initial_registration_w == 0,
      NA_real_,
      (dropout_w) / initial_registration_w
    ),
    dr_m = if_else(
      initial_registration_m == 0,
      NA_real_,
      (dropout_m) / initial_registration_m
    ),
		canc = if_else( #Total dropout rate
      initial_registration == 0,
      NA_real_,
      (cancellations) / initial_registration
    ),
    canc_w = if_else( 
      initial_registration_w == 0,
      NA_real_,
      (cancellations_w) / initial_registration_w
    ),
    canc_m = if_else(
      initial_registration_m == 0,
      NA_real_,
      (cancellations_m) / initial_registration_m
    ),
    reg = if_else(
      initial_registration > 0,
      log(initial_registration),
      NA_real_
      ),
    reg_m = if_else(
      initial_registration_m > 0,
      log(initial_registration_m),
      NA_real_
      ),
    reg_w = if_else(
      initial_registration_w > 0,
      log(initial_registration_w),
      NA_real_
      ),
    T = ifelse( #gen treatment Y/N var
      treat_1 == 1 | treat_2 == 1 | treat_3 == 1 |
        treat_4 == 1 | treat_5 == 1 | treat_6 == 1,
      1L,
      0L
    ),
    T_group = if_else(treat_2 == 1, 2L, T), #Gen unique Treatment grouping variable
    T_group = if_else(treat_3 == 1, 3L, T_group),
    T_group = if_else(treat_4 == 1, 4L, T_group),
    T_group = if_else(treat_5 == 1, 5L, T_group),
    T_group = if_else(treat_6 == 1, 6L, T_group),

    public = if_else(admin_type == "gubernamental", 1L, 0L), #Numeric public vs private
    urban = if_else(area_type == "urbana", 1L, 0L) #Numeric rural vs urban
  )
})

df_gap_level <- datasets$df_gap_level


#keep only bigger kids (wrt their theoretical year of school
df_grown_only <- df_gap_level %>%
  filter(age_gap > 0)


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
grades <- df_grown_only |>
  distinct(grade) |>
  filter(!is.na(grade), grade %in% grade_order) |>
  mutate(
    grade = factor(grade, levels = grade_order)
  ) |>
  arrange(grade)

models <- list()
results <- list()

for (i in seq_len(nrow(grades))) {

  grade_i <- as.character(grades$grade[i])

  df_x <- df_grown_only |>
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

















#============================================================
# TRIPLE DIFFERENCE: TREATMENT × POST × OVER-AGE
# Center and Year Fixed Effects
#============================================================

#----------------------------
# 1. Create over-age indicator
#----------------------------

df_binary_age <- df_gap_level %>%
  mutate(
    old = if_else(age_gap > 0, 1L, 0L)
  )

#----------------------------
# 2. Keep treatment and control groups
#----------------------------

df_ddd <- df_binary_age %>%
  filter(
    T_group %in% c(0, 3),
    !is.na(old)
  )

# Check treatment-group consistency within schools
treatment_check <- df_ddd %>%
  group_by(center_code) %>%
  summarise(
    n_groups = n_distinct(T_group),
    .groups = "drop"
  ) %>%
  filter(n_groups > 1)

if (nrow(treatment_check) > 0) {
  stop("Some schools have multiple treatment groups.")
}

#----------------------------
# 3. Collapse to center × year × old level
#----------------------------

df_ddd <- df_ddd %>%
  group_by(year_school, center_code, old) %>%
  summarise(
    T_group = first(T_group),

    initial_registration = sum(initial_registration, na.rm = TRUE),
    initial_registration_w = sum(initial_registration_w, na.rm = TRUE),
    initial_registration_m = sum(initial_registration_m, na.rm = TRUE),

    dropout = sum(dropout, na.rm = TRUE),
    dropout_w = sum(dropout_w, na.rm = TRUE),
    dropout_m = sum(dropout_m, na.rm = TRUE),

    .groups = "drop"
  )

#----------------------------
# 4. Recalculate dropout rates
#----------------------------

df_ddd <- df_ddd %>%
  mutate(
    dr = if_else(
      initial_registration > 0,
      dropout / initial_registration,
      NA_real_
    ),

    dr_w = if_else(
      initial_registration_w > 0,
      dropout_w / initial_registration_w,
      NA_real_
    ),

    dr_m = if_else(
      initial_registration_m > 0,
      dropout_m / initial_registration_m,
      NA_real_
    )
  )

#----------------------------
# 5. Create treatment variables
#----------------------------

df_ddd <- df_ddd %>%
  mutate(
    treatment_group = as.integer(T_group == 3),
    post = as.integer(year_school >= 2023),
    treated = treatment_group * post
  )

#----------------------------
# 6. Triple-difference regressions
#----------------------------

outcomes <- c("dr", "dr_w", "dr_m")

ddd_models <- list()

for (outcome in outcomes) {

  ddd_models[[outcome]] <- feols(
    as.formula(
      paste(
        outcome,
        "~ treated + old +",
        "treatment_group:old +",
        "post:old +",
        "treated:old |",
        "center_code + year_school"
      )
    ),
    data = df_ddd,
    cluster = ~center_code
  )

}

#----------------------------
# 7. Regression results table
#----------------------------

ddd_table <- etable(
  ddd_models,
  dict = c(
    "treated" = "Treatment x Post",
    "old" = "Over-age",
    "treatment_group:old" = "Treatment group x Over-age",
    "post:old" = "Post x Over-age",
    "treated:old" = "Triple difference (DDD)"
  ),
  headers = c(
    "Total",
    "Female",
    "Male"
  ),
  digits = 4,
  fitstat = ~n + r2,
  title = "Triple-Difference Effects on Dropout Rates"
)

print(ddd_table)

#----------------------------
# 8. Inspect regression summaries
#----------------------------

summary(ddd_models$dr)
summary(ddd_models$dr_w)
summary(ddd_models$dr_m)
