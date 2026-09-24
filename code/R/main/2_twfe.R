#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#TWFE (static and dynamic) estimation
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

df_all <- read.csv(file.path(data, "processed", "csv", "center_level.csv")) #Treatment data



snap <- head(df_all, 1000)


#Loop: 3 main outcomes (diff, dr, canc) * 3 outcomes versions (total, w, m) * 2 datasets (con e senza dr = 1)
#Different loops depending on the T_groups: 1 alone, 2-3-4, then 5-6.

for (x in 3:3) {

  df_x <- df_all |> 
	filter(
    	T_group == 0 | T_group == .env$x
    	) |>
  mutate(
    treated = ifelse(
      T_group == .env$x & year_school >= 2023,
      1, 0),
    treated_group = if_else(T_group == .env$x, 1L, 0L),
    event_time = year_school - 2023
  )



  # --------------------------------
  # 1. TWFE
  # --------------------------------

  twfe <- feols(
  dr ~ treated | center_code + year_school,
  data = df_x,
  cluster = ~center_code
  )

  twfe_w <- feols(
  dr_w ~ treated | center_code + year_school,
  data = df_x,
  cluster = ~center_code
  )

  twfe_m <- feols(
  dr_m ~ treated | center_code + year_school,
  data = df_x,
  cluster = ~center_code
  )

  #print(summary(twfe))
  #print(summary(twfe_w))
  #print(summary(twfe_m))

  # --------------------------------
  # 2. Dynamic TWFE / Event Study
  # --------------------------------

  dynamic_twfe <- feols(
  dr ~ i(event_time, treated_group, ref = -1) |
    center_code + year_school,
  data = df_x,
  cluster = ~center_code
  )

  dynamic_twfe_w <- feols(
  dr_w ~ i(event_time, treated_group, ref = -1) |
    center_code + year_school,
  data = df_x,
  cluster = ~center_code
  )

  dynamic_twfe_m <- feols(
  dr_m ~ i(event_time, treated_group, ref = -1) |
    center_code + year_school,
  data = df_x,
  cluster = ~center_code
  )

  #print(summary(dynamic_twfe))
  #print(summary(dynamic_twfe_w))
  #print(summary(dynamic_twfe_m))

  # --------------------------------
  # 4. Event-study graph
  # --------------------------------

  iplot(
  dynamic_twfe,
  xlab = "Years relative to treatment",
  ylab = "Effect on dropout rate",
  main = paste0("Dynamic TWFE: Treatment Group ", x),
  ref.line = -1
  )

  iplot(
  dynamic_twfe_w,
  xlab = "Years relative to treatment",
  ylab = "Effect on dropout rate",
  main = paste0("Dynamic TWFE (W): Treatment Group ", x),
  ref.line = -1
  )

  iplot(
  dynamic_twfe_m,
  xlab = "Years relative to treatment",
  ylab = "Effect on dropout rate",
  main = paste0("Dynamic TWFE (M): Treatment Group ", x),
  ref.line = -1
  )



  

}

  

plot_colors <- c("black", "#0072B2", "#D55E00")
  plot_symbols <- c(16, 17, 15)

  iplot(
  objects = list(
    dynamic_twfe,
    dynamic_twfe_m,
    dynamic_twfe_w
  ),
  col = plot_colors,
  pch = plot_symbols,
  sep = 0.15,
  ci_level = 0.95,
  xlab = "Years relative to treatment",
  ylab = "Effect on dropout rate",
  main = paste0("Dynamic TWFE: Treatment Group ", x),
  ref.line = -1
  )

  legend(
  "topleft",
  legend = c("Total", "Male", "Female"),
  col = plot_colors,
  pch = plot_symbols,
  bty = "n"
  )

