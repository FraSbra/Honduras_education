#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#TWFE (static and dynamic) estimation
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

df_all <- read.csv(file.path(data, "processed", "csv", "center_level.csv")) #Treatment data



snap <- head(df_all, 1000)


#Loop: 3 main outcomes (diff, dr, canc) * 3 outcomes versions (total, w, m) * 2 datasets (con e senza dr = 1)
#Different loops depending on the T_groups: 1 alone, 2-3-4, then 5-6.

for (x in 2:4) {

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

  # --------------------------------
  # 5. Event-study graph, merged plots
  # --------------------------------
  plot_colors <- c("#1B9E77", "#7570B3", "#D95F02")
  plot_symbols <- c(16, 16, 16)

  old_par <- par(
  mar = c(7, 5, 4, 1),  # Extra bottom space for legend
  bty = "n",            # Remove surrounding box
  las = 1,              # Horizontal axis tick labels
  tcl = 0,              # Remove axis tick marks
  col.axis = "grey40",
  cex.axis = 0.9
  )

  iplot(
  objects = list(
    dynamic_twfe,
    dynamic_twfe_m,
    dynamic_twfe_w
  ),
  col = plot_colors,
  pch = plot_symbols,
  pt.cex = 0.9,
  sep = 0.07,           # Keep estimates within each year closer
  ci_level = 0.95,
  ci.width = "0.4%",    # Shorter confidence-interval caps
  ci.lwd = 1.2,
  grid = TRUE,
  grid.par = list(col = "grey92", lty = 1, lwd = 1),
  zero.par = list(col = "grey50", lty = 2, lwd = 1),
  ref.line = -1,
  ref.line.par = list(col = "grey40", lty = 3, lwd = 1),
  xlab = "Years relative to treatment",
  ylab = "Effect on dropout rate",
  main = ""
  )

  # Left-aligned title and subtitle
  mtext(
  paste0("Dynamic TWFE: Treatment Group ", x),
  side = 3, line = 2, adj = 0, font = 2, cex = 1.1
  )
  mtext(
  "Event-study coefficients; 95% confidence intervals",
  side = 3, line = 0.6, adj = 0, cex = 0.9
  )

  legend(
  "bottom",
  inset = c(0, -0.55),
  legend = c("Total", "Male", "Female"),
  col = plot_colors,
  pch = plot_symbols,
  horiz = TRUE,
  bty = "n",
  xpd = NA,
  cex = 0.9
  )

  par(old_par)
  

}



#At the moment the TWFE is looping only among the 3 groups (2,3,4)
#  and the 3 outcomes (total, m and f).
#We miss
# - diff and canc
# - tables for TWFE static and Dynamic (possibly grouped in the least number of table possible)
# - dataset with dr=1 removed