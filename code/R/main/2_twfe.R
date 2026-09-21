install.packages("fixest")   # only need to do this once
library(fixest)

df <- read.csv(file.path(data, "processed", "csv", "center_level.csv")) #Treatment data

snap <- head(df, 1000)

df <- df |>
  mutate(
    dr = if_else( #Total dropout rate
      initial_registration == 0,
      NA_real_,
      (initial_registration - final_registration) / initial_registration
    ),
    dr_w = if_else( 
      initial_registration_w == 0,
      NA_real_,
      (initial_registration_w - final_registration_w) / initial_registration_w
    ),
    dr_m = if_else(
      initial_registration_m == 0,
      NA_real_,
      (initial_registration_m - final_registration_m) / initial_registration_m
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
  
df <- df |> 
	filter(
    	T_group == 0 | T_group == 3,
    	)

df$treated <- ifelse(
  df$T_group == 3 & df$year_school >= 2022,
  1, 0
)

# --------------------------------
# 1. TWFE
# --------------------------------

twfe <- feols(
  dr ~ treated | center_code + year_school,
  data = df,
  cluster = ~center_code
)

summary(twfe)


# --------------------------------
# 2. Create event time
# --------------------------------

df <- df |>
  mutate(
    treated_group = if_else(T_group == 3, 1L, 0L),
    event_time = year_school - 2022
  )

# --------------------------------
# 3. Dynamic TWFE / Event Study
# --------------------------------

dynamic_twfe <- feols(
  dr ~ i(event_time, treated_group, ref = -1) |
    center_code + year_school,
  data = df,
  cluster = ~center_code
)

summary(dynamic_twfe)

# --------------------------------
# 4. Event-study graph
# --------------------------------

iplot(
  dynamic_twfe,
  xlab = "Years relative to treatment",
  ylab = "Effect on dropout rate",
  main = "Dynamic TWFE: Treatment Group 3",
  ref.line = -1
)



dynamic_twfe_m <- feols(
  dr_m ~ i(event_time, treated_group, ref = -1) |
    center_code + year_school,
  data = df,
  cluster = ~center_code
)

summary(dynamic_twfe_m)

# --------------------------------
# 4. Event-study graph
# --------------------------------

iplot(
  dynamic_twfe_w,
  xlab = "Years relative to treatment",
  ylab = "Effect on dropout rate",
  main = "Dynamic TWFE: Treatment Group 3",
  ref.line = -1
)
text