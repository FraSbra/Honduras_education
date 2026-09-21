##############################################
# Define datasets for analysis
##############################################

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

#----------------------------
# Collapse to center level. Will address this in future, or as robustness
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
	"area_type"
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

df_center_level <- collap(
	df_balanced,
	~ year_school + center_code,
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

df_center_level <- df_center_level |>
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


#Save
write.csv(df_center_level, file = file.path(data, "processed", "csv", "center_level.csv"), row.names = FALSE)

#Clean workspace
rm(list = setdiff(ls(), c("main", "code", "R", "cleaning", "analysis", "data", "output", "start_time"))) #Removes everything except what needed




print("3_dataset_analysis_creation.R run completed")






