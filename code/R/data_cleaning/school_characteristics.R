#----------------------
#Load data
#----------------------
df <- read.csv(file.path(data, "processed", "csv", "school_characteristics_extracted.csv")) #Treatment data

snap <- head(df, 1000)

df <- df %>%
  mutate(center_code = str_extract(codigo_centro, "^[0-9]+"))


#----------------------------
# Collapse to center level. Use means of the schools' covariates
#----------------------------

first_vars <- c(
	"departamento",
	"municipio",
	"aldea"
)

sum_vars <- c(
	"ninas",
	"ninos",
	"total"
)

mean_vars <- c(
  "mobiliario",
  "servicios_basicos",
  "amenazas_naturales",
  "amenazas_sociales",
  "hidrosanitaria",
  "infraestructura",
  "indice_precariedad"
)

#Main Dataset
df_center_level <- collap(
	df,
	~ center_code,
	custom = list(
      ffirst = first_vars, #take first value
	  fsum = sum_vars,     #take sum
    fmean = mean_vars #Take mean
	)
)     #Note: here missing values are not counted in the means, by default --> mean btw 0.6 and NA = 0.6



#Save
write.csv(df_center_level, file = file.path(data, "processed", "csv", "school_covariates_center_level.csv"), row.names = FALSE)

