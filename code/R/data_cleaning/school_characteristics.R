#----------------------
#Load data
#----------------------
df <- read.csv(file.path(data, "processed", "csv", "school_characteristics_extracted.csv")) #Treatment data

snap <- head(df, 1000)

df <- df %>%
  mutate(center_code = str_extract(codigo_centro, "^[0-9]+"))
