#Transform treatment dataset from excel to csv.
#Generate also day, month, year variables.

#Load Data
df <- read_excel(
  file.path(data, "raw", "coded", "excel", "treatment_date.xlsx"),
  col_types = c("guess","guess","guess","guess","guess","guess","text","text")
)

# Extract day, month, and year from treatment date
df$t_day <- as.integer(sub("_.*$", "", df$first_treatment))
df$t_month <- as.integer(sub("^[^_]+_([^_]+)_.*$", "\\1", df$first_treatment))
df$t_year <- as.integer(sub("^.*_([^_]+)$", "\\1", df$first_treatment))

#Save
write.csv(df, file = file.path(data, "processed", "csv", "treatment_date.csv"), row.names = FALSE)

#Clear Environment
rm(df)

print("0_treatment run completed")
