##############################################
#.  Here just merge enrollment data with treatment
##############################################

#Load data
school <- read.csv(file.path(data, "processed", "csv", "enrollment_cleaned_flagged.csv"))
treat <- read.csv(file.path(data, "processed", "csv", "treatment_date_cleaned_collapsed.csv")) #Treatment data

#----------------------------
#Merge
#----------------------------

#1. Setting up
setDT(school)
setDT(treat)

#2. Mark rows that exist in treat before join (to check quality of merging later)
treat[, in_treat := 1L]

#3. Actual merge
setkey(treat, Municipio, Departamento)
merged <- treat[school, on = .(Municipio, Departamento)]   # left join like all.x=TRUE

#4. If 0, merged correctly
merged[is.na(in_treat), .N]

#5. Other check. If both empty, perfect merge.
school_only <- fsetdiff(
  unique(school[, .(Municipio, Departamento)]),
  unique(treat[, .(Municipio, Departamento)])
)

treat_only <- fsetdiff(
  unique(treat[, .(Municipio, Departamento)]),
  unique(school[, .(Municipio, Departamento)])
)

#6. Look at the new dataset
dplyr::glimpse(merged)

#Save
write.csv(merged, file = file.path(data, "processed", "csv", "enrollment_treatment_merged.csv"), row.names = FALSE)

rm(merged, school, school_only, treat, treat_only)

print("2.1_enrollment_treatment_merging.R run completed")
