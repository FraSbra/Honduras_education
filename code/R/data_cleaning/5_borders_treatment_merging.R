#Merge treatment data with borders shapefile

#Load data
treat <- read.csv(file.path(data, "processed", "csv", "treatment_date_cleaned_collapsed.csv")) #Treatment data
correct <- st_read(
  file.path(data, "processed", "shapefiles", "borders_H_municipalities.gpkg"),
  layer = "borders_H_municipalities",
  quiet = TRUE
)

#----------------------------
#Merge
#----------------------------

merged <- correct %>% 
  left_join(treat, by = c("Municipio", "Departamento"))

#Check if unmatched
# rows in treat with no match in adm
treat_unmatched <- treat %>%
  distinct(Municipio, Departamento) %>%
  anti_join(correct %>% distinct(Municipio, Departamento),
            by = c("Municipio", "Departamento"))

# rows in adm with no match in treat
adm_unmatched <- correct %>%
  distinct(Municipio, Departamento) %>%
  anti_join(treat %>% distinct(Municipio, Departamento),
            by = c("Municipio", "Departamento"))
#Count unmatched
nrow(treat_unmatched)
nrow(adm_unmatched)

#----------------------------
# END
#----------------------------

print("5_borders_treatment_merging.R run completed")

