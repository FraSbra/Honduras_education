#Harmonize Borders shapefile (the one at the municipality level) to match treatment data

#Load data
correct <- st_read(file.path(data, "raw", "shapefiles", "gadm41_HND_shp", "gadm41_HND_2.shp"), quiet = TRUE) #Adm1 and 2 shapefiles

#----------------------------
#Cleaning
#----------------------------

#Rename shapeName to match with treatment data
correct %>% rename(Departamento = NAME_1) %>%
            rename(Municipio = NAME_2) -> correct

#Lowercase
correct <- correct %>% mutate(Municipio = tolower(Municipio)) %>%
                       mutate(Departamento = tolower(Departamento))

#Correct spelling
#1. Remove accents and special characters
correct <- correct %>%
  mutate(Municipio = stri_trans_general(str = Municipio, id = "Latin-ASCII"))
correct <- correct %>%
  mutate(Departamento = stri_trans_general(str = Departamento, id = "Latin-ASCII"))

#2. Correct specific mismatches
correct <- correct %>% 
  mutate(Municipio = ifelse(Municipio == "ojo de agua" & Departamento == "comayagua", "ojos de agua", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "trinidad" & Departamento == "copan", "trinidad de copan", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "san marcos de la sierra" & Departamento == "intibuca", "san marcos de sierra", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "aguaqueterique" & Departamento == "la paz", "aguanqueterique", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "nuevo celilac" & Departamento == "santa barbara", "nueva celilac", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "trinidad de copan" & Departamento == "santa barbara", "trinidad", Municipio)) %>%
  mutate(
    Municipio = ifelse(
      grepl("\\bfrancisco\\b", Municipio) & Departamento == "gracias a dios",
      "juan francisco bulnes",
      Municipio
    )
  )

#----------------------------
#END
#----------------------------

#Save
st_write(
  correct,
  dsn = file.path(data, "processed", "shapefiles", "borders_H_municipalities.gpkg"),
  layer = "borders_H_municipalities",
  delete_layer = TRUE,
  quiet = TRUE
)

#Clear Environment
rm(correct)

print("4_borders_cleaning.R run completed")