


#Load data
school_1 <- read.csv(file.path(data, "raw", "csv", "Report_passed_students.csv"))
school_2 <- read.csv(file.path(data, "raw", "csv", "Report_enrollment_students.csv"))

#----------------------------
#Cleaning
#----------------------------

#1. For loop for stuff to do on both datasets
schools <- list(school_1, school_2)
for (i in 1:2) {
#A. Rename vars
school <- schools[[i]] %>% 
  rename(Municipio = MUNICIPIO) %>%
  rename(Departamento = DEPARTAMENTO) %>%
  rename(year_school = AÑO.ESCOLAR) %>%
  rename(admin_type = ADMINISTRACIÓN) %>%
  rename(area_type = TIPO.ZONA) %>%
  rename(school_code = CODIGO.DE.CENTRO) %>%
  rename(school_name = NOMBRE.DEL.CENTRO) %>%
  rename(edu_level = NIVEL.EDUCATIVO) %>%
  rename(grade = GRADO.ACADEMICO) %>%
  rename(age_students = EDAD.ALUMNOS)

#B. Lowercase
school <- school %>% 
  mutate(Municipio = tolower(Municipio)) %>%
  mutate(Departamento = tolower(Departamento)) %>%
  mutate(admin_type = tolower(admin_type)) %>%
  mutate(area_type = tolower(area_type)) %>%
  mutate(school_name = tolower(school_name)) %>%
  mutate(edu_level = tolower(edu_level)) %>%
  mutate(grade = tolower(grade))


#C. Remove accents and special characters from needed vars
school <- school %>%
  mutate(Municipio = stri_trans_general(str = Municipio, id = "Latin-ASCII")) %>%
  mutate(Departamento = stri_trans_general(str = Departamento, id = "Latin-ASCII")) %>%
  mutate(admin_type = stri_trans_general(str = admin_type, id = "Latin-ASCII")) %>%
  mutate(area_type = stri_trans_general(str = area_type, id = "Latin-ASCII")) %>%
  mutate(school_name = stri_trans_general(str = school_name, id = "Latin-ASCII")) %>%
  mutate(edu_level = stri_trans_general(str = edu_level, id = "Latin-ASCII")) %>%
  mutate(grade = stri_trans_general(str = grade, id = "Latin-ASCII"))
 

#D. Correct specific mismatches
school <- school %>%
  mutate(Municipio = ifelse(Municipio == "ojo de agua" & Departamento == "comayagua", "ojos de agua", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "trinidad" & Departamento == "copan", "trinidad de copan", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "san fco. de yojoa" & Departamento == "cortes", "san francisco de yojoa", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "villa nueva" & Departamento == "cortes", "villanueva", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "curarem" & Departamento == "francisco morazan", "curaren", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "juan fco. bulnes" & Departamento == "gracias a dios", "juan francisco bulnes", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "villeda morales" & Departamento == "gracias a dios", "ramon villeda morales", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "wanpusirpi" & Departamento == "gracias a dios", "wampusirpi", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "san marcos de la sierra" & Departamento == "intibuca", "san marcos de sierra", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "san fco. de opalaca" & Departamento == "intibuca", "san francisco de opalaca", Municipio)) %>%  
  mutate(Municipio = ifelse(Municipio == "guaijiquiro" & Departamento == "la paz", "guajiquiro", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "san pedro de tulule" & Departamento == "la paz", "san pedro de tutule", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "santiago puringla" & Departamento == "la paz", "santiago de puringla", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "san manuel de colohete" & Departamento == "lempira", "san manuel colohete", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "san fco. del valle" & Departamento == "ocotepeque", "san francisco del valle", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "san marcos de ocotepeque" & Departamento == "ocotepeque", "san marcos", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "san fco. de becerra" & Departamento == "olancho", "san francisco de becerra", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "san fco. de la paz" & Departamento == "olancho", "san francisco de la paz", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "san fco de ojuera" & Departamento == "santa barbara", "san francisco de ojuera", Municipio)) %>%
  mutate(Municipio = ifelse(Municipio == "san fco. de coray" & Departamento == "valle", "san francisco de coray", Municipio))

#E. Assign changes to the list
schools[[i]] <- school

}

#2. Change original datasets as in the lists
school_1 <- schools[[1]]
school_2 <- schools[[2]]

#3. Single changes
#A School_1
school_1 <- school_1 %>%
  rename(approved_w = APROBADOS.MUJER) %>%
  rename(approved_m = APROBADOS.HOMBRES) %>%
  rename(approved = APROBADOS) %>%
  rename(failed_w = REPROBADOS.MUJER) %>%
  rename(failed_m = REPROBADOS.HOMBRE) %>%
  rename(failed = REPROBADOS)

#B School_2
school_2 <- school_2 %>%
  rename(initial_registration_w = MATRÍCULA.INICIAL.MUJER) %>%
  rename(initial_registration_m = MATRÍCULA.INICIAL.HOMBRE) %>%
  rename(initial_registration = MATRÍCULA.INICIAL) %>%
  rename(dropout_w = DESERCIÓN.MUJER) %>%
  rename(dropout_m = DESERCIÓN.HOMBRE) %>%
  rename(dropout = DESERCIÓN) %>%
  rename(cancellations_w = CANCELACIONES.MUJER) %>%
  rename(cancellations_m = CANCELACIONES.HOMBRE) %>%
  rename(cancellations = CANCELACIONES) %>%
  rename(final_registration_w = MATRÍCULA.FINAL.MUJER) %>%
  rename(final_registration_m = MATRÍCULA.FINAL.HOMBRE) %>%
  rename(final_registration = MATRÍCULA.FINAL) %>%
  rename(repetition_w = REPITENCIA.MUJER) %>%
  rename(repetition_m = REPITENCIA.HOMBRE) %>%
  rename(repetition = REPITENCIA)

school_2 <- school_2 %>%
  mutate(edu_level = ifelse(edu_level == "educacion basica", "basica", edu_level)) %>%
  mutate(edu_level = ifelse(edu_level == "educacion media", "media", edu_level)) %>%
  mutate(edu_level = ifelse(edu_level == "educacion prebasica", "prebasica", edu_level)) 

#----------------------------
# END
#----------------------------

#Checks
#head(school_1, 10)
#head(school_2, 10)

#dplyr::glimpse(school_1)
#dplyr::glimpse(school_2)

#----------------------------
#Create merged schooling dataset
#----------------------------

setDT(school_1)
setDT(school_2)

school <- merge(school_1, school_2, by = c("school_code", "grade", "age_students", "year_school", "Departamento", "Municipio"), all.y = TRUE)

#Check merge
#dplyr::glimpse(school)

#Clean from useless vars
school = subset(school, select = -c(admin_type.x, area_type.x, school_name.x, edu_level.x))

school <- school %>%
  rename(admin_type = admin_type.y) %>%
  rename(area_type = area_type.y) %>%
  rename(school_name = school_name.y) %>%
  rename(edu_level = edu_level.y)

#----------------------------
# END
#----------------------------

#Save
write.csv(school_1, file.path(data, "processed", "csv", "passed_students_cleaned.csv"), row.names = FALSE)
write.csv(school_2, file.path(data, "processed", "csv", "enrollment_cleaned.csv"), row.names = FALSE)
write.csv(school, file.path(data, "processed", "csv", "school_merged_cleaned.csv"), row.names = FALSE)

#Clear Environment
rm(school_1, school_2, school, schools, i)

print("1_school_datasets_cleaning.R run completed")

