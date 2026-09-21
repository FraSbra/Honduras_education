#Harmonize treatment date data (spelling, lowercase, ...)
#Collapse to Municipio level
#Keep both Municipio and ALdea level datasets

#Load data
df <- read.csv(file.path(data, "processed", "csv", "treatment_date.csv")) #Treatment data

#---------------------------
#Renaming / Correct Spelling
#---------------------------

#1 - Rename shapeName to match with treatment data
df %>% rename(Municipio = MuniCipio) -> treat

#2 - Lowercase
treat <- treat %>% mutate(Municipio = tolower(Municipio)) %>%
                   mutate(Departamento = tolower(Departamento))

#3 - Remove accents and special characters
treat <- treat %>%
  mutate(Municipio = stri_trans_general(str = Municipio, id = "Latin-ASCII"))
treat <- treat %>%
  mutate(Departamento = stri_trans_general(str = Departamento, id = "Latin-ASCII"))

#4 - Correct specific mismatches
treat <- treat %>% 
  mutate(
    Municipio = ifelse(
      grepl("\\bfrancisco\\b", Municipio) & Departamento == "gracias a dios",
      "juan francisco bulnes",
      Municipio
    )
  )


#---------------------------
#Collapse to Municipio level
#---------------------------

# Collapse by Municipio-Departamento
#1. Keep only the first observation for each Municipio-Departamento pair
treat_collapsed <- treat %>%
  group_by(Municipio, Departamento) %>%
  slice(1) %>%
  ungroup()

#2. Remove Aldea vars
treat_collapsed <- subset(treat_collapsed, select = -c(ID.Aldea, Aldea) )


#---------------------------
# Generate Treatment Cohort
#---------------------------

#For the purposes of all the paper until I get school level data, I NEED to set all San Pedro Sula and Distrito central
#tratment dates equal to the main cities (starting date of the measure). Remain the same in the NOT COLLAPSED dataset.
#Notice that IT IS NOT P-HACKING --> it is like removing UNSURE observations. Group 1 cannot be use (at the moment) 
#because of spillover concerns.
#What else? --> Creat treatment groups and subgroups (at the moment they are created in the DiD file).

#Change treat dates
treat_collapsed <- treat_collapsed %>%
  mutate(first_treatment = ifelse(Municipio == "san pedro sula" | Municipio == "distrito central", "6_12_2022", first_treatment)) %>%
  mutate(t_day = ifelse(Municipio == "san pedro sula" | Municipio == "distrito central", 6, t_day)) %>%
  mutate(t_month = ifelse(Municipio == "san pedro sula" | Municipio == "distrito central", 12, t_month)) %>%
  mutate(t_year = ifelse(Municipio == "san pedro sula" | Municipio == "distrito central", 2022, t_year))

#Create groups
treat_collapsed <- treat_collapsed %>%
  mutate(treat_1 = ifelse(Municipio == "san pedro sula" | Municipio == "distrito central", 1, 0)) %>%
  mutate(treat_2 = ifelse(!is.na(first_treatment) & t_day == 6 & t_month == 1 & t_year == 2023, 1, 0)) %>%
  mutate(treat_3 = ifelse(!is.na(first_treatment) & t_day == 20 & t_month == 2 & t_year == 2023, 1, 0)) %>%
  mutate(treat_4 = ifelse(!is.na(first_treatment) & t_day == 19 & t_month == 8 & t_year == 2023, 1, 0)) %>%
  mutate(treat_5 = ifelse(!is.na(first_treatment) & t_day == 1 & t_month == 1 & t_year == 2024, 1, 0)) %>%
  mutate(treat_6 = ifelse(!is.na(first_treatment) & t_day == 19 & t_month == 2 & t_year == 2024, 1, 0))

#Save
#1. Save total
write.csv(treat, file.path(data, "processed", "csv", "treatment_date_cleaned_full.csv"), row.names = FALSE) #Saved at Aldea level

#2. Save collapsed
write.csv(treat_collapsed, file.path(data, "processed", "csv", "treatment_date_cleaned_collapsed.csv"), row.names = FALSE) #Saved at Municipio level

#Clear Environment
rm(df, treat, treat_collapsed)

print("0.1_treatment_date.R run completed")
