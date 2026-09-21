#This file checks for duplicates and for other problematic schools/observations
# in particular: unify schools to center code, 
#                flag problematic centers,
#                balancing flagging.

#Load data
school <- read.csv(file.path(data, "processed", "csv", "enrollment_cleaned.csv"))

#------------------------------
#According to SACE code, the last 3 digits identify the track type. However, the school is the same
#So, create new school code that identify only the school center
#------------------------------

school <- school %>%
  mutate(center_code = str_extract(school_code, "^[0-9]+"))

#------------------------------
#The assumption from now on is that center_code identifies unique schools.
#Unable to check better at the moment. Will do in the future.
#------------------------------

#------------------------------
# Check if center_code changes across deterministic variables (department, municipality, admin type and area type)
#------------------------------

vars <- c("Departamento", "Municipio", "admin_type", "area_type")

checks <- list()
changes <- list()
problematic_centers <- list()

for (var in vars) {

  check_name <- paste0(var, "_check")
  change_name <- paste0(var, "_changes")
  flag_name <- paste0(var, "_problem")

  # Create a check (take this outside the loop. There is a problem if creates a dataset with positive number of observations)
  checks[[check_name]] <- school %>%
    group_by(center_code) %>%
    filter(n_distinct(.data[[var]], na.rm = TRUE) > 1) %>%
    ungroup()

  # Create a summary of changes (which are the possible values the variable has within each problematic center code?)
  changes[[change_name]] <- checks[[check_name]] %>%
    group_by(center_code) %>%
    summarise(
      n_values = n_distinct(.data[[var]], na.rm = TRUE),
      values = paste(sort(unique(.data[[var]])), collapse = " | "),
      .groups = "drop"
    ) %>%
    arrange(desc(n_values), center_code)

  # Create list of problematic center_codes
  problematic_centers[[var]] <- changes[[change_name]] %>%
    select(center_code) %>%
    distinct()

  #Add Flag to original dataset
   school <- school %>%
    mutate(
      "{flag_name}" := as.integer(center_code %in% problematic_centers[[var]]$center_code)
    )

}

# Print number of problematic centers depending on the 4 variables
sapply(problematic_centers, nrow) #23 Mun, 18 admin, 89 area

#------------------------------
# Inpect all observations of one random PROBLEMATIC center_code, for each variable above
#------------------------------

rand <- list()

for (var_to_check in vars) {
  
  one_center <- problematic_centers[[var_to_check]] %>%
    slice_sample(n = 1) %>%
    pull(center_code)
  
  one_center <- one_center[1]
  
  rand_name <- paste0("random_", var_to_check)
  
  rand[[rand_name]] <- school %>%
    filter(center_code == one_center) %>%
    arrange(year_school)
  
  print(paste(rand_name, "uses center_code:", one_center))
}

# Export list elements as separate datasets in the workspace
list2env(rand, envir = .GlobalEnv)

#SOME NOTES
# - Area (at least in one of the random center) changes accordingly with the school type.
#     Rural area for pre-basica and urban in the rest.
#     Maybe, I can put as covariate the Value that appear for most observations (over a certain treshold).



#Take into account that MAYBE it is not true that center_code is a good way of selecting the id



#------------------------------
# Flag centers that do not have observations for all years in the sample
#------------------------------

#Calculate number of centers appearing in each year
n_centers_per_year <- school %>%
  group_by(year_school) %>%
  summarise(unique_center_codes = n_distinct(center_code))

#Graphs of center per year
ggplot(n_centers_per_year, aes(x = year_school, y = unique_center_codes)) +
  geom_line() +
  geom_point(size = 2) +
  geom_text(
    aes(label = unique_center_codes),
    vjust = -2,
    size = 3.5
  ) +
  labs(
    title = "Center count",
    x = "Year",
    y = "Number of Centers"
  ) +
  coord_cartesian(ylim = c(23000, 24000)) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

#

#Flag centers that are not present in all years
school <- school %>%
  left_join(
    school %>%
      distinct(center_code, year_school) %>%
      mutate(present = 0L) %>%
      pivot_wider(
        names_from = year_school,
        values_from = present,
        names_prefix = "flag_",
        values_fill = 1L
      ) %>%
      mutate(
        flagged_center = as.integer(
          if_any(starts_with("flag_"), ~ .x == 1L)
        )
      ),
    by = "center_code"
  )

#Print number of balanced and flagged centers
n_balanced_centers <- school %>%
  filter(flagged_center == 0L) %>%
  summarise(
    n = n_distinct(center_code)
  ) %>%
  pull(n)

n_flagged_centers <- school %>%
  filter(flagged_center == 1L) %>%
  summarise(
    n = n_distinct(center_code)
  ) %>%
  pull(n)

print(n_balanced_centers) #21121
print(n_flagged_centers) #4525

#Around 20% of the panel consists of centers appearing only in some years. Should delete them?

#OK TILL HERE





#------------------------------
# Check if names are the same across center codes
#------------------------------

#Count school_names for each center code. Filter for problematic ones
distinct_school_names <- school %>% 
  group_by(center_code) %>%
  summarise(
    distinct_names = n_distinct(school_name)
  )%>%
  filter(distinct_names > 1) #Filter only centers with more than 1 school name
  
distinct_school_names #667 centers with more than 1 school_name

#Show which names appear in those 667 centers and which are the common words
school_names_dataset <- school %>%
  filter(!is.na(school_name)) %>%
  distinct(center_code, school_name) %>%
  group_by(center_code) %>%
  summarise(
    school_names = list(sort(unique(school_name))),
    distinct_names = n_distinct(school_name),
    .groups = "drop"
  ) %>%
  filter(distinct_names > 1) %>%
  mutate(
    # Standardize every school name
    standardized_names = map(
      school_names,
      ~ .x %>%
        str_to_lower() %>%
        stri_trans_general("Latin-ASCII") %>%
        str_replace_all("[^a-z0-9 ]", " ") %>%
        str_squish()
    ),

    # Split every standardized name into its component words
    words_by_name = map(
      standardized_names,
      ~ map(.x, \(name) unique(str_split(name, "\\s+")[[1]]))
    ),

    # Find words occurring in every school name for that center
    common_words = map(
      words_by_name,
      ~ reduce(.x, intersect)
    ),

    # 1 when there is at least one word shared by all names
    has_common_word = as.integer(lengths(common_words) > 0),

    # Convert common words into a readable character variable
    common_words_text = map_chr(
      common_words,
      ~ paste(sort(.x), collapse = ", ")
    ),

    # Optional: readable version of the original names
    names = map_chr(
      school_names,
      ~ paste(.x, collapse = " | ")
    )
  ) #There are some with no common words (and some where the common words are only `el`, `de`, ...)
    #In future need to address this
    #They are only 667 in total, so it will be easy and fast to do it manually


#------------------------------
# Check and Remove duplicates
#------------------------------

school_dt <- as.data.table(school)
duplicates <- school_dt[duplicated(school_dt)] #There are no duplicates.

#------------------------------
# Check registration values
#------------------------------

#We want to check that final registrations are not higher than initial ones

count <- 0

#Loop over all observations
for (i in 1:nrow(school)) {
  if (school$initial_registration[i] - school$final_registration[i] < 0) {
    count <- count + 1
  }
} #Since the resulting number is 0, there are no problems


#Same for the sum of dropout and cancellations
count <- 0

#Loop over all observations
for (i in 1:nrow(school)) {
  if (school$dropout[i] + school$cancellations[i] != school$initial_registration[i] - school$final_registration[i]) {
    count <- count + 1
    cat("Sum", school$dropout[i] + school$cancellations[i], "Diff", school$initial_registration[i] - school$final_registration[i], "\n")
  }
} #There are 3 problematic observations!!!! Pattern --> 1 more kid leave school. Not a major problem for now.




#Save
#1. Save total
write.csv(school, file.path(data, "processed", "csv", "enrollment_cleaned_flagged.csv"), row.names = FALSE) 

#Clear Environment
rm(list = setdiff(ls(), c("main", "code", "R", "cleaning", "analysis", "data", "output", "start_time"))) #Removes everything except what needed




print("2_enrollment_duplicates_balancing.R run completed")
