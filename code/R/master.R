#------------------------
# Runtime start
#------------------------
#start_time <- Sys.time()

#------------------------
# Install and load packages
#------------------------ 
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("stringi")
#install.packages("data.table")
#install.packages("sf")
#install.packages("ggplot2")
#install.packages("stringr")
#install.packages("tidyr")
#install.packages("purrr")
#install.packages("collapse")
#install.packages(c("knitr", "kableExtra"))  #For .tex tables


library(readxl)
library(dplyr)
library(stringi)
library(data.table)
library(sf)
library(ggplot2)
library(stringr) 
library(tidyr)
library(purrr)
library(collapse)
library(knitr)
library(kableExtra)



#------------------------
# Set main paths
#------------------------
main <- "/Users/fra/Library/CloudStorage/GoogleDrive-francesco.sbrana@carloalberto.org/My Drive/CCA/Projects/Mano_Dura/Honduras/replication_package"
  
  code <- file.path(main, "code")
    R <- file.path(code, "R")
      helper <- file.path(R, "helpers")
      cleaning <- file.path(R, "data_cleaning")
      analysis <- file.path(R, "main")

  data <- file.path(main, "data")

  output <- file.path(main, "output")

#Helper Functions
source(file.path(helper, "Tex_table_fct.R"))

#Cleaning scripts
source(file.path(cleaning, "0_treatment.R"))
source(file.path(cleaning, "0.1_treatment_date.R"))
source(file.path(cleaning, "1_school_datasets_cleaning.R"))
source(file.path(cleaning, "2_enrollment_duplicates_balancing.R"))
source(file.path(cleaning, "2.1_enrollment_treatment_merging.R"))
source(file.path(cleaning, "3_dataset_analysis_creation.R"))
source(file.path(cleaning, "4_borders_cleaning.R"))
source(file.path(cleaning, "5_borders_treatment_merging.R"))

#Maps
source(file.path(analysis, "0_treatment_maps.R"))

#Main analysis

#------------------------
# End run time
#------------------------
#end_time <- Sys.time()
#runtime <- end_time - start_time

#cat("Runtime:", runtime, "\n")
