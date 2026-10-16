# Locate the conversion script
school_python <- file.path(python, ".venv", "bin", "python")
school_extractor <- file.path(python, "school_pdf_to_csv.py")

# Run the converter
school_status <- system2(
  command = school_python,
  args = c(
    shQuote(school_extractor),
    "--project-root", shQuote(main),
    "--overwrite"
  )
)

# Stop the R pipeline if conversion fails
if (school_status != 0L) {
  stop("School PDF conversion failed. Check the error above.")
}

rm(list = setdiff(ls(), 
          c("main", "code", "R", "python", "cleaning", "analysis", "data", 
            "output", "start_time", "helper", "export_latex_table", "make_latex_table"))) #Removes everything except what needed
