#Load data
df <- read_excel(file.path(data, "raw", "excel", "dropout_school_2014_2019.xlsx"), sheet = "AÑO 2014", na = "NA")
df_2 <- read.csv(file.path(data, "raw", "csv", "Report_enrollment_students.csv"))

snap <- head(df, 1000)
snap_2 <- head(df_2, 1000)

# Cosa devi fare
# non c'è il codice centro. Disastro. quindi la cosa migliore è unire anno per anno, verificare che dal nome si riconoscano
# tutti i centri e che quindi lo stesso nome nello stesso municipio non identifichi due centri diversi e poi assegnare
# lo stesso codice centro che si ha nel primo dataset ricevuto.

# Meglio fare anno per anno. Per ciascun anno poi, creare variabile year_school = anno (ex. 2014), cos' che poi 
# Ci sia già quando si unisce con l'altro dataset

#Poi applica tutti i cambiamenti di stile che si spno fatti nel cleaning dell'altro dataset.