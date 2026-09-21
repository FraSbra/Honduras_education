# README for "Being Tough on Crime - Effects on school dropout" (Sbrana F.)

## Overview

The code in this replication package constructs the analysis file from multyple data sources (GADM 2026, SEDUC 2026, author's 2026) using R only. One main file runs all of the code to generate the data for the 6 figures. The rest of the code regards Data Cleaning activities. The replicator should expect the code to run for about 10 minutes.

## Data Availability and Provenance Statements

### Statement about Rights

- [x] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript. 
- [ ] I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package. Appropriate permission are documented in the LICENSE.txt file file.

### Summary of Availability

- [ ] All data **are** publicly available.
- [x] Some data **cannot be made** publicly available.
- [ ] **No data can be made** publicly available.

- [x] Confidential data used in this paper and not provided as part of the public replication package will be preserved for 10 years after publication, in accordance with journal policies. 


| Data.Name  | Data.Files | Location | Provided | Citation |
| -- | -- | -- | -- | -- | 
| “School Enrollment Data” | Report_enrollment_students.csv | data/raw/csv | TRUE | SEDUC (2026) |
| “Administrative Borders” | gadm41_HND_2.shp | data/raw/shapefiles/gadm41_HND_shp | TRUE | GADM (2026) |
| “Treatment Dates” | treatment_date.xlsx | data/raw/coded/excel | TRUE | author's production |

where the `Data.Name` column is then expanded in the subsequent paragraphs, and both `SEDUC (2026)` and `GADM (2026)` are resolved in the References section of the README.

### Details on each Data Source

### Treatment Dates

The "Treatment Dates" data used to support the findings of this study were collected by the authors, and are available under a Creative Commons Non-commercial license.
Treatment times are recovered from official PDFs of the Government’s decrees downloaded [here](https://portalunico.iaip.gob.hn/433/24/) on 29/01/2026.

Datafile:  `treatment_date.xlsx`

### Administrative Borders

Data on Administrative Borders were downloaded from "GADM maps and data" (GADM, 2026) on 9/02/2026. Data can be downloaded from https://gadm.org/download_country.html#google_vignette. Select `Honduras` as Country, click on `Shapefile` to download. Multiple data files will be downloaded. `gadm41_HND_2.shp` is the only one used in the programs. A copy of the data is provided as part of this archive. The data are in the public domain.

Datafile:  `gadm41_HND_2.shp`

### School Enrollment Data

Data on School Enrollment have been obtained from the Secretaria de Education (SEDUC, 2026), on 30/01/2026. Data is provided here only for the purpose of this replication package. However, permission for redistribution is denied. Data can be requested freely [here](https://sielho.iaip.gob.hn/inicio/), addressing SEDUC after registration. It takes usually around 20 days (minimum) to obtain the dataset.

Datafile:  `Report_enrollment_students.csv`


## Dataset list

| Data file | Source | Notes    |Provided |
|-----------|--------|----------|---------|
| `data/raw/csv/Report_enrollment_students.csv` | SEDUC (2026) | Confidential | Yes |
| `data/raw/shapefiles/gadm41_HND_shp/gadm41_HND_2.shp` | GADM (2026) | As per terms of use | Yes |
| `data/raw/coded/excel/treatment_date.xlsx`| author's production | See LICENSE.txt | Yes |

## Computational requirements

### Software Requirements

- [x] The replication package contains one or more programs to install all dependencies and set up the necessary directory structure.

- R 4.5.2 (last run ...)
  - `ggplot2` (4.0.2)
  - `sf` (1.0-24)
  - `data.table` (1.18.2.1)
  - `stringi` (1.8.7)
  - `dplyr` (1.2.0)
  - `readxl` (1.4.5)
  - the file "`master.R`" will install all dependencies (latest version).

### Controlled Randomness

- [ ] Random seed is set at line _____ of program ______
- [ ] The analysis relies on random number generation, but setting a seed is not possible (explanation follows)
- [x] No Pseudo random generator is used in the analysis described here.

### Memory, Runtime, Storage Requirements


#### Summary time to reproduce

Approximate time needed to reproduce the analyses on a standard (2026) desktop machine:

- [x] <10 minutes
- [ ] 10-60 minutes
- [ ] 1-2 hours
- [ ] 2-8 hours
- [ ] 8-24 hours
- [ ] 1-3 days
- [ ] 3-14 days
- [ ] > 14 days

#### Summary of required storage space

Approximate storage space needed:

- [ ] < 25 MBytes
- [ ] 25 MB - 250 MB
- [ ] 250 MB - 2 GB
- [x] 2 GB - 25 GB
- [ ] 25 GB - 250 GB
- [ ] > 250 GB

- [ ] Not feasible to run on a desktop machine, as described below.

#### Computational Details

The code was last run on an **8-core Apple M3 MacBook Air with 8 GB of memory and approximately 16 GiB of available storage space**. The project directory required **approximately 9.23 GB of storage space**. Last run took **approximately 6.5 minutes**.

## Description of programs/code

- Programs inside `code/R/data_cleaning` will modify and merge different datasets, as needed for consequent programs. `code/R/master.R` will run them all.
- Programs inside `code/R/main` will create maps and run analisys. `code/R/master.R` will run them all.

### License for Code

The code is licensed under a BSD and Creative Common 4.0 licenses. See LICENSE.txt file for details.

## Instructions to Replicators

- Download the replication package and unzip it. No further actions on the folder structure is required at the moment. 
- Edit `code/R/master.R` to adjust the default path. You just need to edit line ..., changing the `main` directory with your master folder path.
Your master path is the path assigned in your machine to the root of the downloaded project.
- PLEASE NOTE: no more actions are required IF your folder has the following subfolders structure:

`master_folder/ `
`- code/`
` -- R/`
` ---- data_cleaning/`
` ---- main/`
` ---- master.R`
`- data`
` -- processed/`
` ---- csv/`
` ---- excel/`
` ---- shapefiles/`
` -- raw/`
` ---- coded/`
` -------- excel/`
` ---- csv/`
` ---- shapefiles/`
` -------- gadm41_HND_shp/`
`- output`
` -- maps/`

The downloaded folder should already have this structure. Check it before running the code, and edit if something differs. 
- Data is provided already in the needed folders, with the exceptions cited above. Data obtained from outside the replication package need to be stored in the prepared subdirectories of `data/`, depending on the format that you download them in. Unzip them first, if necessary.
- Run `code/R/master.R` to run all steps in sequence. Run code in order. 

## List of tables and programs

The provided code reproduces:

- [x] All numbers provided in text in the paper
- [x] All tables and figures in the paper
- [ ] Selected tables and figures in the paper, as explained and justified below.


| Figure/Table #    | Program                  | Line Number | Output file                      | Note                            |
|-------------------|--------------------------|-------------|----------------------------------|---------------------------------|
| Map 1             | 0_treatment_maps.R.      |    138      | treat_map_1.png.                 ||
| Map 2             | 0_treatment_maps.R.      |    139      | treat_map_12.png.                ||
| Map 3             | 0_treatment_maps.R.      |    140      | treat_map_123.png.               ||
| Map 4             | 0_treatment_maps.R.      |    141      | treat_map_1234.png.              ||
| Map 5             | 0_treatment_maps.R.      |    142      | treat_map_1234.png.              ||
| Map 6             | 0_treatment_maps.R.      |    143      | treat_map_total.png.             ||

## References

Secretaría de Educación de Honduras (SEDUC). 2026. “School Enrollment Data: Report_enrollment_students.csv [dataset].” Tegucigalpa, Honduras: Secretaría de Educación de Honduras. Data requested through SIELHO, https://sielho.iaip.gob.hn/inicio/

GADM. 2026. “Administrative Borders of Honduras, Level 2: gadm41_HND_2.shp [dataset].” GADM Maps and Data. https://gadm.org/download_country.html

Government of Honduras. 2026. “Official Government Decrees on Treatment Dates [PDF documents].” Tegucigalpa, Honduras: Portal Único de Transparencia, Instituto de Acceso a la Información Pública. https://portalunico.iaip.gob.hn/433/24/

---

