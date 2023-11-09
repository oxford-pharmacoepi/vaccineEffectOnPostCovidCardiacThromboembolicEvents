renv::activate()
renv::restore()

library(SqlRender)
library(DBI)
library(here)
library(zip)
library(dbplyr)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(glue)
library(CDMConnector)
library(glmnet)
library(log4r)
library(CirceR)
library(purrr)
library(survival)
library(EmpiricalCalibration)
library(spatstat)
library(Hmisc)

databaseName <- "SIDIAP" # "GOLD", "AURUM", "SIDIAP", "UiO", or "CORIVA"

# Connection details
server_dbi <- Sys.getenv("DB_SERVER_DBI_22t2")
user <- Sys.getenv("DB_USER_longcovid")
password <- Sys.getenv("DB_PASSWORD_longcovid")
port <- Sys.getenv("DB_PORT")
host <- Sys.getenv("DB_HOST")

db <- dbConnect(
  RPostgres::Postgres(),
  dbname = server_dbi,
  port = port,
  host = host,
  user = user,
  password = password
)

cdmDatabaseSchema <- "omop22t2_cmbd"
resultsDatabaseSchema <- "results22t2_cmbd"

cdm <- cdmFromCon(
  con = db, 
  cdmSchema = cdmDatabaseSchema, 
  writeSchema = resultsDatabaseSchema,
  cdmName = databaseName
)

# Count number of individuals in database to see if we connected correctly
cdm$person %>% 
  tally() %>%
  compute()

# cohort stem where cohorts will be instantiated
cohortStem <- "nmb_pacs"

# output folder
results <- paste0("Results_", cdmName(cdm))

# Which steps should be run
instantiateCohorts <- FALSE
runPropensityScores <- TRUE
runOutcomeModel <- TRUE

# Run parallel
runParallel <- FALSE

# run analysis
source("RunStudy.R")

# happy for the long journey
print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the study!")

