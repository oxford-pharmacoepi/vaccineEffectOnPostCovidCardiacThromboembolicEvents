# instantiate COVID cohorts
info(logger, "INSTANTIATE COVID COHORTS")
covidCohortSet <- readCohortSet(here("1_InstantiateCohorts", "covid"))
cdm <- generateCohortSet(cdm, covidCohortSet, covidCohortName, TRUE, TRUE)

# instantiate VACCINATED cohorts
info(logger, "INSTANTIATE VACCINE COHORTS")
if (cdmName(cdm) == "GOLD") {
  source(here("1_InstantiateCohorts", "instantiate_vaccinated_GOLD.R"))
} else {
  vaccinatedCohortSet <- readCohortSet(here("1_InstantiateCohorts", "vaccinated"))
  cdm <- generateCohortSet(cdm, vaccinatedCohortSet, vaccinatedCohortName, TRUE, TRUE)
}

# instantiate CONSEQUENCES cohorts
info(logger, "INSTANTIATE CONSEQUENCES COHORTS")
consequencesCohortSet <- readCohortSet(here("1_InstantiateCohorts", "consequences"))
cdm <- generateCohortSet(cdm, consequencesCohortSet, consequencesCohortName, TRUE, TRUE)

# instantiate GENERAL CONDITIONS cohorts
info(logger, "INSTANTIATE GENERAL CONDITIONS COHORTS")
generalConditionsCohortSet <- readCohortSet(here("1_InstantiateCohorts", "general_conditions"))
cdm <- generateCohortSet(cdm, generalConditionsCohortSet, generalConditionsCohortName, TRUE, TRUE)

# instantiate DATABASE SPECIFIC cohorts
info(logger, "INSTANTIATE DATABASE SPECIFIC COHORTS")
databaseSpecificCohortSet <- readCohortSet(here("1_InstantiateCohorts", "database_specific", cdmName(cdm)))
cdm <- generateCohortSet(cdm, databaseSpecificCohortSet, databaseSpecificCohortName, TRUE, TRUE)
