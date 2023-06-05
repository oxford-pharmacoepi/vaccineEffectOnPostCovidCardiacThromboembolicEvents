postAcuteCovid19Id <- cohortSet(cdm[[symptomsCohortName]]) %>%
  filter(cohort_name == "post_acute_covid19") %>%
  pull("cohort_definition_id")
counts <- cdm[[symptomsCohortName]] %>%
  filter(cohort_definition_id == postAcuteCovid19Id) %>%
  tally() %>%
  pull() %>%
  as.numeric()
if (counts > 5) {
  lscPostAcuteCovid19 <- getLargeScaleCharacteristics(
    cdm = cdm,
    targetCohortName = symptomsCohortName,
    targetCohortId = postAcuteCovid19Id,
    temporalWindows = list(
      c(NA, -366), c(-365, -31), c(-30, -1), c(0, 0), c(1, 30), c(31, 365), 
      c(366, NA)
    ),
    tablesToCharacterize = c(
      "condition_occurrence", "drug_exposure", "observation",
      "procedure_occurrence", "measurement"
    ),
    overlap = FALSE
  )
  write_csv(
    lscPostAcuteCovid19 %>% mutate(cdm_name = cdmName(cdm)),
    here(results, paste0("lsc_post_acute_covid19_", cdmName(cdm), ".csv"))
  )
}
