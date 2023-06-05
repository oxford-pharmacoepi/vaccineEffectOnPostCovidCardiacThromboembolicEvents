# Functions ----
attritionPACS <- function(x, attrition, reason) {
  newLine <- x %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::mutate(obs = paste0(.data$subject_id, "_", .data$cohort_start_date)) %>%
    dplyr::summarise(
      number_records = as.numeric(dplyr::n_distinct(.data$obs)),
      number_subjects = as.numeric(dplyr::n_distinct(.data$subject_id)),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(reason = .env$reason)
  if (is.null(attrition)) {
    attrition <- newLine %>%
      dplyr::mutate(
        reason_id = 1,
        excluded_records = 0,
        excluded_subjects = 0,
        step = as.character(NA)
      )
  } else {
    prevId <- max(attrition$reason_id)
    attrition <- attrition %>%
      dplyr::union_all(
        newLine %>%
          mutate(
            reason_id = .env$prevId + 1,
            excluded_records = 
              .env$attrition$number_records[prevId] - .data$number_records,
            excluded_subjects = 
              .env$attrition$number_subjects[prevId] - .data$number_subjects,
            step = as.character(NA)
          )
      )
  }
  return(attrition)
}

generateDenominatorPopulationPACS <- function(cdm,
                                              cohortName,
                                              targetCohortId) {
  target <- cdm[[cohortName]] %>%
    dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::compute()
  
  attrition <- attritionPACS(target, NULL, "Initial qualifying records")
  
  # No prior target cohort in the previous 42 days
  target <- target %>%
    dplyr::group_by(.data$subject_id) %>%
    dbplyr::window_order(.data$cohort_start_date) %>%
    dplyr::mutate(target_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::compute()
  target <- target %>%
    dplyr::left_join(
      target %>%
        dplyr::mutate(target_id = .data$target_id + 1) %>%
        dplyr::select(
          "subject_id",
          "prev_target" = "cohort_start_date",
          "target_id"
        ),
      by = c("subject_id", "target_id")
    ) %>%
    dplyr::filter(
      is.na(.data$prev_target) |
        .data$cohort_start_date - .data$prev_target > 42
    ) %>%
    dplyr::select(-"target_id", -"prev_target") %>%
    dplyr::compute()
  
  attrition <- attritionPACS(target, attrition, "No prior target in the previous 42 days")
  
  # 180 days of prior observation
  target <- target %>%
    dplyr::inner_join(
      cdm[["observation_period"]] %>%
        dplyr::select(
          "subject_id" = "person_id", "observation_period_start_date"
        ),
      by = "subject_id"
    ) %>%
    dplyr::mutate(
      prior_history =
        .data$cohort_start_date - .data$observation_period_start_date
    ) %>%
    dplyr::filter(.data$prior_history >= 180) %>%
    dplyr::select(-"observation_period_start_date", -"prior_history") %>%
    dplyr::compute()
  
  attrition <- attritionPACS(target, attrition, "180 days of prior history")
  
  attr(target, "attrition") <- attrition %>%
    dplyr::mutate(step = "Generate denominator PACScohort")
  attr(target, "CohortSet") <- dplyr::tibble(
    cohort_definition_id = 1,
    cohort_name = "Denominator PACS cohort"
  )
  
  return(target)
}

# Parameters ----
covid_id     <- cohortSet(cdm[[covidCohortName]]) %>% 
  filter(cohort_name == "covid19") %>% 
  pull("cohort_definition_id")
any_vaccine_id <- cohortSet(cdm[[vaccinatedCohortName]]) %>%
  filter(cohort_name == "any_vaccine") %>%
  pull("cohort_definition_id")

# Vaccination period of interest
priorOUT <- 180 # wash out window for outcome event prior to covid

# Time windows between covid-19 and disease to classify pacs:
# First month after infection
date11 <- 0
date12 <- 30
# 31 to 90 after infection
date21 <- 31
date22 <- 90
# 91 to 180 after infection
date31 <- 91
date32 <- 180
# 181 and up to a year after infection
date41 <- 181
date42 <- 365

# COHORTS DB
target <- generateDenominatorPopulationPACS(cdm, covidCohortName, covid_id)

attrition0 <- attr(target, "attrition")

# Vaccine AZ and PF
vaccine_db <- cdm[[vaccinatedCohortName]] %>%
  dplyr::filter(cohort_definition_id == any_vaccine_id) %>%
  select(subject_id, vaccine_date = cohort_start_date) %>%
  compute()

consequencesCohortSet <- cohortSet(cdm[[consequencesCohortName]]) %>%
  collect()

outcome <- consequencesCohortSet$cohort_name
censoring <- c("leave", "covid+leave", "vax+leave", "covid+vax+leave")
window <- c("0-30", "31-90", "91-180", "181-365")
pacsCohortSet <- expand_grid(outcome, censoring, window) %>%
  mutate(cohort_name = paste(outcome, censoring, window, sep = "_")) %>%
  mutate(cohort_definition_id = row_number()) %>%
  select("cohort_definition_id", "cohort_name", "outcome", "censoring", "window")
outcomeId <- consequencesCohortSet$cohort_definition_id

# PACS cohorts table creation ----
for (j in outcomeId) {

  # Disease db
  outcome_db <- cdm[[consequencesCohortName]] %>%
    filter(cohort_definition_id == j) %>%
    select(subject_id, outcome_date = cohort_start_date) %>%
    compute()
  
  # COVIDS to exclude:  have a complication event 180 days before
  covidToExclude <- target %>%
    left_join(outcome_db, by = "subject_id") %>%
    filter(cohort_start_date > outcome_date & cohort_start_date < outcome_date + days(priorOUT)) %>%
    compute()
  
  target_excluded <- target %>%
    anti_join(covidToExclude, by = c("subject_id", "cohort_start_date")) %>%
    compute()
  
  attrition.j <- attritionPACS(target_excluded, attrition0, "No outcome prior 180 days") %>%
    mutate(cohort_definition_id = 0)

  # PACS db: covid + disease
  target_outcome <- target_excluded %>%
    inner_join(outcome_db, by = "subject_id") %>%
    filter(cohort_start_date <= outcome_date & cohort_start_date + days(date42) >= outcome_date) %>%
    compute()
  
  # ---------------------------- NO CENSORING ----------------------------- #
  outcome_1 <- target_outcome %>%
    mutate(distancePACS = outcome_date - cohort_start_date) %>%
    mutate(window = case_when(
      distancePACS >= date11 & distancePACS <= date12 ~ "0-30",
      distancePACS >= date21 & distancePACS <= date22 ~ "31-90",
      distancePACS >= date31 & distancePACS <= date32 ~ "91-180",
      distancePACS >= date41 & distancePACS <= date42 ~ "181-365",
      TRUE ~ NA
    )) %>%
    select(-"distancePACS", -"cohort_end_date") %>%
    rename("cohort_end_date" = "outcome_date") %>%
    distinct() %>%
    mutate(censoring = "leave") %>%
    compute()

  # --------------------------- COVID CENSORING --------------------------- #
  outcome_2 <- target_outcome %>%
    addEvent(cdm, covidCohortName, covid_id, c(43, NA), name = "next_covid") %>%
    filter(is.na(next_covid) || outcome_date <= next_covid) %>%
    select(-"next_covid") %>%
    mutate(distancePACS = outcome_date - cohort_start_date) %>%
    mutate(window = case_when(
      distancePACS >= date11 & distancePACS <= date12 ~ "0-30",
      distancePACS >= date21 & distancePACS <= date22 ~ "31-90",
      distancePACS >= date31 & distancePACS <= date32 ~ "91-180",
      distancePACS >= date41 & distancePACS <= date42 ~ "181-365",
      TRUE ~ NA
    )) %>%
    select(-"distancePACS", -"cohort_end_date") %>%
    rename("cohort_end_date" = "outcome_date") %>%
    distinct() %>%
    mutate(censoring = "covid+leave") %>%
    compute()

  # -------------------------- VACCINE CENSORING -------------------------- #
  outcome_3 <- target_outcome %>%
    addEvent(cdm, vaccinatedCohortName, any_vaccine_id, c(0, NA), name = "next_vax") %>%
    filter(is.na(next_vax) || outcome_date <= next_vax) %>%
    select(-"next_vax") %>%
    mutate(distancePACS = outcome_date - cohort_start_date) %>%
    mutate(window = case_when(
      distancePACS >= date11 & distancePACS <= date12 ~ "0-30",
      distancePACS >= date21 & distancePACS <= date22 ~ "31-90",
      distancePACS >= date31 & distancePACS <= date32 ~ "91-180",
      distancePACS >= date41 & distancePACS <= date42 ~ "181-365",
      TRUE ~ NA
    )) %>%
    select(-"distancePACS", -"cohort_end_date") %>%
    rename("cohort_end_date" = "outcome_date") %>%
    distinct() %>%
    mutate(censoring = "vax+leave") %>%
    compute()
  
  # ---------------------- COVID + VACCINE CENSORING ---------------------- #
  outcome_4 <- target_outcome %>%
    addEvent(cdm, covidCohortName, covid_id, c(43, NA), name = "next_covid") %>%
    filter(is.na(next_covid) || outcome_date <= next_covid) %>%
    select(-"next_covid") %>%
    addEvent(cdm, vaccinatedCohortName, any_vaccine_id, c(0, NA), name = "next_vax") %>%
    filter(is.na(next_vax) || outcome_date <= next_vax) %>%
    select(-"next_vax") %>%
    mutate(distancePACS = outcome_date - cohort_start_date) %>%
    mutate(window = case_when(
      distancePACS >= date11 & distancePACS <= date12 ~ "0-30",
      distancePACS >= date21 & distancePACS <= date22 ~ "31-90",
      distancePACS >= date31 & distancePACS <= date32 ~ "91-180",
      distancePACS >= date41 & distancePACS <= date42 ~ "181-365",
      TRUE ~ NA
    )) %>%
    select(-"distancePACS", -"cohort_end_date") %>%
    rename("cohort_end_date" = "outcome_date") %>%
    distinct() %>%
    mutate(censoring = "covid+vax+leave") %>%
    compute()
  
  # join all together
  pacsCohort.j <- outcome_1 %>%
    union_all(outcome_2) %>%
    union_all(outcome_3) %>%
    union_all(outcome_4) %>%
    select(-"cohort_definition_id") %>%
    left_join(
      pacsCohortSet %>%
        filter(outcome == .env$outcome[outcomeId == j]) %>%
        select("cohort_definition_id", "window", "censoring"),
      by = c("window", "censoring"),
      copy = TRUE
    ) %>%
    select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") %>%
    compute()
  
  attrition.j <- attrition.j %>%
    inner_join(
      tibble(
        cohort_definition_id_new = pacsCohortSet %>%
          filter(outcome == .env$outcome[outcomeId == j]) %>%
          pull("cohort_definition_id"),
        cohort_definition_id = rep(
          0, 
          pacsCohortSet %>%
            filter(outcome == .env$outcome[outcomeId == j]) %>%
            nrow()
        )
      ),
      by = "cohort_definition_id",
      multiple = "all"
    ) %>%
    select(-"cohort_definition_id") %>%
    rename("cohort_definition_id" = "cohort_definition_id_new") %>%
    relocate("cohort_definition_id") %>%
    union_all(
      pacsCohort.j %>%
        group_by(cohort_definition_id) %>%
        summarise(
          number_records = as.numeric(n()),
          number_subjects = as.numeric(n_distinct(subject_id)),
          .groups = "drop"
        ) %>%
        collect() %>%
        mutate(
          reason = "Outcomes observed in the window",
          reason_id = 5,
          excluded_records = attrition.j$number_records[attrition.j$reason_id == 4] - number_records,
          excluded_subjects = attrition.j$number_subjects[attrition.j$reason_id == 4] - number_subjects,
          step = as.character(NA)
        )
    ) %>%
    arrange(cohort_definition_id, reason_id) %>%
    mutate(step = if_else(is.na(step), "Outcome specific attrition", step))
  
  if (j == 1) {
    pacsCohort <- pacsCohort.j
    attrition <- attrition.j
  } else {
    pacsCohort <- pacsCohort %>% union_all(pacsCohort.j)
    attrition <- attrition %>% union_all(attrition.j)
  }
}

cdm[[pacsCohortName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(pacsCohort, pacsCohortName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = insertTable(pacsCohortSet, cdm, paste0(pacsCohortName, "_set")),
  cohortAttritionRef = insertTable(attrition, cdm, paste0(pacsCohortName, "_attrition")),
  cohortCountRef = insertTable(getCohortCount(pacsCohort), cdm, paste0(pacsCohortName, "_count"))
)
