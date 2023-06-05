longcovid28CohortSet <- cohortSet(cdm[[longcovidCohortName]]) %>%
  filter(window == 28) %>%
  collect()
longcovid28Ids <- longcovid28CohortSet$cohort_definition_id
longcovid28Name <- longcovid28CohortSet$symptom_name
target28 <- cdm[[longcovidCohortName]] %>%
  filter(cohort_definition_id %in% longcovid28Ids) %>%
  select(-"cohort_end_date") %>%
  compute()
overlap28 <- target28 %>%
  filter(cohort_definition_id == !!longcovid28Ids[1]) %>%
  select(-"cohort_definition_id") %>%
  mutate(!!longcovid28Name[1] := 1) %>% 
  compute()
for (k in 2:length(longcovid28Ids)) {
  overlap28 <- overlap28 %>%
    full_join(
      target28 %>%
        filter(cohort_definition_id == !!longcovid28Ids[k]) %>%
        select(-"cohort_definition_id") %>%
        mutate(!!longcovid28Name[k] := 1),
      by = c("subject_id", "cohort_start_date")
    ) %>%
    compute()
}
overlap28 <- overlap28 %>%
  mutate(across(all_of(longcovid28Name), ~ if_else(is.na(.x), 0, .x))) %>%
  collect()
result28 <- NULL
for (den_id in longcovid28Name) {
  overlap28d <- overlap28 %>%
    filter(.data[[den_id]] == 1)
  for (num_id in longcovid28Name) {
    result28 <- result28 %>% 
      union_all(
        tibble(
          numerator = num_id,
          denominator = den_id,
          freq = mean(overlap28d[[num_id]])
        )
      )
  }
}

longcovid90CohortSet <- cohortSet(cdm[[longcovidCohortName]]) %>%
  filter(window == 90) %>%
  collect()
longcovid90Ids <- longcovid90CohortSet$cohort_definition_id
longcovid90Name <- longcovid90CohortSet$symptom_name
target90 <- cdm[[longcovidCohortName]] %>%
  filter(cohort_definition_id %in% longcovid90Ids) %>%
  select(-"cohort_end_date") %>%
  compute()
overlap90 <- target90 %>%
  filter(cohort_definition_id == !!longcovid90Ids[1]) %>%
  select(-"cohort_definition_id") %>%
  mutate(!!longcovid90Name[1] := 1) %>% 
  compute()
for (k in 2:length(longcovid90Ids)) {
  overlap90 <- overlap90 %>%
    full_join(
      target90 %>%
        filter(cohort_definition_id == !!longcovid90Ids[k]) %>%
        select(-"cohort_definition_id") %>%
        mutate(!!longcovid90Name[k] := 1),
      by = c("subject_id", "cohort_start_date")
    ) %>%
    compute()
}
overlap90 <- overlap90 %>%
  mutate(across(all_of(longcovid28Name), ~ if_else(is.na(.x), 0, .x))) %>%
  collect()
result90 <- NULL
for (den_id in longcovid90Name) {
  overlap90d <- overlap90 %>%
    filter(.data[[den_id]] == 1)
  for (num_id in longcovid90Name) {
    result90 <- result90 %>% 
      union_all(
        tibble(
          numerator = num_id,
          denominator = den_id,
          freq = mean(overlap90d[[num_id]])
        )
      )
  }
}
result <- result28 %>% 
  mutate(window = 28) %>% 
  union_all(result90 %>% mutate(window = 90))
write_csv(
  result %>% mutate(cdm_name = cdmName(cdm)), 
  here(results, paste0("longcovid19_symptoms_overlap_", cdmName(cdm), ".csv"))
)
