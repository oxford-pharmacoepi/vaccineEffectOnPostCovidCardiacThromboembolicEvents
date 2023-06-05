indexCohortSet <- cohortSet(cdm[[indexCohortName]]) %>%
  collect()
comparisons <- tibble(
  exposure_cohort_id = c(1:5, rep(3,3), 11:15, rep(13,3), 21:25, rep(23,3), 31:35, rep(33,3)),
  comparator_cohort_id = c(6:10, c(2,4,5), 16:20, c(2,4,5)+10, 26:30, c(2,4,5)+20, 36:40, c(2,4,5)+30)
) %>%
  inner_join(
    indexCohortSet %>% 
      select(
        "exposure_cohort_id" = "cohort_definition_id", 
        "exposure_name" = "cohort_name"
      ),
    by = "exposure_cohort_id"
  ) %>%
  inner_join(
    indexCohortSet %>% 
      select(
        "comparator_cohort_id" = "cohort_definition_id", 
        "comparator_name" = "cohort_name"
      ),
    by = "comparator_cohort_id"
  ) %>%
  mutate(study = as.numeric(substr(exposure_name, 7, 7))) %>%
  mutate(comparator_name = substr(comparator_name, 10, nchar(comparator_name))) %>%
  mutate(exposure_name = substr(exposure_name, 10, nchar(exposure_name))) %>%
  mutate(comparison_id = row_number()) %>%
  mutate(comparison_name = paste0("Study_", study, "_", gsub(" ", "_", exposure_name), "_vs_", gsub(" ", "_", comparator_name))) %>%
  select(comparison_id, comparison_name, study, exposure_cohort_id, exposure_name, comparator_cohort_id, comparator_name)
