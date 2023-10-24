library(here)
library(readr)
library(dplyr)

cases <- read_csv(here("Data", "oldDataCPRD", "region_2022-01-30_infections.csv"), show_col_types = FALSE)
tests <- read_csv(here("Data", "oldDataCPRD", "region_2022-01-30_testing.csv"), show_col_types = FALSE)
vaccines <- read_csv(here("Data", "oldDataCPRD", "region_2022-01-30_vaccination.csv"), show_col_types = FALSE)
pop <- read_csv(here("Data", "oldDataCPRD", "populations.csv"), show_col_types = FALSE)
northern_ireland <- read_csv(here("Data", "oldDataCPRD", "northern_ireland.csv"), show_col_types = FALSE)
scotland <- read_csv(here("Data", "oldDataCPRD", "scotland.csv"), show_col_types = FALSE)

result <- cases %>%
  select("region" = "areaName", "date", "new_cases" = "newCasesBySpecimenDate") %>%
  mutate(region = tolower(region)) %>%
  full_join(
    tests %>%
      select("region" = "areaName", "date", "new_tests" = "newPCRTestsBySpecimenDate") %>%
      mutate(region = tolower(region)),
    by = c("region", "date")
  ) %>%
  full_join(
    vaccines %>%
      select(
        "region" = "areaName", 
        "date", 
        "dose1" = "cumVaccinationFirstDoseUptakeByVaccinationDatePercentage",
        "dose2" = "cumVaccinationSecondDoseUptakeByVaccinationDatePercentage"
      ) %>%
      mutate(region = tolower(region)),
    by = c("region", "date")
  ) %>%
  left_join(
    pop %>% 
      rename("region" = "location_source_value") %>%
      mutate(region = tolower(region)),
    by = "region"
  ) %>%
  mutate(new_cases = if_else(is.na(new_cases), 0, new_cases)) %>%
  mutate(new_tests = if_else(is.na(new_tests), 0, new_tests)) %>%
  mutate(dose2 = if_else(is.na(dose2), 0, dose2)) %>%
  group_by(region) %>%
  arrange(date) %>%
  mutate(cum_cases = cumsum(new_cases)) %>%
  mutate(cum_tests = cumsum(new_tests)) %>%
  mutate(new_cases_7d = cum_cases - lag(cum_cases, 7, 0)) %>%
  mutate(new_tests_7d = cum_tests - lag(cum_tests, 7, 0)) %>%
  ungroup() %>%
  mutate(new_cases_7d_100k = new_cases_7d / pop * 100000) %>%
  mutate(new_tests_7d_100k = new_tests_7d / pop * 100000) %>%
  mutate(dose1_fraction = dose1 / 100) %>%
  mutate(dose2_fraction = dose2 / 100) %>%
  filter(date >= as.Date("2021-01-04")) %>%
  filter(date <= as.Date("2021-07-31")) %>%
  select("region", "date", "new_cases_7d_100k", "new_tests_7d_100k", "dose1_fraction", "dose2_fraction")

write_csv(result, here("Data", "data_AURUM.csv"))

result <- cases %>%
  select("region" = "areaName", "date", "new_cases" = "newCasesBySpecimenDate") %>%
  mutate(region = tolower(region)) %>%
  full_join(
    tests %>%
      select("region" = "areaName", "date", "new_tests" = "newPCRTestsBySpecimenDate") %>%
      mutate(region = tolower(region)),
    by = c("region", "date")
  ) %>%
  full_join(
    vaccines %>%
      select(
        "region" = "areaName", 
        "date", 
        "dose1" = "cumVaccinationFirstDoseUptakeByVaccinationDatePercentage",
        "dose2" = "cumVaccinationSecondDoseUptakeByVaccinationDatePercentage"
      ) %>%
      mutate(region = tolower(region)),
    by = c("region", "date")
  ) %>%
  union_all(
    scotland %>%
      select(
        "region" = "areaName", 
        "date", 
        "new_cases" = "cases",
        "new_tests" = "tests",
        "dose1" = "first_dose",
        "dose2" = "second_dose"
      ) %>%
      mutate(date = as.Date(date, "%d/%m/%Y")) %>%
      mutate(region = tolower(region))
  ) %>%
  union_all(
    northern_ireland %>%
      select(
        "region" = "areaName", 
        "date", 
        "new_cases" = "cases",
        "new_tests" = "tests",
        "dose1" = "cumVaccinationFirstDoseUptakeByPublishDatePercentage",
        "dose2" = "cumVaccinationSecondDoseUptakeByPublishDatePercentage"
      ) %>%
      mutate(date = as.Date(date, "%d/%m/%Y")) %>%
      mutate(region = tolower(region))
  ) %>%
  left_join(
    pop %>% 
      rename("region" = "location_source_value") %>%
      mutate(region = tolower(region)),
    by = "region"
  ) %>%
  mutate(new_cases = if_else(is.na(new_cases), 0, new_cases)) %>%
  mutate(new_tests = if_else(is.na(new_tests), 0, new_tests)) %>%
  mutate(dose2 = if_else(is.na(dose2), 0, dose2)) %>%
  group_by(region) %>%
  arrange(date) %>%
  mutate(cum_cases = cumsum(new_cases)) %>%
  mutate(cum_tests = cumsum(new_tests)) %>%
  mutate(new_cases_7d = cum_cases - lag(cum_cases, 7, 0)) %>%
  mutate(new_tests_7d = cum_tests - lag(cum_tests, 7, 0)) %>%
  ungroup() %>%
  mutate(new_cases_7d_100k = new_cases_7d / pop * 100000) %>%
  mutate(new_tests_7d_100k = new_tests_7d / pop * 100000) %>%
  mutate(dose1_fraction = dose1 / 100) %>%
  mutate(dose2_fraction = dose2 / 100) %>%
  filter(date >= as.Date("2021-01-04")) %>%
  filter(date <= as.Date("2021-07-31")) %>%
  select("region", "date", "new_cases_7d_100k", "new_tests_7d_100k", "dose1_fraction", "dose2_fraction")

write_csv(result, here("Data", "data_GOLD.csv"))
