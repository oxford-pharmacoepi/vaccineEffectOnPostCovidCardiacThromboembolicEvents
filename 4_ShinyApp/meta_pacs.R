library(dplyr)
library(tidyr)
library(EmpiricalCalibration)
library(readr)
library(survival)
library(ggplot2)
library(ggfortify)
library(here)
library(meta)

# functions ----
readFiles <- function(x) {
  dataFiles <- list.files(here("data"), full.names = TRUE)
  dataFiles <- dataFiles[grepl(x, dataFiles)]
  if (x == "lsc_post_acute_covid19") {
    colType <- cols(
      cohort_definition_id = col_integer(),
      cohort_name = col_character(),
      table_name = col_character(),
      window_name = col_character(),
      concept_id = col_integer(),
      concept_name = col_character(),
      concept_count = col_character(),
      denominator_count = col_character(),
      overlap = col_logical(),
      concept_type = col_character(),
      cdm_name = col_character()
    )
  } else if (x == "cdm_snapshot") {
    colType <- cols(
      cdm_source_name = col_character(),
      cdm_version = col_character(),
      cdm_holder = col_character(),
      cdm_release_date = col_date(),
      vocabulary_version = col_character(),
      person_cnt = col_integer(),
      observation_period_cnt = col_integer(),
      cdm_schema = col_character(),
      write_schema = col_character(),
      cdm_name = col_character()
    )
  } else {
    colType <- NULL
  }
  result <- lapply(dataFiles, function(f){read_csv(f, show_col_types = FALSE, col_types = colType)}) %>%
    bind_rows()
  return(result)
}
renameComparisonName <- function(x, new_name) {
  x %>%
    inner_join(new_name, by = "comparison_name") %>%
    select(-"comparison_name") %>%
    rename("comparison_name" = "comparison_new_name")
}

# prepare data ----
cohort_details <- readFiles("cohort_details")
comparisons <- readFiles("comparison")
nco <- readFiles("negative_control_outcomes")
estimates <- readFiles("outcome_estimates") %>% filter(model == "finegray")

comparison_new_name <- comparisons %>%
  select(comparison_name) %>%
  distinct() %>%
  mutate(comparison_new_name = gsub("tudy_", "", comparison_name)) %>%
  mutate(comparison_new_name = gsub("_", " ", comparison_new_name)) %>%
  mutate(comparison_new_name = gsub(" unvaccinated", "", comparison_new_name)) %>%
  mutate(comparison_new_name = gsub(" vaccinated", "", comparison_new_name)) %>%
  mutate(comparison_new_name = gsub(" vaccine", "", comparison_new_name)) %>%
  mutate(comparison_new_name = gsub(" vs ", " ", comparison_new_name)) %>%
  separate(comparison_new_name, c("study", "vax1", "vax2")) %>%
  mutate(vax2 = if_else(vax1 == vax2, "(VE)", paste0("- ", vax2, " (CVE)"))) %>%
  mutate(comparison_new_name = paste(study, vax1, vax2)) %>%
  select("comparison_name", "comparison_new_name")
comparisons <- renameComparisonName(comparisons, comparison_new_name)
comparisons <- comparisons %>%
  filter(cdm_name != "SIDIAP" | grepl("\\(VE\\)", .data$comparison_name)) %>%
  filter(skip == 0)

estimates <- estimates %>%
  inner_join(comparisons, by = c("comparison_id", "cdm_name"))

outcomes <- estimates %>% filter(grepl("pacs", .data$outcome_group)) %>% distinct(outcome_name) %>% pull()

modelMeta <- "random"
estimates_plot <- estimates %>%
  mutate(comparison = substr(comparison_name, 4, nchar(comparison_name))) %>%
  filter(variable == "groupexposure") %>%
  filter(skip == 0) %>%
  filter(outcome_name %in% outcomes) %>%
  mutate(study = paste0("Study ", study)) %>%
  select(
    "cdm_name", "comparison", "adjustment", "censoring_method", "outcome_name", 
    "study", "coef", "se_coef", "hr", "lower_hr", "upper_hr"
  ) %>%
  mutate(i2 = NA)

cdmName <- unique(estimates_plot$cdm_name)
comp <- estimates_plot %>% 
  distinct(comparison) %>% 
  filter(!(grepl("(CVE)", .data$comparison) & 
             !grepl("astrazeneca", .data$comparison))) %>%
  filter(!grepl("moderna", .data$comparison)) %>%
  filter(!grepl("janssen", .data$comparison)) %>%
  pull()
cens  <- unique(estimates_plot$censoring_method)
ajust <- unique(estimates_plot$adjustment)
study <- unique(estimates_plot$study)

estimates_metaanalysis <- NULL

# Metaanalysis across studies ----
for (cdmNameK in cdmName) {
  for (compK in comp) {
    for (censK in cens) {
      for (ajustK in ajust) {
        for (outcome in outcomes) {
          x <- estimates_plot %>%
            filter(
              cdm_name == cdmNameK & comparison == compK & 
                censoring_method == censK & adjustment == ajustK &
                outcome_name == outcome
            ) %>%
            filter(!is.na(hr)) %>%
            filter(!is.na(se_coef))
          
          if (nrow(x) > 1) {
            xMA <- metagen(TE = x$coef, seTE = x$se_coef, sm = "HR")
            
              resultMeta <- tibble(
                cdm_name = cdmNameK, comparison = compK, adjustment = ajustK,
                censoring_method = censK, outcome_name = outcome,
                study = "Meta Analysis", 
                coef = eval(parse(text = paste0("xMA$TE.", modelMeta))),
                se_coef = eval(parse(text = paste0("xMA$seTE.", modelMeta))),
                hr = exp(eval(parse(text = paste0("xMA$TE.", modelMeta)))),
                lower_hr = exp(eval(parse(text = paste0("xMA$lower.", modelMeta)))),
                upper_hr = exp(eval(parse(text = paste0("xMA$upper.", modelMeta)))),
                i2 = xMA$I2
              )
            estimates_metaanalysis <- bind_rows(estimates_metaanalysis, resultMeta)
          }
        }
      }
    }
  }
}


# Metaanalysis across database & studies
# comp <- c("astrazeneca (VE)", "pfizer (VE)")
# cens <- "leave"
# ajust <- "calibrated"
for (compK in comp) {
  for (censK in cens) {
    for (ajustK in ajust) {
      for (outcome in outcomes) {
        x <- estimates_plot %>%
          filter(
            comparison == compK & 
              censoring_method == censK & adjustment == ajustK &
              outcome_name == outcome
          ) %>%
          filter(!is.na(hr)) %>%
          filter(!is.na(se_coef))
        
        if (compK == "pfizer - astrazeneca (CVE)") {
          x <- x %>%
            filter(cdm_name != "CORIVA") %>%
            filter(cdm_name != "SIDIAP") 
        }
        if (nrow(x) > 1) {
          tryCatch(
            {
              xMA <- metagen(TE = x$coef, seTE = x$se_coef, sm = "HR")
              resultMeta <- tibble(
                cdm_name = "Meta Analysis", comparison = compK, adjustment = ajustK,
                censoring_method = censK, outcome_name = outcome,
                study = "Meta Analysis", 
                coef = eval(parse(text = paste0("xMA$TE.", modelMeta))),
                se_coef = eval(parse(text = paste0("xMA$seTE.", modelMeta))),
                hr = exp(eval(parse(text = paste0("xMA$TE.", modelMeta)))),
                lower_hr = exp(eval(parse(text = paste0("xMA$lower.", modelMeta)))),
                upper_hr = exp(eval(parse(text = paste0("xMA$upper.", modelMeta)))),
                i2 = xMA$I2
              )
              estimates_metaanalysis <- bind_rows(estimates_metaanalysis, resultMeta)
            },
            error = function(e) {
            }
            )
        }
      }
    }
  }
}

# Metaanalysis across databases and cohort 1----
comp <- c("astrazeneca (VE)", "pfizer (VE)")
cens <- "leave"
ajust <- "calibrated"
for (compK in comp) {
  for (censK in cens) {
    for (ajustK in ajust) {
      for (outcome in outcomes) {
        x <- estimates_plot %>%
          filter(
            comparison == compK & 
              censoring_method == censK & 
              adjustment == ajustK &
              outcome_name == outcome &
              study == "Study 1"
          ) %>%
          filter(!is.na(hr)) %>%
          filter(!is.na(se_coef))
        
        if (nrow(x) > 1) {
          xMA <- metagen(TE = x$coef, seTE = x$se_coef, sm = "HR")
          
          resultMeta <- tibble(
            comparison = compK, adjustment = ajustK,
            censoring_method = censK, outcome_name = outcome,
            cdm_name = "Meta Analysis", study = "Study 1",
            coef = eval(parse(text = paste0("xMA$TE.", modelMeta))),
            se_coef = eval(parse(text = paste0("xMA$seTE.", modelMeta))),
            hr = exp(eval(parse(text = paste0("xMA$TE.", modelMeta)))),
            lower_hr = exp(eval(parse(text = paste0("xMA$lower.", modelMeta)))),
            upper_hr = exp(eval(parse(text = paste0("xMA$upper.", modelMeta)))),
            i2 = xMA$I2
          )
          estimates_metaanalysis <- bind_rows(estimates_metaanalysis, resultMeta)
        }
      }
    }
  }
}

estimates_plot <- estimates_plot %>%
  union_all(estimates_metaanalysis) %>%
  mutate(study = gsub("Study", "Cohort", .data$study))


write_csv(estimates_plot, here("estimates_pacs.csv"))
