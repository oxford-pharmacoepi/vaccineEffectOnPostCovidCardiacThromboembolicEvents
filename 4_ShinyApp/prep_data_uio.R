database_name <- "SIDIAP"

numOutcomes <- readFiles("outcome_estimates") %>%
  filter(cdm_name == database_name) %>%
  filter(grepl("pacs", outcome_group)) %>%
  select(number_events_comparator, number_events_exposure, comparison_id, outcome_name, 
         outcome_group, comparison_id, censoring_method) %>%
  distinct()

comparisons <- readFiles("comparison")
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
  filter(cdm_name == database_name)  %>%
  filter(skip == 0)


estimates <- numOutcomes %>%
  inner_join(comparisons, by = c("comparison_id")) %>%
  mutate(study = paste0("Cohort ", study)) %>%
  mutate(comparison = substr(comparison_name, 4, nchar(comparison_name)))

# Outcome groups
vte <- c("venous_thromboembolism", "deep_vein_thrombosis", "pulmonary_embolism")
ate <- c("arterial_thromboembolism", "ischemic_stroke", "transient_ischemic_attack", "myocardial_infarction")
oth <- c("heart_failure", "haemorrhagic_stroke", "myocarditis_pericarditis", "ventricular_arrhythmia_cardiac_arrest")


estimates <- estimates %>%
  mutate(window = ifelse(grepl("0-30", .data$outcome_name),
                         "0 to 30 days",
                         ifelse(grepl("31-90", .data$outcome_name),
                                "31 to 90 days",
                                ifelse(grepl("91-180", .data$outcome_name),
                                       "91 to 180 days",
                                       "181 to 365 days"))),
         analysis = ifelse(grepl('_vax', .data$outcome_name),
                           "vax_leave",
                           ifelse(grepl('_leave', .data$outcome_name),
                                  "leave",
                                  ifelse(grepl('vax', .data$outcome_name),
                                         "covid_vax_leave",
                                         "covid_leave"))),
         comparison = ifelse(grepl("astrazeneca", .data$comparison),
                             gsub("astrazeneca", "ChAdOx1", .data$comparison),
                             ifelse(grepl("pfizer", .data$comparison),
                                    gsub("pfizer", "BNT162b2", .data$comparison),
                                    ifelse(grepl("any", .data$comparison),
                                           gsub("any", "Any vaccine", .data$comparison),
                                           comparison)))) %>%
  mutate(
    censoring_method = case_when(
      analysis == "leave" & censoring_method == "leave" ~ "main",
      analysis == "vax_leave" & censoring_method == "leave" ~ "sa_pcs",
      analysis == "covid_leave" & censoring_method == "leave" ~ "sa_covid",
      analysis == "leave" & censoring_method == "leave+vaccine" ~ "sa_vaccine"
    )) %>% 
  filter(!is.na(censoring_method)) %>%
  mutate(
    comparison = gsub("pfizer", "BNT162b2", .data$comparison),
    outcome_name_old = outcome_name,
    outcome_name = ifelse(grepl("venous_thrombosembolism", .data$outcome_name),
                          vte[1],
                          ifelse(grepl(vte[2], .data$outcome_name),
                                 vte[2],
                                 ifelse(grepl(vte[3], .data$outcome_name),
                                        vte[3],
                                        ifelse(grepl("arterial_thrombosembolism", .data$outcome_name),
                                               ate[1],
                                               ifelse(grepl(ate[2], .data$outcome_name),
                                                      ate[2],
                                                      ifelse(grepl(ate[3], .data$outcome_name),
                                                             ate[3],
                                                             ifelse(grepl(ate[4], .data$outcome_name),
                                                                    ate[4],
                                                                    ifelse(grepl(oth[1], .data$outcome_name),
                                                                           oth[1],
                                                                           ifelse(grepl(oth[2], .data$outcome_name),
                                                                                  oth[2],
                                                                                  ifelse(grepl(oth[3], .data$outcome_name),
                                                                                         oth[3],
                                                                                         oth[4])))))))))),
    comparison_type = ifelse(grepl("(CVE)", .data$comparison), "CVE", "VE"),
    comparison = gsub(" \\(CVE\\)", "", .data$comparison)) %>%
  mutate(group = ifelse(.data$outcome_name %in% vte,
                        "VTE",
                        ifelse(.data$outcome_name %in% ate,
                               "ATE",
                               "Others")),
         nice_outcome_name = stringr::str_to_sentence(gsub("_", " ", .data$outcome_name)),
         comparison = gsub(" \\(VE\\)", "", .data$comparison)) %>%
  filter(!grepl("moderna", .data$comparison) & !grepl("janssen", .data$comparison)) %>%
  mutate(cdm_name = factor(cdm_name, levels = c("AURUM", "GOLD", "SIDIAP", "CORIVA", "UiO", "Meta Analysis")),
         window = factor(window, levels = c("0 to 30 days", "31 to 90 days", "91 to 180 days", "181 to 365 days"))) %>%
  mutate(meta = if_else(study == "Meta Analysis", "meta", "no meta")) %>%
  mutate(nice_outcome_name = gsub("arrhythmia cardiac", "arrhythmia or cardiac", .data$nice_outcome_name)) %>%
  mutate(nice_outcome_name = gsub("Arterial thromboembolism", "Arterial thrombosis/thromboembolism", .data$nice_outcome_name)) %>%
  mutate(number_events_exposure = nice_per10mil(number_events_exposure, number_exposures, 2)) %>%
  mutate(number_events_comparator = nice_per10mil(number_events_comparator, number_comparators, 2)) %>%
  mutate(outcome_name_short = ifelse(grepl(vte[1], .data$outcome_name),
                                     "VTE",
                                     ifelse(grepl(vte[2], .data$outcome_name),
                                            "DVT",
                                            ifelse(grepl(vte[3], .data$outcome_name),
                                                   "PE",
                                                   ifelse(grepl(ate[1], .data$outcome_name),
                                                          "ATE",
                                                          ifelse(grepl(ate[2], .data$outcome_name),
                                                                 "IS",
                                                                 ifelse(grepl(ate[3], .data$outcome_name),
                                                                        "TIA",
                                                                        ifelse(grepl(ate[4], .data$outcome_name),
                                                                               "MI",
                                                                               ifelse(grepl(oth[1], .data$outcome_name),
                                                                                      "HF",
                                                                                      ifelse(grepl(oth[2], .data$outcome_name),
                                                                                             "HS",
                                                                                             ifelse(grepl(oth[3], .data$outcome_name),
                                                                                                    "MP",
                                                                                                    "VA/CA"))))))))))) %>%
  distinct() %>%
  filter(censoring_method %in% c("main", "sa_covid", "sa_vaccine")) %>%
  mutate(censoring_method = factor(censoring_method, levels = c("main", "sa_covid", "sa_vaccine"))) 

captions <- comparisons %>%
  filter(skip != 1) %>%
  filter(grepl("(VE)", .data$comparison_name)) %>%
  select(cdm_name, exposure_name, comparator_name) %>%
  mutate(nice_exposure_name = ifelse(exposure_name == "any vaccine vaccinated",
                                     "any COVID-19 vaccine",
                                     ifelse(exposure_name == "astrazeneca vaccinated",
                                            "ChAdOx1 vaccine",
                                            ifelse(exposure_name == "pfizer vaccinated",
                                                   "BNT162b2 vaccine",
                                                   "no")))) %>%
  filter(nice_exposure_name != "no") %>%
  filter(!grepl(" vaccinated", .data$comparator_name)) %>%
  mutate(cdm_name_nice = ifelse(.data$cdm_name == "AURUM" | .data$cdm_name == "GOLD",
                                paste0("CPRD ", .data$cdm_name),
                                .data$cdm_name)) %>%
  mutate(caption_w_1 = paste0("Characteristics of weighted populations in ", cdm_name_nice, ","),
         caption_u_1 = paste0("Characteristics of unweighted populations in ", cdm_name_nice, ","),
         caption_2 = paste0(" database, stratified by staggered cohort and exposure status. Exposure is ", nice_exposure_name, ".")) %>%
  select(-nice_exposure_name) %>%
  distinct() %>%
  mutate(cve = FALSE) 

captions_outcome <- captions %>%
  select(exposure_name, cve) %>%
  distinct() %>%
  cross_join(tibble(
    censoring_method = c("main", "sa_vaccine", "sa_covid")
  )) %>%
  mutate(nice_exposure_name = ifelse(exposure_name == "any vaccine vaccinated",
                                     "any COVID-19 vaccine",
                                     ifelse(exposure_name == "astrazeneca vaccinated",
                                            "ChAdOx1 vaccine",
                                            ifelse(exposure_name == "pfizer vaccinated",
                                                   "BNT162b2 vaccine",
                                                   "no"))),
         censoring_name = case_when(
           censoring_method == "main" ~ " ",
           censoring_method == "sa_pcs" ~ "Vaccination between COVID-19 and outcome not allowed.",
           censoring_method == "sa_covid" ~ "Only first outcome after COVID-19 captured.",
           censoring_method == "sa_vaccine" ~ "Follow-up ends at first vaccine dose after index date."
         ))  %>%
  mutate(
    caption_1 = paste0("Number of records (and risk per 10,000 individuals) for post COVID-19 cardiac and thromboembolic complications,"),
    caption_2 = ifelse(cve,
                       paste0(" across cohorts and databases, stratified by vaccine. ", censoring_name),
                       paste0(" across cohorts and databases, stratified by exposure status (", nice_exposure_name, "). ", censoring_name)))

border <- fp_border_default(width = 0.5, color = "gray")

order_main <- expand_grid("Staggered cohort" = c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4"),
                          "Days since COVID-19" = c("0 to 30 days", "31 to 90 days", "91 to 180 days", "181 to 365 days"),
                          "Post COVID outcomes" = c("VTE", "ATE","HF"))
order_sup <- expand_grid("Staggered cohort" = c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4"),
                         "Days since COVID-19" = c("0 to 30 days", "31 to 90 days", "91 to 180 days", "181 to 365 days"),
                         "Post COVID outcomes" = c("VTE", "DVT","PE", "ATE", "IS", "TIA", "MI", "HF", "HS", "MP", "VACA"))
