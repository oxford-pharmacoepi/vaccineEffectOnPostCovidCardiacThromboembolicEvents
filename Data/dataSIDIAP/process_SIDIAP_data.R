library(here)
library(readr)
library(dplyr)

pop <- read_csv(here("Data", "dataSIDIAP", "population.csv"), show_col_types = FALSE,quote = '"')
result <- read_delim(here("Data", "dataSIDIAP", "regio_diari_total_pob.csv"), show_col_types = FALSE, delim = ";") %>%
  group_by(NOM, DATA) %>%
  summarise(
    number_cases = sum(CASOS_CONFIRMAT),
    num_tests = sum(PCR + TAR),
    dosi1 = sum(VACUNATS_DOSI_1),
    dosi2 = sum(VACUNATS_DOSI_2),
    .groups = "drop"
  ) %>%
  rename("region" = "NOM", "date" = "DATA") %>%
  group_by(region) %>%
  arrange(date) %>%
  mutate(
    cum_cases = cumsum(number_cases),
    cum_tests = cumsum(num_tests),
    cum_dosi1 = cumsum(dosi1),
    cum_dosi2 = cumsum(dosi2)
  ) %>%
  left_join(pop, by = "region") %>%
  mutate(new_cases_7d = cum_cases - lag(cum_cases, 7, 0)) %>%
  mutate(new_tests_7d = cum_tests - lag(cum_tests, 7, 0)) %>%
  ungroup() %>%
  mutate(new_cases_7d_100k = new_cases_7d / population * 100000) %>%
  mutate(new_tests_7d_100k = new_tests_7d / population * 100000) %>%
  mutate(dose1_fraction = cum_dosi1 / population * 100) %>%
  mutate(dose2_fraction = cum_dosi2 / population * 100) %>%
  filter(date >= as.Date("2021-01-04")) %>%
  filter(date <= as.Date("2021-08-31")) %>%
  select("region", "date", "new_cases_7d_100k", "new_tests_7d_100k", "dose1_fraction", "dose2_fraction")

result <- result %>%
  mutate(region = factor(.data$region, 
                         levels = pop$region, 
                         labels = c("alt pirineu-aran","barcelona","tarragona",
                                    "catalunya central","girona","lleida","metropolitana nord",
                                    "metropolitana sud","terres de lebre")))

write_csv(result, here("Data", "data_SIDIAP.csv"))
