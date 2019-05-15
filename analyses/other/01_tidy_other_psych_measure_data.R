## Tidy Falk & Hermle (2018) and Stoet & Geary (2018) data (for SI)
library(tidyverse)
library(here)
library(readstata13)
library(countrycode)

STOET_PATH <- here("data/other/stoet_geary_2018_data.csv")
FALK_PATH <- here("data/other/falk_hermle_2018_data.dta")
LANGUAGE_COUNTRY_IN <- here("data/study0/processed/top_lang_by_country_ethnologue.csv")
LANG_IAT_PATH <- here("data/study1b/iat_es_lang.csv")

OUTFILE <- here("data/other/tidy_other_psych_measures.csv")

stoet_data <- read_csv(STOET_PATH) %>%
  mutate(country_code = countrycode(country_name, "country.name", "iso2c")) %>%
  select(country_code, self_efficacy_diff, intra_indv_diff) %>%
  rename(self_efficacy_diff_sg = self_efficacy_diff, 
         intra_indv_diff_sg = intra_indv_diff)

falk_data <- read.dta13(FALK_PATH) %>%
  mutate(country_code = countrycode(ison, "iso3n", "iso2c")) %>%
  select(country_code, genderdif) %>%
  rename(genderdif_fh = genderdif)

unique_langs_per_country <- read_csv(LANGUAGE_COUNTRY_IN) %>%
  select(country_code, wiki_language_code, language_name) %>%
  rename(language_code = wiki_language_code)

other_data <- full_join(stoet_data, falk_data) %>%
  full_join(unique_langs_per_country)

iat_lang_es <- read_csv(LANG_IAT_PATH)

tidy_other_data <- other_data %>%
 # group_by(language_code, language_name)%>%
 # summarize_if(is.numeric, mean, na.rm = T) %>%
  filter(!is.na(language_code)) %>%
  left_join(iat_lang_es) %>%
  filter(language_code != "zu")

write_csv(tidy_other_data, OUTFILE)