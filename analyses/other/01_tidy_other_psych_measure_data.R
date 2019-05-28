## Tidy Falk & Hermle (2018) and Stoet & Geary (2018) data (for SI)
library(tidyverse)
library(here)
library(readstata13)
library(countrycode)

STOET_PATH <- here("data/other/stoet_geary_2018_data.csv")
FALK_PATH <- here("data/other/falk_hermle_2018_data.dta")
LANGUAGE_COUNTRY_IN <- here("data/study0/processed/top_lang_by_country_ethnologue.csv")
LANG_IAT_PATH <- here("data/study1b/iat_es_lang.csv")
PSYCH_IAT_PATH <- here("data/study0/processed/by_country_df.csv")


OUTFILE <- here("data/other/tidy_other_psych_measures.csv")

# get GDP 2017 data from World Bank API
gdp_data <- wbstats::wb(indicator = "NY.GDP.PCAP.CD", 
                        startdate = 2017, 
                        enddate = 2017) %>%
  select(iso2c, value) %>%
  rename(gdp_2017 = value,
         country_code = iso2c)

stoet_data <- read_csv(STOET_PATH) %>%
  mutate(country_code = countrycode(country_name, "country.name", "iso2c")) %>%
  select(per_women_stem, country_code, self_efficacy_diff, intra_indv_diff) %>%
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
psych_iat_es <- read_csv(PSYCH_IAT_PATH) %>%
  select(country_code, es_iat_sex_age_order_implicit_resid)

tidy_other_data <- other_data %>%
  filter(!is.na(language_code),
         language_code != "zu") %>%
  left_join(iat_lang_es) %>%
  left_join(gdp_data) %>%
  left_join(psych_iat_es) %>%
  mutate(country_name = countrycode(country_code, "iso2c","country.name"))

write_csv(tidy_other_data, OUTFILE)