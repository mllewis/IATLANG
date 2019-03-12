# cache df with all measures in it so can provide desscriptive statistics
# (this is the same df as in study1_writeup)

library(tidyverse)
library(here)

LANG_IAT_PATH <- here("data/study1b/iat_es_lang.csv")
LANG_FAMILY_PATH <- here("data/study0/processed/top_lang_by_country_ethnologue.csv")
BEHAVIORAL_IAT_PATH <- here("data/study0/processed/by_language_df.csv")
BY_LANGUAGE_OCCUPATION_PATH  <- here("data/study2/occupation_gender_score_by_language.csv")
OCCUPATION_OVERLAP_PATH <- here('data/study2/occupation_gender_scores.csv')
LANGUAGE_NAME_PATH <- here("data/study0/processed/lang_name_to_wiki_iso.csv")
OUTFILE <- here("writeup/journal/SI/data/tidy_measures.csv")

iat_lang_es <- read_csv(LANG_IAT_PATH)
lang_family <- read_csv(LANG_FAMILY_PATH) %>%
  select(wiki_language_code, family) %>%
  rename(language_code = "wiki_language_code") %>%
  distinct()

iat_behavioral_es <- read_csv(BEHAVIORAL_IAT_PATH) %>%
  rename(language_code = "wiki_language_code") %>%
  select(language_code, median_country_age, 
         prop_male,log_age, es_iat_sex_age_order_explicit_resid,
         es_iat_sex_age_order_implicit_resid, per_women_stem_2012_2017, n_participants)

# Study 2 measures (included here for making single grand table)
occupation_semantics <- read_csv(BY_LANGUAGE_OCCUPATION_PATH) 

by_lang_scores <- read_csv(OCCUPATION_OVERLAP_PATH)

language_names <- read_csv(LANGUAGE_NAME_PATH) %>%
  rename(language_code = wiki_language_code) %>%
  distinct(language_code, .keep_all = TRUE)

# combine lang and behavioral and family info
all_es <- left_join(iat_behavioral_es, iat_lang_es, by = "language_code") %>%
  left_join(lang_family)   %>%
  left_join(occupation_semantics)  %>% 
  left_join(by_lang_scores) %>%
  left_join(language_names)

write_csv(all_es, OUTFILE)
