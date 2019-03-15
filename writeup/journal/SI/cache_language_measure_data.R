# cache all_es_tidy_scaled for Study 2 regressions
library(tidyverse)
library(here)

OUTFILE <- here("writeup/journal/SI/data/all_es_tidy2_scaled.csv")

# occupation form overlap by language
OCCUPATION_OVERLAP_PATH <- here('data/study2/occupation_gender_scores.csv')
by_lang_scores_tidy <- read_csv(OCCUPATION_OVERLAP_PATH) 

# Behavioral IAT by languages measure
BEHAVIORAL_IAT_PATH <- here("data/study0/processed/by_language_df.csv")
iat_behavioral_es <- read_csv(BEHAVIORAL_IAT_PATH) %>%
  rename(language_code = "wiki_language_code") %>%
  select(language_code, median_country_age, 
         prop_male,log_age, es_iat_sex_age_order_explicit_resid,
         es_iat_sex_age_order_implicit_resid, per_women_stem_2012_2017, n_participants)

LANG_IAT_PATH <- here("data/study1b/iat_es_lang.csv")
iat_lang_es <- read_csv(LANG_IAT_PATH)

all_es_tidy2_scaled <- full_join(by_lang_scores_tidy, iat_behavioral_es) %>%
  left_join(iat_lang_es)  %>%
  filter(language_code != "zu") %>% # exclude zulu as in study 1b 
  mutate_if(is.numeric, scale) 

write_csv(all_es_tidy2_scaled, OUTFILE)

