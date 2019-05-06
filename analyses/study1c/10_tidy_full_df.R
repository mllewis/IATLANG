# merge behavioral (long form) and language data together
library(tidyverse)
library(here)

LANGUAGE_PATH <- here("data/study1c/processed/bnc_vs_coca_es_400_10_x5.csv")
RAW_BEHAVIORAL_CONF <- here("data/study1c/processed/tidy_behavioral_iat_data_confirmatory_full.csv") 
OUTFILE <- here("data/study1c/processed/long_form_confirmatory_behavior_and_language.csv") 

# language es (5 runs of each model)
es_lang_raw <- read_csv(LANGUAGE_PATH) 
es_lang_tidy <- es_lang_raw %>%
  spread(model, effect_size) %>%
  rename(coca_lang_es = coca, 
         bnc_lang_es = bnc) %>%
  mutate(lang_diff = bnc_lang_es - coca_lang_es) # get bnc - coca language es difference 

# behavioral
behavioral <- read_csv(RAW_BEHAVIORAL_CONF) %>%
  select(user_id, domain, residence, resid)%>%
  rename(behavioral_effect_resid = resid) %>%
  mutate(user_id = as.character(user_id)) 

# full df
full_df <- behavioral %>%
  mutate(temp = list(es_lang_tidy)) %>%
  unnest() %>%
  filter(domain == domain1) %>%
  select(-domain1) %>%
  mutate_if(is.character, as.factor) 

write_csv(full_df, OUTFILE)