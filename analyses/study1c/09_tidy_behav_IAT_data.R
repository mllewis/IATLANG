# tidy behavioral data, and add residual information
library(tidyverse)
library(here)
library(modelr)

BEHAVIORAL_PATH <- here("data/study1c/raw/AIID_subset_confirmatory.csv") #here("data/study1c/raw/AIID_subset_exploratory.csv")
DOMAIN_PATH <- here("data/study1c/processed/all_target_words_5.csv")
OUTFILE1 <- here("data/study1c/processed/tidy_behavioral_iat_data_confirmatory_full.csv") #here("data/study1c/processed/tidy_behavioral_iat_data.csv")

# tidy behavioral data
behavioral_df <- read_csv(BEHAVIORAL_PATH)
behavioral_tidy <- behavioral_df %>%
  select(user_id, datetime_ymdhms,  residence, sex, age, 
         education, exclude_iat, domain, task_order, block_order, D) %>%
  mutate(domain = case_when(as.character(domain) == "Determinism - Free will" ~ "Determinism - Free Will",
                            TRUE ~ as.character(domain))) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, droplevels) 

# do exclusions
target_domains <- read_csv(DOMAIN_PATH) %>% 
  filter(cat_id != "good", cat_id != "bad") %>%
  distinct(domain) %>%
  pull(domain)

behavioral_us_uk <- behavioral_tidy %>%
  filter(residence %in% c("us", "uk")) # only us and uk residence

# count(behavioral_us_uk, residence)

behavioral_complete <- behavioral_us_uk %>%
  filter(!exclude_iat, # exclude participants using pre-defined criteria 
         domain %in% target_domains) %>% # focus only on target domains
  select(-exclude_iat) %>%
  drop_na()

# add residuals
resid_es <- behavioral_complete %>%
  add_residuals(lm(D ~ task_order + sex + age + block_order + education,
                   data = behavioral_complete))

write_csv(resid_es, OUTFILE1)
