# This data relies on a large file that causes the app to take a long time to load
# this file caches this data into a summary format for the DV histograms
library(here)
library(tidyverse)

PARTICIPANT_PATH <- here("data/study0/processed/by_participant_df.csv")
MULTIPLE  <- .25
PARTICIPANT_HISTO_PATH <- here("writeup/journal/SI/data/DV_histo_data.csv")

participant_df <- read_csv(PARTICIPANT_PATH)
histo_data <- participant_df %>%
  select(country_name, overall_iat_D_score, explicit_dif, 
         es_iat_sex_age_order_explicit_resid, es_iat_sex_age_order_implicit_resid) %>%
  gather("measure", "value", -country_name) %>%
  group_by(country_name, measure) %>%
  mutate(bin = round(value/MULTIPLE) * MULTIPLE) %>%
  count(country_name, measure, bin) 

write_csv(histo_data, PARTICIPANT_HISTO_PATH)