# cache target miskersky data

library(tidyverse)
library(here)

MISERSKY_RAW_PATH <- here("data/study2/misersky_norms_clean.csv")
STUDY2_WORD_ITEMS <-   here("data/study2/occupation_gender_scores_by_word.csv")

MISERSKY_OUT_PATH <- "data/misersky_target_items.csv"

items <- read_csv(STUDY2_WORD_ITEMS) %>%
  distinct(occupation) %>%
  pull(occupation)

misersky_norms_raw <- read_csv(MISERSKY_RAW_PATH)
misersky_norms <- misersky_norms_raw %>%
  filter(language == "english") %>%
  select(-language) %>% 
  mutate(occupation  = substr(occupation, 1, nchar(occupation)-1)) %>% # get rid of plural
  mutate(occupation = case_when(occupation == "postme" ~ "postman/postwoman",
                                occupation == "physician" ~ "doctor/physician",
                                occupation == "secretarie" ~ "secretary",
                                TRUE ~ occupation)) %>%
  filter(occupation %in% items) %>%
  arrange(mean_gender_rating) 

write_csv(misersky_norms, MISERSKY_OUT_PATH)


