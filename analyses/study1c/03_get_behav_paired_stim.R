# get pairings between category and attribute words 
library(tidyverse)
library(here)

STIM_PATH <- here("data/study1c/raw/AIID_category_stimuli.csv")
OUTFILE <- here("data/study1c/processed/category_attribute_pair_stim.csv")

# get cat-att pairs
stim <- read_csv(STIM_PATH)[-1,]

names(stim) <- c("domain", "cat_id", "cat_label",
                 "S1", "S2", "S3", "S4", "S5", "S6", "S7",
                 "S8", "S9", "evaluative_label", "notes")

rep_domain <- stim %>%
  select(domain) %>%
  filter(!is.na(domain)) %>%
  pull(domain) %>%
  rep(each = 2)

domain_eval_set <- stim %>%
  slice(1:190) %>%
  select(evaluative_label) %>%
  mutate(domain = rep_domain) %>%
  filter(!is.na(evaluative_label)) %>%
  mutate(evaluative_label = str_replace(evaluative_label, " ", "_"))

write_csv(domain_eval_set, OUTFILE)