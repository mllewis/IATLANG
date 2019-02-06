# tidy attribute (good vs. bad) stimuli csv from aiid dataset, for all 95 iats

library(tidyverse)
library(here)

STIM_PATH <- here("data/study1c/raw/AIID_attribute_stimuli.csv")
OUTFILE <- here("data/study1c/processed/filtered_attribute_stim.csv")

stim <- read_csv(STIM_PATH)[-15:-18,]

names(stim) <- c("set_id", "cat_label", 
                 "A1", "A2", "A3", "A4", "A5", "A6")

stim_repeat <- stim %>% 
  select(set_id) %>% 
  filter(!is.na(set_id)) %>% 
  mutate(set_id = str_replace(set_id, " ", "_")) %>%
  pull(set_id) %>% 
  rep(each = 2) 

tidy_stim <- stim %>%
  mutate(set_id = stim_repeat,
         cat_label = rep(c("good", "bad"), 7)) %>%
  gather("stim_id", "stim_name", c(-1:-2)) %>%
  arrange(set_id, cat_label) %>%
  mutate_all(as.factor)  %>%
  mutate(stim_name = tolower(stim_name))

write_csv(tidy_stim, OUTFILE)


