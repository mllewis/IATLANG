# tidy stimuli csv from aiid dataset, for all 95 iats
# remove stimuli that are pictures or contains spaces

library(tidyverse)
library(here)

STIM_PATH <- here("data/study1c/raw/AIID_category_stimuli.csv")
OUTFILE <- here("data/study1c/processed/filtered_category_stim.csv")
SPACE_CUT_OFF <- .2 # max proportion of target words that can be missing due to having spaces

stim <- read_csv(STIM_PATH)[-1,]

names(stim) <- c("domain", "cat_id", "cat_label",
                 "S1", "S2", "S3", "S4", "S5", "S6", "S7",
                 "S8", "S9", "evaluative_label", "notes")

stim_repeat <- stim %>% 
  select(domain) %>% 
  filter(!is.na(domain)) %>% 
  pull(domain) %>% 
  rep(each = 2) 

tidy_stim <- stim %>%
  slice(1:190) %>%
  mutate(domain = stim_repeat) %>%
  gather("stim_id", "stim_name", c(-1:-3, -13, -14)) %>%
  arrange(domain, cat_label) %>%
  filter(!is.na(stim_name)) %>%
  mutate_all(as.factor) %>%
  select(-evaluative_label, -notes) %>%
  mutate(stim_name = tolower(stim_name))

pic_domains <- tidy_stim %>%
  mutate(is_pic = str_detect(stim_name,  ".jpg")) %>%
  filter(is_pic) %>%
  distinct(domain) %>%
  pull(domain)

domains_with_spaces <- tidy_stim %>%
  filter(!(domain %in% pic_domains)) %>% # exclude all domains that have spaces
  mutate(has_space = str_detect(stim_name,  " ")) 
  
good_domains <- domains_with_spaces %>%
    count(domain, has_space) %>%
    spread(key = has_space, value = n, fill = 0) %>%
    rename(has_space = `TRUE`,
           no_space = `FALSE`) %>%
    mutate(prop_has_space = has_space/(no_space + has_space)) %>%
    filter(prop_has_space < SPACE_CUT_OFF) %>%
    pull(domain)

tidy_stim_filtered <- tidy_stim %>%
  filter(domain %in% good_domains) 

write_csv(tidy_stim_filtered, OUTFILE)


