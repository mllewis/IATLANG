# get behavioral iat words, by further filtering based on whether or not they appear in the models 
# (i.e. occur more than 5 times)
library(tidyverse)
library(here)

COCA_TRAINED <-here("data/study1c/processed/trained_coca_fasttext_5.csv") 
BNC_TRAINED <- here("data/study1c/processed/trained_bnc_fasttext_5.csv") 
CATEGORIES  <- here("data/study1c/processed/filtered_category_stim.csv") 
ATTRIBUTES <- here("data/study1c/processed/filtered_attribute_stim.csv") 
OUTFILE <- here("data/study1c/processed/all_target_words_5.csv") 
MIN_PROP_PRESENT <- .75

## category words
coca <- read_csv(COCA_TRAINED)
bnc <- read_csv(BNC_TRAINED)
cats <- read_csv(CATEGORIES)

cat_words <- cats %>%
  select(domain, cat_id, stim_name, stim_id) %>%
  left_join(coca %>% select(target_word) %>% mutate(has_word = 1), 
            by = c("stim_name" = "target_word")) %>%
  left_join(bnc %>% select(target_word) %>% mutate(has_word = 1), 
          by = c("stim_name" = "target_word")) %>%
  rename(coca = has_word.x,
         bnc = has_word.y) %>%
  mutate(coca = case_when(is.na(coca) ~ 0, TRUE ~ coca),
         bnc = case_when(is.na(bnc) ~ 0, TRUE ~ bnc)) %>%
  gather("corpus", "word_is_present", -1:-4) 

# get the set of words in each model
words_in_both_models <- cat_words %>%
  filter(word_is_present == 1) %>%
  count(domain, cat_id, stim_name) %>%
  filter(n == 2) %>%
  select(-n) %>%
  mutate(word_in_both = 1)

# get prop present for each domain (in both models)
prop_cat_present <- cat_words %>%
  distinct(domain, cat_id, stim_name, stim_id) %>%
  left_join(words_in_both_models) %>%
  mutate(word_in_both = case_when(is.na(word_in_both) ~ 0, TRUE ~ word_in_both)) %>%
  group_by(domain, word_in_both) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  filter(word_in_both == 1) %>%
  select(-word_in_both)

target_domains <- prop_cat_present %>%
  filter(prop > MIN_PROP_PRESENT) %>%
  pull(domain)

target_cats <- cat_words %>%
  distinct(domain, cat_id, stim_name, stim_id) %>%
  filter(domain %in% target_domains,
         stim_name %in% words_in_both_models$stim_name)
  
## attribute words
atts <- read_csv(ATTRIBUTES)

att_words <- atts %>%
  select(set_id, stim_name, stim_id, cat_label) %>%
  left_join(coca %>% select(target_word) %>% mutate(has_word = 1), 
            by = c("stim_name" = "target_word")) %>%
  left_join(bnc %>% select(target_word) %>% mutate(has_word = 1), 
            by = c("stim_name" = "target_word")) %>%
  rename(coca = has_word.x,
         bnc = has_word.y) %>%
  mutate(coca = case_when(is.na(coca) ~ 0, TRUE ~ coca),
         bnc = case_when(is.na(bnc) ~ 0, TRUE ~ bnc)) %>%
  gather("corpus", "word_is_present", -1:-4) 

# get only those attribute words that are in both models
target_atts <- att_words %>%
  filter(word_is_present == 1) %>%
  count(cat_label, stim_name, set_id, stim_id) %>%
  filter(n == 2)  %>%
  select(-n)

## combine attribute and category words
all_target_words <- target_atts %>%
  rename(domain = set_id,
         cat_id = cat_label) %>%
  bind_rows(target_cats)

write_csv(all_target_words, OUTFILE)

