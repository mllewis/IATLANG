# structure cat-att lists of words to calculate language iat
library(tidyverse)
library(here)

INFILE <- here("data/study1c/processed/all_target_words_5.csv") 
STIM_PATH <- here("data/study1c/processed/category_attribute_pair_stim.csv")
OUTFILE <- here("data/study1c/processed/all_stim_lists_5.RData")

# get list of category- attribut mappings
domain_eval_set <- read_csv(STIM_PATH)

# get target words
target_words <- read_csv(INFILE)

make_stim_list <- function(this_bias_type, targs, eval_pairs){
  
  cat_words <- targs %>%
    filter(domain == this_bias_type)
  
  att_words <- eval_pairs %>%
    filter(domain == this_bias_type) %>%
    left_join(targs, by = c("evaluative_label" = "domain"))
  
  list(test_name = this_bias_type,
       bias_type = this_bias_type,
       category_1 = filter(cat_words, cat_id == "X") %>% pull(stim_name),
       category_2 =  filter(cat_words, cat_id == "Y") %>% pull(stim_name),
       attribute_1 = filter(att_words, cat_id == "bad") %>% pull(stim_name),
       attribute_2 = filter(att_words, cat_id == "good") %>% pull(stim_name))
}

target_domains <- target_words %>%
  filter(!(cat_id %in% c("good", "bad"))) %>%
  distinct(domain) %>%
  pull(domain)

all_stim_sets <- map(target_domains, 
                     make_stim_list, 
                     target_words, 
                     domain_eval_set)

save(all_stim_sets,  file = OUTFILE)

