# calculate language iat using caliskan method

library(tidyverse)
library(here)
source(here("analyses/study1c/IAT_utils.R"))

BNC_PATH_F <- here("data/study1c/processed/trained_bnc_fasttext_400_10.csv")
COCA_PATH_F <- here("data/study1c/processed/trained_coca_fasttext_400_10.csv")
TARG_WORDS <- here("data/study1c/processed/all_target_words_5.csv") 
WORD_LIST <- here("data/study1c/processed/all_stim_lists_5.RData")
OUTFILE <- here("data/study1c/processed/bnc_vs_coca_es_400_10.csv")

# function for getting iat score for different models
get_effect_sizes_for_given_model <- function(model_path, word_df, stim_sets){
  
  model_source_name <- tail(str_split(model_path, "/")[[1]], 1)
  model_df <- read_csv(model_path)
  
  # check that all words in model
  print(paste0("Num words missing from model: ", setdiff(word_df$stim_name, model_df$target_word)))
  
  # get es
  effect_sizes <- map_df(stim_sets, get_ES, model_df)   %>%
    mutate(model_source = model_source_name)
}


all_targ_words <- read_csv(TARG_WORDS)
load(WORD_LIST) # list of target words (all_stim_sets)
paths <- c(BNC_PATH_F, COCA_PATH_F)

all_es <- map_df(paths, 
                 get_effect_sizes_for_given_model,
                 all_targ_words, 
                 all_stim_sets)

all_es_tidy <- all_es %>%
  rename(domain = test) %>%
  select(-bias_type) %>%
  rowwise() %>%
  mutate(model_source = str_split(model_source, ".csv")[[1]][1],
         model_source = str_split(model_source, "trained_")[[1]][2]) %>%
  select(model_source, domain, effect_size)

write_csv(all_es_tidy, OUTFILE)

