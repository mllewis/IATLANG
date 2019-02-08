# calculate language iat using caliskan method

library(tidyverse)
library(here)
library(googledrive)
source(here("analyses/study1c/IAT_utils.R"))

# runs of model from google drive
MODEL_PATHS <- list(
  "trained_models/_bnc_model_5_400_FALSE_10_R1",
  "trained_models/_bnc_model_5_400_FALSE_10_R2",
  "trained_models/_bnc_model_5_400_FALSE_10_R3",
  "trained_models/_bnc_model_5_400_FALSE_10_R4",
  "trained_models/_bnc_model_5_400_FALSE_10_R5",
  "trained_models/_coca_model_5_400_FALSE_10_R1",
  "trained_models/_coca_model_5_400_FALSE_10_R2",
  "trained_models/_coca_model_5_400_FALSE_10_R3",
  "trained_models/_coca_model_5_400_FALSE_10_R4",
  "trained_models/_coca_model_5_400_FALSE_10_R5")
TARG_WORDS <- here("data/study1c/processed/all_target_words_5.csv") 
WORD_LIST <- here("data/study1c/processed/all_stim_lists_5.RData")
OUTFILE_RAW <- here("data/study1c/processed/bnc_vs_coca_es_400_10_x5.csv")

# function for getting iat score for different models
get_effect_sizes_for_given_model <- function(model_path, word_df, stim_sets){
  
  model_source_name <- tail(str_split(model_path, "/")[[1]], 1)
  drive_download(model_path, "temp_model_file.csv", overwrite = T)
  model_df <- read_csv("temp_model_file.csv")

  # check that all words are in model
  print(paste0("Num words missing from model: ", 
               setdiff(word_df$stim_name, model_df$target_word)))
  
  # get es
  effect_sizes <- map_df(stim_sets, get_ES, model_df)   %>%
    mutate(model_source = model_source_name)
}


all_targ_words <- read_csv(TARG_WORDS)
load(WORD_LIST) # list of target words (all_stim_sets)

all_es <- map_df(MODEL_PATHS, 
                 get_effect_sizes_for_given_model,
                 all_targ_words, 
                 all_stim_sets)

all_es_tidy <- all_es %>%
  rename(domain = test) %>%
  select(-bias_type) %>%
  rowwise() %>%
  mutate(model = str_split(model_source, "_")[[1]][2],
         run = tail(str_split(model_source, "_")[[1]],1)) %>%
  select(model, run, domain, effect_size)

# save es from 5 independent runs
file.remove("temp_model_file.csv")
write_csv(all_es_tidy, OUTFILE_RAW)



