# calculate language iat using caliskan method, exploring model parameter space

library(tidyverse)
library(here)
library(fastrtext)
library(googledrive)

source(here("analyses/study1c/IAT_utils.R"))
source(here("analyses/study1c/parameter_exploration/param_exploration.R"))

TARG_WORDS <- here("data/study1c/processed/all_target_words_5.csv") 
WORD_LIST <- here("data/study1c/processed/all_stim_lists_5.RData")
COCA_CORPUS <- here("data/study1c/raw/COCAshort_words.txt")  
BNC_CORPUS <- here("data/study1c/raw/BNCspokenFormatted.txt")
TEMP_PATH <- here("analyses/study1c/parameter_exploration/trained_models/temp_model.csv")
OUTPATH <- here("analyses/study1c/parameter_exploration/es_params_400_10.csv")

# load stimuli sets
all_targ_words <- read_csv(TARG_WORDS)
load(WORD_LIST) # list of target words (all_stim_sets) - this is just to check that all words are in model

# define params space
all_params <- cross(list(wc = 5,
                         vs = c(400),
                         ng = c(FALSE),
                         ws = c(10)))



##### coca corpus ##### 
coca_corpus <- read_lines(COCA_CORPUS)   %>%
  str_split(" ") %>%
  unlist()

for (i in 1:5){
  walk(all_params, train_model_get_iat, coca_corpus, 
       all_targ_words, all_stim_sets, TEMP_PATH, OUTPATH,
       "coca")  
}

##### bnc corpus ##### 
bnc_corpus <- read_lines(BNC_CORPUS)  %>%
  str_split(" ") %>%
  unlist()

for (i in 1:4){
walk(all_params, train_model_get_iat, bnc_corpus, 
     all_targ_words, all_stim_sets, TEMP_PATH, OUTPATH,
     "bnc")  
}
