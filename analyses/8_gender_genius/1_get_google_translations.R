# gets translations for all the genius-career words

library(feather)
library(tidyverse)
library(data.table)
library(googleLanguageR)
source("essay_translation_helper.R")

####################### SET PARAMS ######################

#TARGET_GOOGLE_LANGS <- c("en", "es", "hi", "ur", "fr", "pt", "ml", "ta", "it") 
#TARGET_GOOGLE_LANGS <- c("ar", "el", "de", "ro", "tr")
TARGET_GOOGLE_LANGS <- read_tsv("language_name_to_google.csv") %>%
  filter(!is.na(language_code), !is.na(google_language_code)) %>% 
  filter(is.na(complete)) %>%
  pull(google_language_code)


OUTPUT_PATH <- "gender_genius_translations3.csv"
WORD_PATH <- "gender_genius_target_words.csv"
gl_auth("/Users/mollylewis/Documents/research/Projects/1_in_progress/L2ETS/studies/study2/analyses/8_tensor_factorization/A_preprocessing_R/wikipedia/L2ETS\ prompt\ translations-a8edd99b5aa9.json") # authenticate google api

################## GET ALL TRANSLATIONS ######################
target_words <- read_csv(WORD_PATH) %>%
  pull(target_words)

lw_combos <- expand.grid(TARGET_GOOGLE_LANGS, sort(target_words)) %>%
  rename(langs = Var1, words = Var2)

walk2(as.character(lw_combos$langs),  
      as.character(lw_combos$words), 
      get_essay_words_translation,
      OUTPUT_PATH) 
