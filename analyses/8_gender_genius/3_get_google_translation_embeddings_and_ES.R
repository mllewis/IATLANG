# get model embeddings from google translate words

library(feather)
library(data.table)
library(tidyverse)
source("IAT_utils2.R")

####################### SET PARAMS ######################
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/wiki."
CALCULATED_VECTOR_PATH <-"subsetted_wiki_vectors/"
OUTPUT_PATH <- "genius_effect_sizes_google_restricted.csv"

####################### DEFINE WORDS ######################
tidy_translations <- read.csv("tidy_google_genius_translations.csv", encoding = "UTF-8") %>%
  rename(language_code = language_name) 

LANGKEY <- read_csv("language_name_to_google.csv") 

TARGET_GOOGLE_LANGS <- LANGKEY %>%
  filter(!is.na(language_code), !is.na(google_language_code)) %>% 
  filter(is.na(complete)) %>%
  pull(google_language_code)

#### define test words for genius-gender test ####
word_list <- list(test_name = "genius_gender", 
                  bias_type = "genius_gender",
                  category_1 = c("male", "man", "he", "him", "his"),
                  category_2 = c("female", "woman",  "she", "her", "hers"),
                  attribute_1 = c("genius", "brilliant", "super smart"),
                  attribute_2 = c("creative", "artistic", "super imaginative"))

####################### GET ES ######################
get_wiki_es <- function(current_lang){
  
  
  # read back in subsetted file
  subsetted_model <- read_csv(paste0(CALCULATED_VECTOR_PATH, 
                                     current_lang, "_wiki_calculated_genius_google.csv")) %>%
    select(-language_code)
  
  wiki_lang <- LANGKEY %>%
    filter(google_language_code == current_lang) %>%
    pull(language_code) 
  
  # calculate ES
  ES <- get_ES(word_list, subsetted_model) %>%
        mutate(language_code = wiki_lang) %>%
        select(language_code, everything())
  
  write_csv(ES, OUTPUT_PATH, append = TRUE)
}

walk(TARGET_GOOGLE_LANGS, get_wiki_es)
