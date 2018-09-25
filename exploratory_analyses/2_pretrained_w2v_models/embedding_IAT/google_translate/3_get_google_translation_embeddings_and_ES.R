# get model embeddings from google translate words

library(feather)
library(data.table)
library(tidyverse)
source("../IAT_utils2.R")

####################### SET PARAMS ######################
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/wiki."
CALCULATED_VECTOR_PATH <-"../../../data/models/wikipedia/subsetted_career_model_google/"
OUTPUT_PATH <- "career_effect_sizes_google.csv"

####################### DEFINE WORDS ######################
tidy_translations <- read.csv("tidy_google_translations.csv", encoding = "UTF-8") %>%
  rename(language_code = language_name)
LANGS <- unique(tidy_translations$language_code)

#### define test words for career-gender test ####
word_list <- list(test_name = "WEAT_6", # not identical to caliskan (caliskan used proper names)
                  bias_type = "gender-bias-career-family",
                  category_1 = c("male", "man", "boy", "brother", "he", "him", "his", "son"),
                  category_2 = c("female", "woman", "girl", "sister", "she", "her", "hers", "daughter"),
                  attribute_1 = c("executive", "management", "professional", "corporation", "salary", 
                                  "office", "business", "career"),
                  attribute_2 = c("home", "parents", "children", "family", "cousins", "marriage", 
                                  "wedding", "relatives"))

####################### GET VECTORS ######################
save_subsetted_model <- function(current_lang, model_prefix){
  
  print(paste0("===== ", current_lang, " ====="))
  model_path <- paste0(model_prefix, current_lang, ".vec")
 
   # read in model from temp_filename
  model <- fread(model_path,    
                 skip = 1,
                 key = "V1",
                 encoding = "UTF-8",
                 data.table = TRUE,
                 verbose = F)
  
  # get model of the words we care about 
  translated_word_list <- tidy_translations %>%
    filter(language_code == current_lang) 
  
  relevant_vectors <- translated_word_list %>%
    merge(model  %>% rename(translation = V1),  # get vectors for relevant words only
          by = "translation", all.x = TRUE)

  calculated_vectors <- relevant_vectors %>%
    select(-translation) %>%
    group_by(language_code, target_word, translation_id) %>% # sum across word ids
    summarise_at(vars(V2:V301), sum, na.rm = TRUE) %>%
    group_by(language_code, target_word) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) # mean across words
  
  # write calculated vectors
  write_csv(calculated_vectors, paste0(CALCULATED_VECTOR_PATH, "wiki.", 
                                       current_lang, "_calculated_career_google.csv"))
  
}

# get all subsetted models
# https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md
walk(LANGS, save_subsetted_model, MODEL_PREFIX)

####################### GET ES ######################
get_wiki_es <- function(current_lang){
  
  # read back in subsetted file
  subsetted_model <- read_csv(paste0(CALCULATED_VECTOR_PATH, 
                                     "wiki.", current_lang, "_calculated_career_google.csv")) %>%
          select(-language_code)
        
  # calculate ES
  ES <- get_ES(word_list, subsetted_model) %>%
          mutate(language_code = current_lang) %>%
          select(language_code, everything())
  
  write_csv(ES, OUTPUT_PATH, append = TRUE)
}

effect_sizes <- map_df(LANGS, get_wiki_es)

####################### FIGURE OUT WHICH ARE MISSING TRANS ######################

get_num_missing <- function(current_lang){
  
  # read back in subsetted file
  subsetted_model <- read_csv(paste0(CALCULATED_VECTOR_PATH, 
                                     "wiki.", current_lang, "_calculated_career_google.csv")) %>%
    select(-language_code) %>%
    filter(V4 == 0 & V2 == 0 & V3 == 0)
  
  data.frame(lang = current_lang,
             nrows_missing = nrow(subsetted_model))
  
}

missing_rows <- map_df(LANGS, get_num_missing)
