# get model embeddings from google translate words

library(feather)
library(data.table)
library(tidyverse)
source("IAT_utils2.R")

####################### SET PARAMS ######################
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/wiki."
CALCULATED_VECTOR_PATH <-"subsetted_wiki_vectors/"

####################### DEFINE WORDS ######################
tidy_translations <- read.csv("tidy_google_genius_translations_roots.csv", encoding = "UTF-8") %>%
  rename(language_code = language_name) 

LANGKEY <- read_tsv("language_name_to_google.csv") 

TARGET_GOOGLE_LANGS <- LANGKEY %>%
  filter(!is.na(language_code), !is.na(google_language_code)) %>% 
  filter(is.na(complete)) %>%
  pull(google_language_code)

TARGET_GOOGLE_LANGS <- c("en", "es", "hi", "ur", "fr", "pt", "ml", "ta", "it", "ar", "el", "de", "ro", "tr", "ru", "zh-cn") 



#### define test words for genius-gender test ####
word_list <- list(test_name = "genius_gender", 
                  bias_type = "genius_gender",
                  category_1 = c("male", "man", "he", "him", "his"),
                  category_2 = c("female", "woman", "she", "her", "hers"),
                  attribute_1 = c("genius", "brilliant", "very smart"),
                  attribute_2 = c("create", "art", "very imaginative"))

####################### GET VECTORS ######################
save_subsetted_model <- function(current_lang, model_prefix){
  
  print(paste0("===== ", current_lang, " ====="))
  
  wiki_lang <- LANGKEY %>%
    filter(google_language_code == current_lang) %>%
    pull(language_code)
  
  model_path <- paste0(model_prefix, wiki_lang, ".vec")
 
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
    group_by(language_code, target_word) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) # mean across words
  
  # write calculated vectors
  write_csv(calculated_vectors, paste0(CALCULATED_VECTOR_PATH,
                                       current_lang, "_wiki_calculated_genius_google_root.csv"))
  
}

# get all subsetted models
# https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md
walk(TARGET_GOOGLE_LANGS, save_subsetted_model, MODEL_PREFIX)

walk("zh-cn", save_subsetted_model, MODEL_PREFIX)

