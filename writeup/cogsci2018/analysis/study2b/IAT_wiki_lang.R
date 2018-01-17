## Process embeddings for caliskan effect size measure for many languages ##

# load packages
library(tidyverse)
library(data.table)

source("../_shared/IAT_utils.R")

#### read in languages data ####
countries_to_langs <- read_csv("data/languages_with_percent.csv")$wiki_language_code
wiki_langs <- unique(countries_to_langs[!is.na(countries_to_langs)])[-15]

#### define test words for career-gender test ####
WORD_LIST <- list(test_name = "WEAT_6", # not identical to caliskan (caliskan used proper names)
           bias_type = "gender-bias-career-family",
           category_1 = c("male", "man", "boy", "brother", "he", "him", "his", "son"),
           category_2 = c("female", "woman", "girl", "sister", "she", "her", "hers", "daughter"),
           attribute_1 = c("executive", "management", "professional", "corporation", "salary", 
                                "office", "business", "career"),
           attribute_2 = c("home", "parents", "children", "family", "cousins", "marriage", 
                            "wedding", "relatives"))

#### loop over languages and get word vectors ####
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/wiki."
RAW_VECTOR_PATH <- "data/wiki_language_embeddings_career/raw/"
CALCULATED_VECTOR_PATH <- "data/wiki_language_embeddings_career/calculated/"
#CALCULATED_VECTOR_PATH <- "/Users/mollylewis/Documents/research/Projects/IATLANG/data/models/wikipedia/subsetted_career_models/calculated/"
ES_OUTPUT_PATH <- "data/career_effect_sizes.csv"
save_subsetted_model <- function(current_lang, tidy_translations, model_prefix, raw_prefix, calculated_prefix){
  
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
  
  # write raw vectors
  write_csv(relevant_vectors, paste0(raw_prefix, "wiki.", current_lang, "_raw_career.csv"))
  
  calculated_vectors <- relevant_vectors %>%
    select(-translation) %>%
    group_by(language_code, target_word, translation_id) %>% # sum across word ids
    summarise_at(vars(V2:V301), sum, na.rm = TRUE) %>%
    group_by(language_code, target_word) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) # mean across words
  
  # write calculated vectors
  write_csv(calculated_vectors, paste0(calculated_prefix, "wiki.", 
                                       current_lang, "_calculated_career.csv"))
  
}

# get all subsetted models
# https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md
walk(wiki_langs, save_subsetted_model, tidy_translations, MODEL_PREFIX, RAW_VECTOR_PATH, CALCULATED_VECTOR_PATH)

#### loop over langs and get effect sizes #### 
get_wiki_es <- function(current_lang, calculated_prefix, output_path, word_list){
  
  # read back in subsetted file
  subsetted_model <- read_csv(paste0(calculated_prefix, 
                                     "wiki.", current_lang, "_calculated_career.csv")) %>%
    select(-language_code)
  
  # calculate ES
  ES <- get_ES(word_list, subsetted_model) %>%
    mutate(language_code = current_lang) %>%
    select(language_code, everything())
  
  write_csv(ES, append = TRUE, path = output_path)

}

walk(wiki_langs, get_wiki_es, CALCULATED_VECTOR_PATH, ES_OUTPUT_PATH, WORD_LIST)
