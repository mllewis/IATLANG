## Get effect size measure from wiki embeddings for names ## 
# (career_effect_sizes_hand_translations.csv/career_effect_sizes_google_translations.csv) 
## lots of words, google translate
# load packages
library(tidyverse)
library(data.table)

source("../_shared/IAT_utils.R")

################ set parameters ############
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/wiki."

# google translations
TRANSLATION_PATH <- "data/tidy_google_translations.csv"
TRANSLATION_PATH2 <- "data/tidy_google_translations_expanded.csv"
RAW_VECTOR_PATH <- "data/wiki_language_embeddings_career/raw/expanded_google_" 
CALCULATED_VECTOR_PATH <- "data/wiki_language_embeddings_career/calculated/expanded_google_" 
ES_OUTPUT_PATH <- "data/career_effect_sizes_google_translations_expanded.csv"

WORD_LIST <- list(test_name = "WEAT_6", # not identical to caliskan (caliskan used proper names, and few words )
                  bias_type = "gender-bias-career-family",
                  category_1 = c("male", "man", "boy", "brother", "he", "him", "his", "son", 
                                 "mister", "father", "uncle", "sir", "gentleman", "patriarch", "husband", "nephew"),
                  category_2 = c("female", "woman", "girl", "sister", "she", "her", "hers", "daughter",
                                 "miss", "mother", "aunt", "madame", "lady", "matriarch", "wife", "niece"),
                  attribute_1 = c("executive", "management", "professional", "corporation", "salary", "office", "business", "career",
                                  "enterprise", "money", "meeting", "decisions", "operator",  "market", "supervisor", "administrator"),
                  attribute_2 = c("home", "parents", "children", "family", "cousins", "marriage", "wedding", "relatives",
                                  "house", "household", "kids", "chore", "kin", "people", "relationship", "folks"))

######################## read in language and word data ####################
countries_to_langs <- read_csv("data/language_names_to_wiki_codes.csv")$wiki_language_code
wiki_langs <- countries_to_langs[!is.na(countries_to_langs)]

#tidy_translations <- read_csv(TRANSLATION_PATH)
tidy_translations <- rbind(read_csv(TRANSLATION_PATH), read_csv(TRANSLATION_PATH2))


################## loop over languages and get word vectors #################
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
    filter(wiki_language_code == current_lang) %>%
    data.table()
  
  relevant_vectors <- translated_word_list %>%
    merge(model  %>% rename(translation = V1),  # get vectors for relevant words only
          by = "translation", all.x = TRUE)
  
  # write raw vectors
  write_csv(relevant_vectors, paste0(raw_prefix, "wiki.", current_lang, "_raw_career.csv"))
  
  calculated_vectors <- relevant_vectors %>%
    select(-translation) %>%
    group_by(wiki_language_code, target_word, translation_id) %>% # sum across word ids
    summarise_at(vars(V2:V301), sum, na.rm = TRUE) %>%
    group_by(wiki_language_code, target_word) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) # mean across words
  
  # write calculated vectors
  write_csv(calculated_vectors, paste0(calculated_prefix, "wiki.", 
                                       current_lang, "_calculated_career.csv"))
}

# # do the thing: get all subsetted models
# (full models from: https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md)
walk(wiki_langs, save_subsetted_model, tidy_translations, MODEL_PREFIX, RAW_VECTOR_PATH, CALCULATED_VECTOR_PATH)

######################## loop over langs and get effect sizes ########################
get_wiki_es <- function(current_lang, calculated_prefix, output_path, word_list){
  
  # read back in subsetted file
  subsetted_model <- read_csv(paste0(calculated_prefix, 
                                     "wiki.", current_lang, "_calculated_career.csv")) %>%
    select(-wiki_language_code)
  
  if (nrow(subsetted_model) > 10){ # does the subsetted model exist?
  # calculate ES
    ES <- get_ES(word_list, subsetted_model) %>%
      mutate(language_code = current_lang) %>%
      select(language_code, everything())
  } else {
    ES <- data.frame(language_code = current_lang)
  }
  
  write_csv(ES, append = TRUE, path = output_path)
}

# do the thing
walk(wiki_langs, get_wiki_es, CALCULATED_VECTOR_PATH, ES_OUTPUT_PATH, WORD_LIST)

######################## loop over langs and get effect sizes ########################
count_missing<- function(current_lang, calculated_prefix, output_path){
  
  # read back in subsetted file
  subsetted_model <- read_csv(paste0(calculated_prefix, 
                                     "wiki.", current_lang, "_calculated_career.csv")) %>%
    select(-wiki_language_code) %>%
    filter(V2 == 0)

  
  data.frame(language_code = current_lang,
             num_missing = nrow(subsetted_model))

}

# do the thing
m = map_df(wiki_langs, count_missing, CALCULATED_VECTOR_PATH)
