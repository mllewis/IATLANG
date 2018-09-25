## process embeddings for caliskan effect size measure
# load packages
library(knitr)
library(rmarkdown)
library(tidyverse)
library(langcog)
library(broom)
library(data.table)

source("IAT_utils2.R")

#### read in data ####
# list of languages that I got word translations for (N = 21)
translated_wiki_langs <- read_csv("../../data/other/langs_for_study1.csv") %>%
  add_row(language_name = "Hebrew", language_code = "he")

# mapping of countries to languages
countries_langs <- read_csv("../../data/other/countries_lang.csv") %>%
  mutate(language_name = ifelse(language_name == "Spanish; Castilian", "Spanish", language_name),
         language_name = ifelse(language_name == "Dutch; Flemish", "Dutch", language_name))

## countries we have IAT data for (N = 44)
iat_countries <- read_csv("../../data/other/iat_behavior_langs.csv") %>%
  left_join(countries_langs %>% select(country_code, language_code), 
            by = c("countryres"= "country_code"))

#iat_countries %>%
#  inner_join(translated_wiki_langs) %>%
#  count(language_name) %>%
#  as.data.frame()

# langs missing from translations that are present in IAT data: tl (tagalog), hr (croatian), ro (romanian), el (greek), th (thai), zu (zulu)
#### Pre-process word list text ####
translated_words <- read.csv("../../data/models/translations_for_models/Clean - IATLANG STUDY 1 TRANSLATIONS.csv",
                             encoding ='UTF-8')
# note that there are some arabic words that are not represented correctly - not sure if this is an R terminal issue or a real issue

translated_clean <- translated_words %>%
  mutate(English = ENGLISH) %>%
  gather(language_name, translation, -1) %>%
  left_join(countries_langs %>% select(language_name, language_code)) %>%
  rename(target_word = ENGLISH) %>%
  select(target_word, language_code, translation) %>%
  mutate(translation = trimws(translation),
         translation = tolower(translation),
         translation = str_replace(translation, "\b+", ""),
         translation = str_replace(translation, "/ ", "/"),
         translation = str_replace(translation, " /", "/"))

#### gather multiple translations and words for each translation ####
tidy_translations <- translated_clean %>%
  separate(translation, 
           c("t1", "t2", "t3", "t4", "t5", "t6", "t7"), "/") %>%
  gather("translation_id", "translation", -1:-2) %>%
  separate(translation, 
              c("w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9"), " ") %>%
  gather("word_id", "translation", -1:-3) %>%
  filter(!is.na(translation))
# write_csv(tidy_translations,"tidy_translations.csv")

#### define test words for career-gender test ####
word_list <- list(test_name = "WEAT_6", # not identical to caliskan (caliskan used proper names)
           bias_type = "gender-bias-career-family",
           category_1 = c("male", "man", "boy", "brother", "he", "him", "his", "son"),
           category_2 = c("female", "woman", "girl", "sister", "she", "her", "hers", "daughter"),
           attribute_1 = c("executive", "management", "professional", "corporation", "salary", 
                                "office", "business", "career"),
           attribute_2 = c("home", "parents", "children", "family", "cousins", "marriage", 
                            "wedding", "relatives"))

#### loop over languages and get word vectors ####
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/wiki."
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
  
  # write raw vectors
  relevant_raw_vector_path <- "../../data/models/wikipedia/subsetted_career_models/raw/"
  write_csv(relevant_vectors, paste0(relevant_raw_vector_path, "wiki.", current_lang, "_raw_career.csv"))
  
  calculated_vectors <- relevant_vectors %>%
    select(-translation) %>%
    group_by(language_code, target_word, translation_id) %>% # sum across word ids
    summarise_at(vars(V2:V301), sum, na.rm = TRUE) %>%
    group_by(language_code, target_word) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) # mean across words
  
  # write calculated vectors
  relevant_calculated_vector_path <- "../../data/models/wikipedia/subsetted_career_models/calculated/"
  write_csv(calculated_vectors, paste0(relevant_calculated_vector_path, "wiki.", 
                                       current_lang, "_calculated_career.csv"))
  
}

# get all subsetted models
# https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md
 walk(translated_wiki_langs$language_code[22], save_subsetted_model, MODEL_PREFIX)

#### loop over langs and get effect sizes #### 
get_wiki_es <- function(current_lang){
  
  # read back in subsetted file
  relevant_calculated_vector_path <- "../../data/models/wikipedia/subsetted_career_models/calculated/"
  
  subsetted_model <- read_csv(paste0(relevant_calculated_vector_path, 
                                     "wiki.", current_lang, "_calculated_career.csv")) %>%
    select(-language_code)
  
  # calculate ES
  ES <- get_ES(word_list, subsetted_model) %>%
    mutate(language_code = current_lang) %>%
    select(language_code, everything())
  
  write_csv(ES, "career_effect_sizes.csv", append = TRUE)
}

effect_sizes <- map_df("pl", get_wiki_es)

# write_csv(effect_sizes, "career_effect_sizes.csv")
