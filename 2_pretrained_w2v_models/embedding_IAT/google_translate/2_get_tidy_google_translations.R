# tidy google translations

## process embeddings for caliskan effect size measure

# load packages
library(tidyverse)
library(data.table)


####################### SET PARAMS ######################
INPUT_PATH <- "google_translated_words.csv"
OUTPUT_PATH <- "tidy_google_translations.csv"

####################### READ DATA ######################
# langs missing from translations that are present in IAT data: tl (tagalog), hr (croatian), ro (romanian), el (greek), th (thai), zu (zulu)
#### Pre-process word list text ####
translated_words <- read.csv(INPUT_PATH, 
                             encoding ='UTF-8', 
                             col.names= c("language_name", 
                                          "target_word",
                                          "translation"))

####################### TIDY TRANSLATIONS ######################

translated_clean <- translated_words %>%
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
  filter(!is.na(translation)) %>%
  mutate_if(is.character, as.factor)

####################### WRITE DATA ######################
write_csv(tidy_translations, OUTPUT_PATH)
