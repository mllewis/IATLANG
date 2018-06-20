# tidy google translations

## process embeddings for caliskan effect size measure

# load packages
library(tidyverse)
library(data.table)


####################### SET PARAMS ######################
INPUT_PATH1 <- "gender_genius_translations.csv"
INPUT_PATH2 <- "gender_genius_translations2.csv"
INPUT_PATH3 <- "gender_genius_translations3.csv"

OUTPUT_PATH <- "tidy_google_genius_translations.csv"

####################### READ DATA ######################
#### Pre-process word list text ####
translated_words1 <- read.csv(INPUT_PATH1, 
                             encoding ='UTF-8', header = F, 
                             col.names= c("language_name", 
                                          "target_word",
                                          "translation"))

translated_words2 <- read.csv(INPUT_PATH2, 
                              encoding ='UTF-8', header = F, 
                              col.names= c("language_name", 
                                           "target_word",
                                           "translation"))
translated_words3 <- read.csv(INPUT_PATH3, 
                              encoding ='UTF-8', header = F, 
                              col.names= c("language_name", 
                                           "target_word",
                                           "translation"))

translated_words <- bind_rows(translated_words1, translated_words2) %>%
  bind_rows(translated_words3)
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
