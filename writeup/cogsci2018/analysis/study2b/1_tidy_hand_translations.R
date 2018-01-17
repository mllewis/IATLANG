### Tidy hand translations for Caliksan effect size measure ## (tidy_hand_translations.csv)

# load packages
library(tidyverse)
library(langcog)

#### Pre-process word list text ####
translated_words <- read.csv("data/Clean - IATLANG STUDY 1 TRANSLATIONS.csv", encoding = 'UTF-8')

# Note- I think that when you open it in excel it corrupts the encoding 
# (it might need to be saved as a "CSV UTF-8"?) Don't open it. See Clean - IATLANG STUDY 1 TRANSLATIONS_DO_NOT_OPEN!.csv for fresh copy.

language_codes <- distinct(read_csv("data/language_names_to_wiki_codes.csv"))

translated_clean <- translated_words %>%
  mutate(English = ENGLISH) %>%
  gather(language_name, translation, -1) %>%
  left_join(language_codes) %>%
  rename(target_word = ENGLISH) %>%
  select(target_word, wiki_language_code, translation) %>%
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

# write_csv(tidy_translations,"data/tidy_hand_translations.csv")
