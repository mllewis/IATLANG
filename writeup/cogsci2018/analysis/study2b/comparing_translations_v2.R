# compare translations

# load packages
library(tidyverse)
library(data.table)


####################### SET PARAMS ######################
INPUT_PATH <- "google_translated_words.csv"
OUTPUT_PATH <- "data/tidy_google_translations.csv"

goog = read_csv("data/tidy_google_translations.csv")%>%
  arrange(target_word, wiki_language_code)
hand = read_csv("data/tidy_hand_translations.csv") %>%
  arrange(target_word, wiki_language_code)

m = left_join(hand, goog, by = c('target_word', "wiki_language_code", "word_id")) %>%
  mutate(same = translation.x == translation.y)


####################### READ DATA ######################
# langs missing from translations that are present in IAT data: tl (tagalog), hr (croatian), ro (romanian), el (greek), th (thai), zu (zulu)
#### Pre-process word list text ####
translated_words_google <- read.csv(INPUT_PATH, 
                             encoding ='UTF-8', 
                             col.names= c("language_name", 
                                          "target_word",
                                          "translation"))

translated_words_hand<- read.csv("/Users/mollylewis/Documents/research/Projects/IATLANG/data/models/translations_for_models/Clean\ -\ IATLANG\ STUDY\ 1\ TRANSLATIONS.csv", 
                                    encoding ='UTF-8')

translated_clean_hand <- translated_words_hand %>%
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

google_crit <- translated_words_google %>%
  #filter(target_word %in% c("wedding", "salary")) %>%
  rename(translation_google = translation)

hand_crit <- translated_clean_hand %>%
  #filter(target_word %in% c("wedding", "salary")) %>%
  rename(translation_hand= translation) %>%
  rename(language_name = language_code)

m = full_join(google_crit, hand_crit) %>%
  distinct() %>%
  filter(translation_google != translation_hand) %>%
  filter(language_name %in% c("pl", "tr")) %>%
  arrange(language_name)
as.data.frame(m)

#turkish, polish
