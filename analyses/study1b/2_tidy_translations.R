# tidy raw iat word translations
# load packages
library(tidyverse)

INFILE <- '../../data/study2b/iat_translations_raw.csv'
OUTFILE <- '../../data/study2b/iat_translations_tidy.csv'

GENDERED_WORDS <- c("executive","management","professional", "parents",
                    "children", "family", "cousins", "relatives")

translated_words <- read_csv(INFILE)

translated_clean <- translated_words %>%
  mutate(translation = trimws(translation), # trim leading/trailing white space
         translation = tolower(translation),
         translation = str_replace_all(translation, "\b+", ""),
         translation = str_replace_all(translation, "  *", " "), # get rid of multiple spaces
         translation = str_replace_all(translation, " -", "-"), # get rid of spaces around -
         translation = str_replace_all(translation, " - ", "-"),
         translation = str_replace_all(translation, "- ", "-"),
         translation = str_replace_all(translation, "-", " "), # replace dash with space to indicate diff word
         translation = str_replace_all(translation, "/ ", "/"),
         translation = str_replace_all(translation, " /", "/")) %>%
  select(word, language, gender, translation) 

#### gather multiple translations and words for each translation ####
tidy_translations <- translated_clean %>%
  separate(translation, 
           c("t1", "t2", "t3", "t4", "t5", "t6", "t7"), "/") %>%
  gather("translation_id", "translation", -1:-3) %>%
  separate(translation, 
              c("w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9"), " ") %>%
  gather("word_id", "translation", -1:-4) %>%
  mutate_at(vars(word, language, word_id, translation_id), as.factor)  %>%
  filter(!is.na(translation))

gender_tidy_translations <- tidy_translations %>%
  mutate(gender = as.factor(ifelse(word %in% GENDERED_WORDS, gender, "N"))) %>%
  distinct()
  
 write_csv(gender_tidy_translations, OUTFILE)
