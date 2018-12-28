# tidy raw ocuppations translations and pre-process 

library(tidyverse)
library(here)

print("tidy translations")

INFILE <- here('data/study2/occupation_translations_raw.csv')
OUTFILE <- here('data/study2/occupation_translations_tidy.csv')

INCOMPLETE_LANGS <- c("romanian","telugu", "zulu",
                    "indonesian", "hindi", "swedish", "arabic",
                    "croatian (latin)", "serbian (cyrillic)")

translated_words <- read_csv(INFILE) %>%
  select(1,2,4,6) %>% # exclude typically used columns for now
  janitor::clean_names() %>%
  filter(!(language %in% INCOMPLETE_LANGS))  # exclude missing languages
  

long_form_translations <- translated_words %>%
  mutate(male_form = case_when(is.na(male_form) & !is.na(female_form) ~ female_form,
                               TRUE ~ male_form), # if translation is missing for one gender, fill in other gender form
         female_form = case_when(is.na(female_form) & !is.na(male_form) ~ male_form,
                               TRUE ~ female_form)) %>%
  gather("word_form_type", "translation", c(-1,-4))

translated_clean <- long_form_translations %>%
  mutate(translation = trimws(translation), # trim leading/trailing white space
         translation = tolower(translation),
         translation = str_replace_all(translation, "\b+", ""),
         translation = str_replace_all(translation, "  *", " "), # get rid of multiple spaces
         translation = str_replace_all(translation, " -", "-"), # get rid of spaces around -
         translation = str_replace_all(translation, " - ", "-"),
         translation = str_replace_all(translation, "- ", "-"),
         translation = str_replace_all(translation, "-", " "), # replace dash with space to indicate diff word
         translation = str_replace_all(translation, "/ ", "/"),
         translation = str_replace_all(translation, " /", "/"),
         translation = str_replace_all(translation, " ", "^")) # denote spacevs in multiword utterances with ^

#### gather multiple translations for each translation ####
tidy_translations <- translated_clean %>%
  separate(translation, 
           c("t1", "t2", "t3", "t4", "t5", "t6", "t7"), "/") %>%
  gather("translation_id", "translation", -1:-3) %>%
  filter(!is.na(translation)) %>%
  mutate_all( as.factor) 

write_csv(tidy_translations, OUTFILE)
 
 