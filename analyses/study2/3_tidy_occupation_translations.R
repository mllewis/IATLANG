# tidy raw ocuppations translations and pre-process for wiki

library(tidyverse)
library(here)
library(googlesheets)
library(janitor)

print("tidy translations")

OUTFILE <- here('data/study2/occupation_translations_tidy.csv')

# INFILE
# this sheet is the cleaned version of occupation_translations_raw pasted into google sheets
# it is cleaned by removing coding idiosyncracies (e.g. articles, optional words in parens, ? marks, added "NO TRANSLATION", etc.)
googlesheet_df <- gs_title("occupations for translation")
translated_words <- gs_read(googlesheet_df, 30) %>%
  select(1,2,4,6) %>% # exclude typically used columns for now
  clean_names() 
  
long_form_translations <- translated_words %>%
  mutate(male_form = case_when(is.na(male_form) & !is.na(female_form) ~ female_form,
                               TRUE ~ male_form), # if translation is missing for one gender, fill in other gender form
         female_form = case_when(is.na(female_form) & !is.na(male_form) ~ male_form,
                               TRUE ~ female_form)) %>%
  gather("word_form_type", "translation", c(-1,-4)) %>%
  filter(translation != "NO TRANSLATION") # cases where the translator explicitly indicated no form exisited

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
         translation = str_replace_all(translation, " ", "_"),)

#### gather multiple translations for each translation ####
tidy_translations <- translated_clean %>%
  separate(translation, 
           c("t1", "t2", "t3", "t4", "t5", "t6", "t7"), "/") %>%
  gather("translation_id", "translation", -1:-3) %>%
  mutate_at(vars(language, translation_id), as.factor)  %>%
  filter(!is.na(translation)) 

write_csv(tidy_translations, OUTFILE)
 
 