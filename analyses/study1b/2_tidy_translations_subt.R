# tidy raw iat word translations and pre-process so can look at concatenation in subttiel model
# load packages
library(tidyverse)
library(here)

print("tidy translations")

INFILE <- here('data/study1b/iat_translations_raw.csv')
OUTFILE <- here('data/study1b/iat_translations_tidy_subt.csv')

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
tidy_translations1 <- translated_clean %>%
  separate(translation, 
           c("t1", "t2", "t3", "t4", "t5", "t6", "t7"), "/") %>%
  gather("translation_id", "translation", -1:-3) %>%
  mutate(translation = str_replace_all(translation, " ", "_"),
         translation = case_when(str_detect(translation, "_") ~ paste(translation, toupper(translation)), # capitalize and add underscores to multiword phrases
                                  TRUE ~ translation )) %>%
  separate(translation, 
           c("w1", "w2"), " ") %>%
  gather("word_id", "translation", -1:-4) %>% # some concatenations seem to be capiltaized, others not. This takes the average acrosss both forms (though presumably only one exists)
  mutate(translation_type = case_when(str_detect(translation, "_") ~ "concatenated_multi", # capitalize and add underscores to multiword phrases
                          TRUE ~ "single" )) %>%
  mutate_at(vars(word, language, word_id, translation_id), as.factor)  %>%
  filter(!is.na(translation))

tidy_translations2 <- translated_clean %>%
  separate(translation, 
           c("t1", "t2", "t3", "t4", "t5", "t6", "t7"), "/") %>%
  gather("translation_id", "translation", -1:-3) %>%
  filter(str_detect(translation, " ") ) %>%
  separate(translation, 
           c("w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9"), " ") %>%
  gather("word_id", "translation", -1:-4) %>%
  mutate_at(vars(word, language, word_id, translation_id), as.factor)  %>%
  filter(!is.na(translation)) %>%
  mutate(translation_type = "separate_multi")

tidy_translations <- bind_rows(tidy_translations1, tidy_translations2)

gender_tidy_translations <- tidy_translations %>%
  mutate(gender = as.factor(ifelse(word %in% GENDERED_WORDS, gender, "N"))) %>%
  distinct()
  
 write_csv(gender_tidy_translations, OUTFILE)