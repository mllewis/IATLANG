# get gender measure from occupation translations

library(tidyverse)
library(here)


OUTFILE_LANGUAGE <- here('data/study2/occupation_gender_scores.csv')
OUTFILE_WORD_LANGUAGE <- here('data/study2/occupation_gender_scores_by_word.csv')

LANG_CODE_PATH <- here("data/study0/processed/lang_name_to_wiki_iso.csv")
INFILE <- here('data/study2/occupation_translations_tidy.csv')

tidy_clean_occs <- read_csv(INFILE) 

wide_occs <- tidy_clean_occs %>%
  group_by(language, occupation, word_form_type) %>%
  nest(translation) %>%
  spread(word_form_type, data)

# get score by word x language
by_item_lang_scores <-  wide_occs %>%
  mutate(female_overlap_with_male = map2_dbl(female_form, male_form, function(x,y) {length(intersect(x$translation,
                                                                            y$translation))/length(x$translation)}),
         male_overlap_with_female = map2_dbl(female_form, male_form, function(x,y) {length(intersect(x$translation,
                                                                                                 y$translation))/length(y$translation)})) %>%
  rowwise() %>%
  mutate(mean_overlap = mean(c(female_overlap_with_male, male_overlap_with_female)),
         mean_overlap = case_when(mean_overlap == "NaN"~ 0,
                                  TRUE ~ mean_overlap)) %>%
  select(-contains("male"), -contains("female")) 

lang_codes <- read_csv(LANG_CODE_PATH) %>%
  mutate(language_name = tolower(language_name)) 

by_item_lang_scores_tidy <- by_item_lang_scores %>% 
  mutate(language = case_when(language == "Serbian (latin)\n"~ "serbian (latin)",
                              TRUE ~ language)) %>%
  left_join(lang_codes, by = c("language" = "language_name")) %>%
  select(-language) %>%
  rename(language_code = wiki_language_code)

write_csv(by_item_lang_scores_tidy, OUTFILE_WORD_LANGUAGE)

# get score by language
by_lang_scores_tidy <- by_item_lang_scores_tidy %>%
  group_by(language_code) %>%
  summarize(mean_prop_overlap_occs = mean(mean_overlap)) %>%
  arrange(mean_prop_overlap_occs)

write_csv(by_lang_scores_tidy, OUTFILE_LANGUAGE)

