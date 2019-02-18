# get gender measure from occupation translations

library(tidyverse)
library(here)

INFILE <- here('data/study2/occupation_translations_tidy.csv')
OUTFILE <- here('data/study2/occupation_gender_scores.csv')
LANG_CODE_PATH <- here("data/study0/processed/lang_name_to_wiki_iso.csv")

tidy_occs <- read_csv(INFILE)

wide_occs <- tidy_occs %>%
  group_by(language, occupation, word_form_type) %>%
  nest(translation) %>%
  spread(word_form_type, data)

# get score
by_item_lang_scores <-  wide_occs %>%
  mutate(female_overlap_with_male = map2_dbl(female_form, male_form, function(x,y) {length(intersect(x$translation,
                                                                            y$translation))/length(x$translation)}),
         male_overlap_with_female = map2_dbl(female_form, male_form, function(x,y) {length(intersect(x$translation,
                                                                                                 y$translation))/length(y$translation)})) %>%
  rowwise() %>%
  mutate(mean_overlap = mean(c(female_overlap_with_male, male_overlap_with_female)))

full_df <- by_item_lang_scores %>%
  group_by(language) %>%
  summarize(mean_prop_overlap_occs = mean(mean_overlap)) %>%
  arrange(mean_prop_overlap_occs) %>%
  mutate(language = case_when(language == "Serbian (latin)\n"~ "serbian (latin)",
                              TRUE ~ language))

lang_codes <- read_csv(LANG_CODE_PATH) %>%
  mutate(language_name = tolower(language_name))

by_lang_scores_tidy <- full_df %>%
  left_join(lang_codes, by = c("language" = "language_name")) %>%
  rename(language_code = "wiki_language_code") %>%
  select(language_code, mean_prop_overlap_occs) 

write_csv(by_lang_scores_tidy, OUTFILE)

