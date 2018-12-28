# get gender measure from occupation translations

library(tidyverse)
library(here)

INFILE <- here('data/study2/occupation_translations_tidy.csv')
OUTFILE <- here('data/study2/occupation_gender_scores.csv')
OUTFILE2 <- here('data/study2/occupation_gender_scores_by_quantile.csv')

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

by_lang_scores <- by_item_lang_scores %>%
  group_by(language) %>%
  summarize(mean_prop_overlap_occs = mean(mean_overlap)) %>%
  arrange(mean_prop_overlap_occs) 

lang_codes <- read_csv(LANG_CODE_PATH) %>%
  mutate(language_name = tolower(language_name))

by_lang_scores_tidy <- by_lang_scores %>%
  left_join(lang_codes, by = c("language" = "language_name")) %>%
  rename(language_code = "wiki_language_code") %>%
  select(language_code, mean_prop_overlap_occs) 

write_csv(by_lang_scores_tidy, OUTFILE)

by_lang_scores2 <- by_item_lang_scores %>%
  mutate(gender_quantile = case_when(occupation %in% c("dancer", "nurse", "singer", "cleaner", "secretary") ~ 1,
                                     occupation %in% c("waiter", "journalist", "baker", "author", "athlete") ~ 2,
                                     occupation %in% c("lawyer", "doctor/physician", "professor", "governor", "judge") ~ 3,
                                     TRUE ~ 4)) %>%
  group_by(language, gender_quantile) %>%
  summarize(mean_prop_overlap_occs = mean(mean_overlap)) %>%
  arrange(mean_prop_overlap_occs) 
  


by_lang_scores_tidy2 <- by_lang_scores2 %>%
  left_join(lang_codes, by = c("language" = "language_name")) %>%
  rename(language_code = "wiki_language_code") %>%
  select(language_code, gender_quantile, mean_prop_overlap_occs) 

write_csv(by_lang_scores_tidy2, OUTFILE2)



