# get by-language bias mean based on occupation label biases

library(tidyverse)
library(here)

INFILE <-   here("data/study3_SI/study2_replication/wiki_native_occupation_gender_score.csv")
OUTFILE  <-  here("data/study3_SI/study2_replication/occupation_gender_score_by_language_native_wiki.csv")

wiki_scores <- read_csv(INFILE) %>%
  mutate(model = "wiki",
         language_code = case_when(language_code %in% c("hr", "sr") ~ "hr", # average across hr and sr
                                   TRUE~ language_code))

by_lang_scores <- wiki_scores %>%
  mutate(gender_diff_score_fm_abs = abs(gender_diff_score)) %>%
  select(model, language_code, occupation, gender_diff_score_fm_abs) %>%
  group_by(model, language_code) %>%
  summarize(gender_diff_score_fm_abs = mean(gender_diff_score_fm_abs, na.rm = T))

by_lang_scores_wide_fm <- by_lang_scores %>%
  spread(model, gender_diff_score_fm_abs) %>%
  rename(wiki_native_gender_diff_score_fm_abs = wiki)

write_csv(by_lang_scores_wide_fm, OUTFILE)
