# get by-language bias mean based on occupation label biases

library(tidyverse)
library(here)

INFILE<-   here("data/study3_SI/study2_replication/wiki_native_occupation_gender_score.csv")
OUTFILE  <-  here("data/study3_SI/study2_replication/occupation_gender_score_by_language_native_wiki.csv")

wiki_scores <- read_csv(INFILE) %>%
  mutate(model = "wiki_native",
         language_code = case_when(language_code %in% c("hr", "sr") ~ "hr", # average across hr and sr
                                TRUE~ language_code))

by_lang_scores <- wiki_scores %>%
    mutate(gender_diff_score_mf = male_score - female_score) %>%
    rename(gender_diff_score_fm = gender_diff_score) %>%
    group_by(model, language_code) %>%
    summarize(gender_diff_score_mf = mean(gender_diff_score_mf, na.rm = T),
              gender_diff_score_fm = mean(gender_diff_score_fm, na.rm = T))

by_lang_scores_wide_mf <- by_lang_scores %>%
  select(-gender_diff_score_fm) %>%
  spread(model, gender_diff_score_mf) %>%
  rename(wiki_native_occu_semantics_mf = wiki_native)

by_lang_scores_wide_fm <- by_lang_scores %>%
  select(-gender_diff_score_mf) %>%
  spread(model, gender_diff_score_fm) %>%
  rename(wiki_native_occu_semantics_fm = wiki_native)

by_lang_scores_wide <- full_join(by_lang_scores_wide_mf, by_lang_scores_wide_fm)

write_csv(by_lang_scores_wide, OUTFILE)
