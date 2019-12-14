# get by-language bias mean based on occupation label biases

library(tidyverse)
library(here)

INFILE1  <-  here("data/study2/wiki_occupation_gender_score.csv")
INFILE2 <-  here("data/study2/sub_occupation_gender_score.csv")
OUTFILE  <-  here("data/study2/occupation_gender_score_by_language.csv")

wiki_scores <- read_csv(INFILE1) %>%
  mutate(model = "wiki",
         language_code = case_when(language_code %in% c("hr", "sr") ~ "hr", # average across hr and sr
                                TRUE~ language_code))

subt_scores <- read_csv(INFILE2) %>%
  mutate(model = "subt") %>%
  filter(language_code != "de") # over 50% missing

by_lang_scores <- wiki_scores %>%
 bind_rows(subt_scores) %>%
 mutate(gender_diff_score_fm_abs = abs(gender_diff_score)) %>%
 select(model, language_code, occupation, gender_diff_score_fm_abs) %>%
 group_by(model, language_code) %>%
 summarize(gender_diff_score_fm_abs = mean(gender_diff_score_fm_abs, na.rm = T))

by_lang_scores_wide_fm <- by_lang_scores %>%
  spread(model, gender_diff_score_fm_abs) %>%
  rename(subt_gender_diff_score_fm_abs = subt,
         wiki_gender_diff_score_fm_abs = wiki)

write_csv(by_lang_scores_wide_fm, OUTFILE)
