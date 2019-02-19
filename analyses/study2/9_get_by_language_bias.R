# get by-language bias mean based on occupation label biases

library(tidyverse)
library(here)

INFILE1  <-  here("data/study2/wiki_occupation_gender_score.csv")
INFILE2 <-  here("data/study2/sub_occupation_gender_score.csv")
OUTFILE  <-  here("data/study2/occupation_gender_score_by_language.csv")

wiki_scores <- read_csv(INFILE1) %>%
  mutate(model = "wiki",
         language_code = case_when(language_code %in% c("hr", "sr") ~ "hr", # average across hr and sr
                                TRUE~ language_code)) %>%
  group_by(language_code, model, occupation) %>%
  summarize_at(vars(female_score, male_score, gender_diff_score), mean)

subt_scores <- read_csv(INFILE2) %>%
  mutate(model = "subt")

by_lang_scores <- wiki_scores %>%
    bind_rows(subt_scores) %>%
    group_by(model, language_code) %>%
    summarize(mean = mean(gender_diff_score, na.rm = T))  

by_lang_scores_wide <- by_lang_scores %>%
  spread(model, mean) %>%
  rename(subt_occu_semantics = subt,
         wiki_occu_semantics = wiki)

write_csv(by_lang_scores_wide, OUTFILE)