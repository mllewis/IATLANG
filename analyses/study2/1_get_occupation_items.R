# get gender score for each occupation word in each language
# get stimuli for occupation norming

library(tidyverse)
library(data.table)


GABRIELPATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/IATLANG/exploratory_analyses/11_xling_occupation_norms/gabriel/raw_data/GabrielAPP-B(2008).txt"
gabriel_norms <- read_delim(GABRIELPATH, delim = "\t") %>%
  select(1,2) %>%
  slice(-1:-4) %>%
  rename(occupation = `Appendix B`,
         human_english_male_rating_g = X2) %>%
  mutate(occupation = tolower(occupation),
         human_english_male_rating_g = as.numeric(human_english_male_rating_g))


MISERSKYPATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/IATLANG/exploratory_analyses/11_xling_occupation_norms/miseresky/mirsesky_norms_clean.csv"

misersky_norms <- read_csv(MISERSKYPATH) %>%
  filter(language == "english") %>%
  select(-language) %>%
  mutate(mean_gender_rating = -mean_gender_rating) %>%
  rename(human_english_male_rating_m = mean_gender_rating)

all_norms <- full_join(gabriel_norms, misersky_norms) %>%
  filter(!is.na(human_english_male_rating_m)) %>%
  mutate(quartile = ntile(human_english_male_rating_m, 4)) %>%
  arrange(quartile)

cor.test(all_norms$human_english_male_rating_g, 
         all_norms$human_english_male_rating_m)


##Selected items and human rating quantile:
#dancers	1
#nurses	1
#singers	1
#cleaners	1
#secretaries	1
#waiters	2
#journalists	2
#bakers	2
#authors	2
#athletes	2
#lawyers	3
#doctors/physicians	3
#professors	3
#governors	3
#judges	3
#sailors	4
#postmen	4
#mechanics	4
#hunters	4
#firefighters	4
