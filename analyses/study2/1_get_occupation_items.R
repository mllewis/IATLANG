# get stimuli for occupation norming
# using existing norms of gender bias in workforce by occupation (Misersky et al., 2014)

library(tidyverse)
library(here)

MISERSKY_PATH <- here("data/study2/misersky_norms_clean.csv")
misersky_norms_raw <- read_csv(MISERSKY_PATH) 

misersky_norms <- misersky_norms_raw %>%
  filter(language == "english") %>%
  select(-language) %>%
  mutate(mean_gender_rating = -mean_gender_rating) %>%
  rename(human_english_male_rating_m = mean_gender_rating) %>%
  mutate(quartile = ntile(human_english_male_rating_m, 4)) %>%
  arrange(quartile)


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
