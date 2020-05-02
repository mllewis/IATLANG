# get F1 data
library(tidyverse)
library(here)
library(broom)

DATA_OUTFILE <- here("writeup/journal/supporting_information/main_figures/F1/F1.csv")

GENDER_NORMS <- here("data/study1a/raw/GlasgowNorms.csv")
glasgow_norms <- read_csv(GENDER_NORMS) %>%
  rename(maleness_norm = GEND_M)  %>%
  select(word, maleness_norm) %>%
  rowwise() %>%
  mutate(word =  str_split(word, " ", simplify = T)[1],
         word = tolower(word)) %>%
  group_by(word)  %>%
  summarize(maleness_norm = mean(maleness_norm))  # take the mean across multiple sense of word

EMBEDDING_BIAS_SUB  <- here("data/study1a/processed/gender_bias_by_word_english_sub.csv")
EMBEDDING_BIAS_WIKI <- here("data/study1a/processed/gender_bias_by_word_english_wiki.csv")

sub_male_bias <- read_csv(EMBEDDING_BIAS_SUB) %>%
  rename(maleness_embedding_sub = male_score)

wiki_male_bias <- read_csv(EMBEDDING_BIAS_WIKI) %>%
  rename(maleness_embedding_wiki = male_score)

embedding_ratings <- sub_male_bias %>%
  full_join(wiki_male_bias, by = "word") %>%
  select(word, contains("embedding")) %>%
  filter_at(vars(contains("embedding")), any_vars(!is.na(.)))

MALE_WORDS <- c("son", "his","him","he", "brother","boy", "man", "male")
FEMALE_WORDS <- c("daughter", "hers", "her", "she",  "sister", "girl", "woman", "female")

all_ratings <- embedding_ratings %>%
  left_join(glasgow_norms) %>%
  filter(!(word %in% c(MALE_WORDS, FEMALE_WORDS)))

write_csv(all_ratings, DATA_OUTFILE)

#cor.test(all_ratings$maleness_embedding_sub, all_ratings$maleness_norm)
#cor.test(all_ratings$maleness_embedding_wiki, all_ratings$maleness_norm)
