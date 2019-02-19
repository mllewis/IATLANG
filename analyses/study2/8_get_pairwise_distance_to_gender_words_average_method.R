# get gender bias score for each word using same method used in Caliskan
# in english, using both corpora (sub or wiki)

library(tidyverse)
library(data.table)
library(here)

MALE_WORDS <- c("son", "his","him", "he", "brother","boy", "man", "male") 
FEMALE_WORDS <- c("daughter", "hers", "her", "she",  "sister", "girl", "woman", "female")
ANCHOR_PATH <- here("data/study2/wiki_calculated_models/")
OUTFILE  <-  here("data/study2/wiki_occupation_gender_score.csv")
#ANCHOR_PATH <- here("data/study2/subt_calculated_models/")
#OUTFILE  <-  here("data/study2/sub_occupation_gender_score.csv")

all_models <- map_df(list.files(ANCHOR_PATH, full.names = T), ~ read_csv(.))  %>%
  mutate(word_type = case_when(word %in% MALE_WORDS ~ "anchor_M",
                               word %in% FEMALE_WORDS ~ "anchor_F",
                               TRUE ~ "occupation")) %>%
  select(language_code, word, gender, word_type, everything())

# functions for doign the thing
get_gender_score_for_one_gender <- function(this_mat, these_anchor_words, target_gender){
  
  target_mat <- this_mat %>%
    filter(gender == target_gender | word_type == paste0("anchor", "_", target_gender))  %>%
    select(-gender)
  
  word_word_dists <- coop::cosine(t(as.matrix(target_mat[,c(-1:-2)])))
  
  wide_word_word_dists <- word_word_dists %>%
    as.data.frame()  %>%
    mutate(word1 =  target_mat$word,
           word_type1 = target_mat$word_type) %>% 
    select(word1, word_type1, everything())  
  
  names(wide_word_word_dists)  = c("word1", "word_type1", wide_word_word_dists$word1)
  
  distances <- wide_word_word_dists  %>%
    filter(word_type1 != "occupation") %>%
    select(-word1, -word_type1) %>%
    summarize_all(mean, na.rm = T) %>%
    gather("occupation", "mean_cosine_distance") %>%
    filter(!(occupation %in% these_anchor_words)) %>%
    mutate(gender = target_gender)
}
get_gender_scores <- function(mat, anchor_words){
  f_distances <- get_gender_score_for_one_gender(mat, anchor_words, "F")
  m_distances <- get_gender_score_for_one_gender(mat, anchor_words, "M")
  bind_rows(f_distances, m_distances) %>%
    select(occupation, gender, mean_cosine_distance)
}

### DO THE THING ##
crit_dists_df <- all_models %>%
  group_by(language_code) %>%
  nest() %>%
  mutate(temp = map(data, get_gender_scores, c(MALE_WORDS, FEMALE_WORDS))) %>%
  select(-data) %>%
  unnest()

# add diff score
crit_dists_df_diffs <- crit_dists_df %>%
  spread(gender, mean_cosine_distance) %>%
  rename(female_score = "F",
         male_score = "M") %>%
  mutate(gender_diff_score = female_score - male_score)

write_csv(crit_dists_df_diffs, OUTFILE)


