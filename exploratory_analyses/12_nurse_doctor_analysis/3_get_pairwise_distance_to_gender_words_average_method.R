# get gender bias score for each word using same method used in Caliskan
# in english, using both corpora (sub or wiki)

library(tidyverse)
library(data.table)
library(here)

MALE_WORDS <- c("son", "his","him","he", "brother","boy", "man", "male") 
FEMALE_WORDS <- c("daughter", "hers", "her", "she",  "sister", "girl", "woman", "female")
ANCHOR_PATH <- here("data/study1b/subt_subsetted_models/calculated/")
OCCUPATION_PATH <- here("exploratory_analyses/12_nurse_doctor_analysis/data/subt_subsetted_models/calculated/")
OUTFILE  <- "data/gender_bias_by_word_occupation_sub.csv"

anchor_words <- map_df(list.files(ANCHOR_PATH, full.names = T), ~ read_csv(.)) %>%
  filter(word %in% c(MALE_WORDS, FEMALE_WORDS)) %>%
  mutate(type = "anchor",
         gender = ifelse(word %in% MALE_WORDS, "male", "female"))
occupation_words <- map_df(list.files(OCCUPATION_PATH, full.names = T), ~ read_csv(.)) %>%
  mutate(type = "occupation")


all_words <- bind_rows(anchor_words, occupation_words)  %>%
  select(language_code, word, type, gender, everything())

get_gender_score <- function(this_lang, this_word, all_words_df){
  print(this_word)
  mat <- all_words_df %>%
    filter((word == this_word & type == "occupation")| type == "anchor") %>%
    filter(language_code == this_lang) %>%
    mutate(word = case_when(word == this_word ~ paste0(word, "_", gender),
                            TRUE ~ word))
  
  word_word_dists <- coop::cosine(t(as.matrix(mat[,c(-1:-4)])))
  
  wide_word_word_dists <- word_word_dists %>%
    as.data.frame()  %>%
    mutate(word1 =  mat$word,
           gender1 = mat$gender) %>%
    select(word1,gender1, everything())
  
  names(wide_word_word_dists)  = c("word1", "gender1", wide_word_word_dists$word1)
  
  long_word_word_dists_female <- wide_word_word_dists %>%
    filter(!grepl("^male_form", gender1)) %>% # get rid of male row
    select(-contains("_male_")) %>% # get rid of male column
    filter(gender1 != "male") %>%
    gather("word2", "cos_dist", -word1, -gender1) %>%
    select(word1, word2, gender1, everything()) %>%
    filter(grepl( "form",word2) & !grepl( "form",word1)) %>%
    mutate(word2_type = "female_occupation_translation") %>%
    select(word1, gender1, word2, word2_type, cos_dist) %>%
    arrange(gender1)
  
  long_word_word_dists_male <-  wide_word_word_dists %>%
    filter(!grepl("female_form", gender1)) %>% # get rid of female row
    select(-contains("_female_")) %>% # get rid of female column
    filter(gender1 != "female") %>%
    gather("word2", "cos_dist", -word1, -gender1) %>%
    select(word1, word2, gender1, everything()) %>%
    filter(grepl( "form",word2) & !grepl( "form",word1)) %>%
    mutate(word2_type = "male_occupation_translation") %>%
    select(word1, gender1, word2, word2_type, cos_dist) %>%
    arrange(gender1)
  
  long_word_word_dists <- bind_rows(long_word_word_dists_female, 
                                    long_word_word_dists_male)
  
  try({
    long_word_word_dists %>%
      group_by(gender1,word2_type)%>%
      summarize(mean_cos_dist = mean(cos_dist)) %>%
      spread(gender1, mean_cos_dist)  %>%
      summarize(word = this_word,
                female_target =  mean(female, na.rm = T),
                male_target = mean(male, na.rm = T),
                male_score = male_target - female_target) %>%
      mutate(language_code = this_lang) %>%
      select(word, language_code, everything())
  })
}

all_pairs <- cross_df(list(language = unique(occupation_words$language_code),
         word = unique(occupation_words$word)))

# this is slow....
crit_dists <- map2(all_pairs$language,
                  all_pairs$word,
                  get_gender_score, 
                  all_words)

crit_dists_df <- keep(crit_dists, ~length(.) > 1) %>%
  bind_rows() 

write_csv(crit_dists_df, OUTFILE)

# num forms
occupation_words %>%
  select(language_code, word, V2) %>%
 distinct() %>%
  count(language_code) %>%
  arrange(n)



