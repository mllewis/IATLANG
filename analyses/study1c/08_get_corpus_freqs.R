# get difference in word frequency for coca and bnc corpora for target words
# load packages 
library(tidyverse)
library(here)

# set params
CORPUS_PATH1 <- here("data/study1c/raw/COCAshort_words.txt") #
CORPUS_PATH2 <- here("data/study1c/raw/BNCspokenFormatted.txt")#  
TARG_WORDS <- here("data/study1c/processed/all_target_words_5.csv")
STIM_PATH <- here("data/study1c/processed/category_attribute_pair_stim.csv")
OUTFILE <- here("data/study1c/processed/iat_word_freq_difference_5.csv")

# get corpus counts
corpus_bnc <- read_lines(CORPUS_PATH2)   %>%
  str_replace("<s>", "") %>%
  str_replace("</s>", "") %>%
  str_split(" ")  %>%
  unlist()

corpus_coca <- read_lines(CORPUS_PATH1)   %>%
  str_replace("<s>", "") %>%
  str_replace("</s>", "") %>%
  str_split(" ")  %>%
  unlist()

word_count_df <- data.frame(corpus = c(rep("bnc",  length(corpus_bnc)), 
                      rep("coca",  length(corpus_coca))),
           word = c(corpus_bnc, corpus_coca))

word_counts <- count(word_count_df, corpus, word) 

# merge with target words
targ_words <- read_csv(TARG_WORDS)

diff_by_word <- word_counts %>%
  right_join(targ_words, by = c("word" = "stim_name")) %>%
  spread(corpus, n) %>%
  mutate(freq_diff = bnc - coca) 

# get cat-att pairs
domain_eval_set <- read_csv(STIM_PATH)

get_iat_word_freq_difference <- function(this_bias_type, targs, eval_pairs, wcs){
  
  cat_words <- targs %>%
    filter(domain == this_bias_type)
  
  att_words <- eval_pairs %>%
    filter(domain == this_bias_type) %>%
    left_join(targs, by = c("evaluative_label" = "domain")) %>%
    rename(cat_id = evaluative_label)
  
  wcs %>%
    distinct(word, freq_diff) %>%
    right_join(bind_rows(att_words, cat_words), by = c("word" = "stim_name")) %>%
    group_by(domain, cat_id)  %>%
    summarize(mean_freq_diff = mean(freq_diff)) %>%
    group_by(domain) %>%
    summarize(mean_freq_diff = mean(mean_freq_diff))
}

target_domains <- targ_words %>%
  filter(!(cat_id %in% c("good", "bad"))) %>%
  distinct(domain) %>%
  pull(domain)

iat_freq_diff <- map_df(target_domains, 
                     get_iat_word_freq_difference, 
                     targ_words, 
                     domain_eval_set,
                     diff_by_word)

write_csv(iat_freq_diff, OUTFILE)
