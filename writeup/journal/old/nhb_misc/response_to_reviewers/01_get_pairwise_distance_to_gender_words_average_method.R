# get gender bias score for each word using same method used in Caliskan
# in english, using both corpora (sub or wiki)

library(tidyverse)
library(data.table)
library(here)

MALE_WORDS <- c("son", "his","him","he", "brother","boy", "man") 
FEMALE_WORDS <- c("daughter", "hers", "her", "she",  "sister", "girl", "woman")
GENDER_NORMS <- here("data/study1a/raw/GlasgowNorms.csv")
MODEL_SOURCE <- "wiki_cc" # sub/wiki/wiki_cc

if (MODEL_SOURCE == "sub"){
  MODEL_PATH <- "/Volumes/wilbur_the_great/subtitle_models/sub.en.vec"
  OUTFILE  <- here("data/study1a/processed/gender_bias_by_word_english_sub.csv")
} else if (MODEL_SOURCE == "wiki"){
  MODEL_PATH <- "/Volumes/wilbur_the_great/wiki_models/wiki.en.vec" 
  OUTFILE  <- here("data/study1a/processed/gender_bias_by_word_english_wiki.csv")
} else if (MODEL_SOURCE == "wiki_cc"){
  MODEL_PATH <- "/Volumes/wilbur_the_great/wiki_cc_models/wiki_cc.en.vec" 
  OUTFILE  <- here("data/study1a/processed/gender_bias_by_word_english_wikicc.csv")
}

model <- fread(
  MODEL_PATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("word",
                unlist(lapply(2:301, function(x) paste0("V", x)))))

glasgow_norms <- read_csv(GENDER_NORMS) %>%
  select(word) %>%
  rowwise() %>%
  mutate(word =  str_split(word, " ", simplify = T)[1],
         word = tolower(word)) %>%
  distinct() %>%
  filter(!(word %in% c(MALE_WORDS, FEMALE_WORDS, "gender")))
  
  #group_by(word) %>%
  #summarize(GEND_M = mean(GEND_M)) %>% # take the mean across multiple sense of word 
  #rename(norm_maleness = GEND_M)  

glasglow_word_coordinates <-  glasgow_norms %>%
  left_join(model)  %>%
  mutate(type = "glasgow", 
         gender = NA)

target_word_coordinates <- model %>%
  filter(word %in% c(MALE_WORDS, FEMALE_WORDS)) %>%
  mutate(type = "target",
         gender = ifelse(word %in% MALE_WORDS, "male", "female"))

all_words <- bind_rows(glasglow_word_coordinates, target_word_coordinates)  %>%
  select(word, type, gender, everything())

get_gender_score <- function(this_word, all_words_df){
  print(this_word)
  mat <- all_words_df %>%
    filter((word == this_word & type == "glasgow") | type == "target")
  
  word_word_dists <- coop::cosine(t(as.matrix(mat[,c(-1, -2, -3)])))
  
  wide_word_word_dists <- word_word_dists %>%
    as.data.frame()  %>%
    mutate(word1 =  mat$word,
           gender = mat$gender) %>%
    select(word1,gender, everything())
  
  names(wide_word_word_dists)  = c("word1", "gender", mat$word)
  
  long_word_word_dists <- gather(wide_word_word_dists, "word2", "cos_dist", -word1, -gender) %>%
    select(word1, word2, gender, everything()) %>%
    filter(word2 == this_word & word1 != this_word) 
  
  try({
    long_word_word_dists %>%
      group_by(gender)%>%
      summarize(mean_cos_dist = mean(cos_dist)) %>%
      spread(gender, mean_cos_dist)  %>%
      mutate(male_score = male - female,
             word = this_word) %>%
      rename(female_target = female,
             male_target = male) %>%
      select(word, everything())
  })
}

# this is slow....
crit_dists <- map(glasgow_norms$word, 
                     get_gender_score, 
                     all_words)

crit_dists_df <- keep(crit_dists, ~length(.) > 1) %>%
  bind_rows()

write_csv(crit_dists_df, OUTFILE)

