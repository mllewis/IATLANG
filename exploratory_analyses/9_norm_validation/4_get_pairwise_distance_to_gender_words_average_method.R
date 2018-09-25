library(tidyverse)
library(data.table)


MODEL_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec"
MALE_WORDS <- c("son", "his","him","he", "brother","boy", "man", "male")
FEMALE_WORDS <- c("daughter", "hers", "her", "she",  "sister", "girl", "woman", "female")

GENDER_NORMS <- "GlasgowNorms.csv"
OUTFILE  <- "embedding_gender_bias_average_method.csv"

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
  mutate(word =  str_split(word, " ", simplify = T)[1]) %>%
  distinct()

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
  mat <- all_words_df %>%
    filter((word == this_word & type == "glasgow")| type == "target")
  
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




