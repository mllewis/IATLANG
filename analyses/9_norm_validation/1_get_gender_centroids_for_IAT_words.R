library(tidyverse)
library(data.table)


MODEL_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec"
MALE_WORDS <- c("son", "his","him","he", "brother","boy", "man", "male")
FEMALE_WORDS <- c("daughter", "hers", "her", "she",  "sister", "girl", "woman", "female")
OUTFILE <- "gender_centroids_wiki_english.csv"

model <- fread(
  MODEL_PATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("target_word",
                unlist(lapply(2:301, function(x) paste0("V", x)))))

target_words <- data.frame(male_targets = MALE_WORDS,
                           female_targets = FEMALE_WORDS) %>%
  gather("word_type", "target_word")

crit_words <- target_words %>%
  left_join(model) %>%
  select(-target_word)


gender_centroids <- crit_words %>%
  split(.$word_type) %>%
  map(function(x) {
    colMeans(x[,-1]) %>% 
      t() %>% 
      as.data.frame() %>% 
      mutate(word_type = x[1,1])}) %>%
  bind_rows() %>%
  select(word_type, everything())

write_csv(gender_centroids, OUTFILE)

