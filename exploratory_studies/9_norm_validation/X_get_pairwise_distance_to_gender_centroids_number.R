library(tidyverse)
library(data.table)


MODEL_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec"
CENTROID_PATH <- "gender_centroids_wiki_english.csv"
OUTFILE  <- "embedding_gender_bias"

model <- fread(
  MODEL_PATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("word",
                unlist(lapply(2:301, function(x) paste0("V", x)))))

centroids <- read_csv(CENTROID_PATH) %>%
  rename(word = word_type)


NUM_WORDS<- c("one",  "doctor", "lady", "man", "nurse", "two", "three", "four", "five", "six", "seven", "eight", "nine")

all_word_coordinates <-  model %>%
  filter(word %in% NUM_WORDS) %>%
  bind_rows(centroids) 

word_word_dists <- coop::cosine(t(as.matrix(all_word_coordinates[,-1]))) # this is fast

wide_word_word_dists <- word_word_dists %>%
  as.data.frame()  %>%
  mutate(word1 =  all_word_coordinates$word) %>%
  select(word1, everything())

names(wide_word_word_dists)  = c("word1", all_word_coordinates$word)

long_word_word_dists <- gather(wide_word_word_dists, "word2", "cos_dist", -word1) %>%
  select(word1, word2, everything())

crit_dists <- long_word_word_dists  %>%
  filter(word1 %in% c("male_targets", "female_targets")) %>%
  filter(!is.na(cos_dist)) %>%
  spread(word1, cos_dist) %>%
  mutate(male_score = male_targets - female_targets)

crit_dists <- crit_dists %>%
  select(word2, male_score) %>%
  rename(word = word2) %>%
  filter(word != "male_targets") %>%
  filter(word != "female_targets")  %>%
  arrange(male_score) 


ggplot(crit_dists, aes(y = male_score, x = reorder(word, male_score))) +
  geom_bar(stat = "identity") +
  theme_classic()



