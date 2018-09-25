library(tidyverse)
library(data.table)


MODEL_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec"
CENTROID_PATH <- "gender_centroids_wiki_english.csv"
GENDER_NORMS <- "GlasgowNorms.csv"
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

glasgow_norms <- read_csv(GENDER_NORMS) %>%
  select(word) %>%
  rowwise() %>%
  mutate(word =  str_split(word, " ", simplify = T)[1]) %>%
  distinct()

#  select(word, GEND_M) %>%
#  rename(gender_norm = GEND_M) 

glasglow_word_coordinates <-  glasgow_norms %>%
  left_join(model) %>%
  bind_rows(centroids) 

word_word_dists <- coop::cosine(t(as.matrix(glasglow_word_coordinates[,-1]))) # this is fast

wide_word_word_dists <- word_word_dists %>%
  as.data.frame()  %>%
  mutate(word1 =  glasglow_word_coordinates$word) %>%
  select(word1, everything())

names(wide_word_word_dists)  = c("word1", glasglow_word_coordinates$word)

long_word_word_dists <- gather(wide_word_word_dists, "word2", "cos_dist", -word1) %>%
  select(word1, word2, everything())

crit_dists <- long_word_word_dists  %>%
  filter(word1 %in% c("male_targets", "female_targets")) %>%
  filter(!is.na(cos_dist)) %>%
  spread(word1, cos_dist) %>%
  mutate(male_score = male_targets - female_targets)

write_csv(crit_dists, OUTFILE)




