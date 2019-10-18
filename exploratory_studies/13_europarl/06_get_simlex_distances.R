# get simlex distances
library(tidyverse)
library(here)
library(data.table)
library(broom)


SIMLEX_DATA <- here("exploratory_studies/13_europarl/other_data/SimLex-999.txt")
simlex <- read_tsv(SIMLEX_DATA) %>%
  select(word1, word2, SimLex999) %>%
  janitor::clean_names()

UNTRANS_MODEL <- "/Users/mollylewis/Downloads/europarl_models/untrans_europarl_5ep_en.txt.vec"
TRANS_MODEL <- "/Users/mollylewis/Downloads/europarl_models/trans_f_europarl_5ep_en.txt.vec"

# untranslated
current_model <- fread(
  UNTRANS_MODEL,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("word",
                unlist(lapply(2:301, function(x) paste0("V", x))))) %>%
  filter(word %in% c(simlex$word1, simlex$word2))

word_word_dists <- coop::cosine(t(as.matrix(current_model[,-1])))

wide_word_word_dists <- word_word_dists %>%
  as.data.frame()  %>%
  mutate(word1 =  current_model$word) %>%
  select(word1,everything())

names(wide_word_word_dists)  = c("word1", current_model$word)

long_word_word_dists_untranslated <- gather(wide_word_word_dists, "word2", "cos_dist", -word1) %>%
  select(word1, word2, everything())  %>%
  mutate(model = "untranslated")

# translated
current_model <- fread(
  TRANS_MODEL,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("word",
                unlist(lapply(2:301, function(x) paste0("V", x))))) %>%
  filter(word %in% c(simlex$word1, simlex$word2))

word_word_dists <- coop::cosine(t(as.matrix(current_model[,-1])))

wide_word_word_dists <- word_word_dists %>%
  as.data.frame()  %>%
  mutate(word1 =  current_model$word) %>%
  select(word1,everything())

names(wide_word_word_dists)  = c("word1", current_model$word)

long_word_word_dists_translated <- gather(wide_word_word_dists, "word2", "cos_dist", -word1) %>%
  select(word1, word2, everything())  %>%
  mutate(model = "translated")

# wiki
WIKI_MODEL <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/exploratory_analyses/0_exploration/wiki.en.vec"
current_model <- fread(
  WIKI_MODEL,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("word",
                unlist(lapply(2:301, function(x) paste0("V", x))))) %>%
  filter(word %in% c(simlex$word1, simlex$word2))

word_word_dists <- coop::cosine(t(as.matrix(current_model[,-1])))

wide_word_word_dists <- word_word_dists %>%
  as.data.frame()  %>%
  mutate(word1 =  current_model$word) %>%
  select(word1,everything())

names(wide_word_word_dists)  = c("word1", current_model$word)

long_word_word_dists_wiki <- gather(wide_word_word_dists, "word2", "cos_dist", -word1) %>%
  select(word1, word2, everything())  %>%
  mutate(model = "wiki")


simlex_distances <- simlex %>% left_join(long_word_word_dists_untranslated) %>%
  bind_rows(simlex %>% left_join(long_word_word_dists_translated)) %>%
  bind_rows(simlex %>% left_join(long_word_word_dists_wiki)) %>%
  filter(!is.na(cos_dist))

ggplot(simlex_distances, aes(x = cos_dist, y = sim_lex999)) +
facet_wrap(~model) +
  geom_point(alpha = .2) +
  geom_smooth(method = "lm") +
  theme_classic()

simlex_distances %>%
  group_by(model)%>%
  nest() %>%
  mutate(temp = map(data, ~tidy(cor.test(.$sim_lex999, .$cos_dist, method = "pearson")))) %>%
  select(-data) %>%
  unnest() 


