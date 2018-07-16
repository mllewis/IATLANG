library(tidyverse)
library(data.table)


GENDER_NORMS <- "GlasgowNorms.csv"
EMBEDDING_BIAS  <- "embedding_gender_bias.csv"

glasgow_norms <- read_csv(GENDER_NORMS) %>%
  select(word, GEND_M) %>%
  rename(norm_maleness = GEND_M)  %>% # take the mean across multiple sense of word 
  rowwise() %>%
  mutate(word =  str_split(word, " ", simplify = T)[1]) %>%
  distinct() %>%
  group_by(word) %>%
  summarize(norm_maleness = mean(norm_maleness))


embedding_ratings <- read_csv(EMBEDDING_BIAS)

all_ratings <- embedding_ratings %>%
  left_join(glasgow_norms, by = c("word2" = "word"))

ggplot(all_ratings, aes(x = norm_maleness, y = male_score)) +
  geom_point(size = .2) +
  xlab("Glasglow gender norm rating (maleness)") +
  ylab("Word embedding gender score (maleness)") +
  geom_smooth(method = "lm") +
  annotate("text", label = "r = .59", x = 2, y = .2, color = "red") +
  theme_classic()

cor.test(all_ratings$norm_maleness, all_ratings$male_score)

cor.test(all_ratings$female_targets, all_ratings$male_targets)
cor.test(all_ratings$female_targets, all_ratings$norm_maleness)
cor.test(all_ratings$male_targets, all_ratings$norm_maleness)








