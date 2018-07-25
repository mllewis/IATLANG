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
  #geom_point(size = .4) +
  geom_text(aes(label = word2), size = 1) +
  xlab("Explicit gender norm rating\n(maleness)") +
  ylab("Word embedding gender score\n (maleness)") +
  ggtitle("Explicit vs. Language-Embedding \nword gender bias") +
  geom_smooth(method = "lm") +
  annotate("text", label = "r = .59", x = 6, y = -.2, 
           color = "red", size = 18) +
  theme_classic() +
  theme(text = element_text(size = 26),
        axis.line = element_line(colour = "black", size = 1.2)) 

cor.test(all_ratings$norm_maleness, all_ratings$male_score)
cor.test(all_ratings$female_targets, all_ratings$male_targets)
cor.test(all_ratings$female_targets, all_ratings$norm_maleness)
cor.test(all_ratings$male_targets, all_ratings$norm_maleness)








