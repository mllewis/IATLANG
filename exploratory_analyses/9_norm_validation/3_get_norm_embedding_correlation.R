library(tidyverse)
library(data.table)


GENDER_NORMS <- "GlasgowNorms.csv"
EMBEDDING_BIAS1  <- "embedding_gender_bias.csv"
embedding_ratings1 <- read_csv(EMBEDDING_BIAS1) %>%
  rename(word = word2) %>%
  select(word, male_score) %>%
  rename(male_score_centroid = male_score)

EMBEDDING_BIAS2  <- "embedding_gender_bias_average_method.csv"
embedding_ratings2 <- read_csv(EMBEDDING_BIAS2) %>%
  select(word, male_score) %>%
  rename(male_score_average = male_score)

glasgow_norms <- read_csv(GENDER_NORMS) %>%
  select(word, GEND_M) %>%
  rename(norm_maleness = GEND_M)  %>% # take the mean across multiple sense of word 
  rowwise() %>%
  mutate(word =  str_split(word, " ", simplify = T)[1]) %>%
  distinct() %>%
  group_by(word) %>%
  summarize(norm_maleness = mean(norm_maleness))

all_ratings <- embedding_ratings1 %>%
  left_join(embedding_ratings2) %>%
  left_join(glasgow_norms) %>%
  filter(!is.na(male_score_average)) %>%
  filter(!(word %in% c(MALE_WORDS, FEMALE_WORDS)))
  

cor.test(all_ratings$norm_maleness, all_ratings$male_score_average)
cor.test(all_ratings$norm_maleness, all_ratings$male_score_centroid)
cor.test(all_ratings$male_score_average, all_ratings$male_score_centroid)


ggplot(all_ratings, aes(male_score_centroid,male_score_average )) +
  geom_text(aes(label = word), size = 1) +
  
 # geom_point() +
  geom_smooth(method= "lm")



ggplot(all_ratings, aes(x = norm_maleness, y = male_score)) +
  #geom_point(size = .4) +
  geom_text(aes(label = word), size = 1) +
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








