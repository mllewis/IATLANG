library(tidyverse)

INFILE <- "data/gender_bias_by_word_occupation_sub.csv"
distances <- read_csv(INFILE)

tidy_dists <- distances %>%
  filter(!is.na(male_score))

tidy_dists %>%
  ggplot(aes(x = male_score)) +
  geom_histogram() +
  facet_wrap(~word)

doc_nurse <- tidy_dists %>%
  filter(word %in% c("doctor/physician", "nurse"))

tidy_dists %>%
  group_by(word) %>%
  multi_boot_standard(col  = "male_score") %>%
  ggplot(aes(x = reorder(word, mean), y = mean, color = word)) +
  geom_pointrange( aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_hline(aes(yintercept = 0)) +
  ylab("Male Embedding Score") +
  xlab("Occupation Name") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none")

ggplot(doc_nurse, aes(x = word, y = male_score, group = language_code, color = word)) +
  geom_line(color = "black") +
  geom_point(size = 2.4) +
  geom_text(data = doc_nurse %>% filter(word == "nurse"), 
                                 aes(x = 2.05, y = male_score, label = language_code), 
            color = "black") +
  ylab("Male Embedding Score") +
  xlab("Occupation Name") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none")
  
tidy_dists %>%
  filter(word %in% c("doctor/physician", "nurse")) %>%
  ggplot(aes(x = word, y = female_target, group = language_code, color = word)) +
  geom_line(color = "black") +
  geom_text(data = doc_nurse %>% filter(word == "nurse"), 
            aes(x = 2.05, y = female_target, label = language_code), 
            color = "black") +
  geom_point(size = 2.4) +
  theme_classic(base_size = 15) +
  theme(legend.position = "none")

tidy_dists %>%
  filter(word %in% c("doctor/physician", "nurse")) %>%
  ggplot(aes(x = word, y = male_target, group = language_code, color = word)) +
  geom_line(color = "black") +
  geom_text(data = doc_nurse %>% filter(word == "nurse"), 
            aes(x = 2.05, y = male_target, label = language_code), 
            color = "black") +
  geom_point(size = 2.4)  +
  theme_classic(base_size = 15) +
  theme(legend.position = "none")