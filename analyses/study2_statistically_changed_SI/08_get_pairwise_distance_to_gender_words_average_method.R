# get gender bias score for each word using same method used in Caliskan
# in english, using both corpora (sub or wiki)

library(tidyverse)
library(data.table)
library(here)

MALE_WORDS <- c("son", "his","him", "he", "brother","boy", "man", "male")
FEMALE_WORDS <- c("daughter", "hers", "her", "she",  "sister", "girl", "woman", "female")
ANCHOR_PATH <- here("data/study2_statistically_changed_SI/wiki_statistically_changed_models/occupation_models/")
#feminized_calculated_occupation.csv")
OUTFILE  <-  here("data/study2_statistically_changed_SI/wiki_occupation_gender_score.csv")

all_models <- map_df(list.files(ANCHOR_PATH, full.names = T), ~ read_csv(.x) %>% mutate(model = .x))  %>%
  mutate(word_type = case_when(word %in% MALE_WORDS ~ "anchor_M",
                               word %in% FEMALE_WORDS ~ "anchor_F",
                               TRUE ~ "occupation")) %>%
  select(model, language_code, word, gender, word_type, everything()) %>%
  mutate(model = map_chr(model, ~str_split(., "_es_")[[1]][2]),
         model = map_chr(model, ~str_split(., "_")[[1]][1]))


# functions for doign the thing
get_gender_score_for_one_gender <- function(this_mat, these_anchor_words, target_gender){

  target_mat <- this_mat %>%
    filter(gender == target_gender | word_type == paste0("anchor", "_", target_gender))  %>%
    select(-gender)

  word_word_dists <- coop::cosine(t(as.matrix(target_mat[,c(-1:-3)])))

  wide_word_word_dists <- word_word_dists %>%
    as.data.frame()  %>%
    mutate(word1 =  target_mat$word,
           word_type1 = target_mat$word_type) %>%
    select(word1, word_type1, everything())

  names(wide_word_word_dists)  = c("word1", "word_type1", wide_word_word_dists$word1)

  distances <- wide_word_word_dists  %>%
    filter(word_type1 != "occupation") %>%
    select(-word1, -word_type1) %>%
    summarize_all(mean, na.rm = T) %>%
    gather("occupation", "mean_cosine_distance") %>%
    filter(!(occupation %in% these_anchor_words)) %>%
    mutate(gender = target_gender)
}
get_gender_scores <- function(mat, anchor_words){
  f_distances <- get_gender_score_for_one_gender(mat, anchor_words, "F")
  m_distances <- get_gender_score_for_one_gender(mat, anchor_words, "M")
  bind_rows(f_distances, m_distances) %>%
    select(occupation, gender, mean_cosine_distance)
}

### DO THE THING ##
crit_dists_df <- all_models %>%
  group_by(model) %>%
  nest() %>%
  mutate(temp = map(data, get_gender_scores, c(MALE_WORDS, FEMALE_WORDS))) %>%
  select(-data) %>%
  unnest()

# add diff score
crit_dists_df_diffs <- crit_dists_df %>%
  spread(gender, mean_cosine_distance) %>%
  rename(female_score = "F",
         male_score = "M") %>%
  mutate(gender_diff_score = female_score - male_score) %>%
  filter(occupation %in% c("nurse", "firefighter", "firefighter_netural1", "firefighter_netural2",
                           "nurse_netural1", "nurse_netural2"),
         model %in% c("orig", "orig2", "neuteredx", "neuteredie")) %>%
  arrange(model, occupation) %>%
  filter(!is.na(female_score), !is.na(male_score)) %>%
  ungroup() %>%
  mutate(occupation2 = c("firefighter", "nurse", "firefighter", "nurse", "firefighter", "nurse", "firefighter", "nurse")) %>%
  arrange(occupation2)

data_long <- crit_dists_df_diffs %>%
  select(model, occupation2, female_score, male_score, gender_diff_score) %>%
  gather("measure", "value", -1:-2) %>%
  mutate(model = fct_relevel(model, "orig", "orig2", "neuteredie", "neuteredx"),
         measure = fct_relevel(measure, "female_score", "male_score", "gender_diff_score"))

ggplot(data_long, aes(x = model, y = value, fill = model)) +
  ylab("bias") +
  geom_bar(stat = "identity") +
  facet_grid(occupation2~measure) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_classic() +
  theme(legend.position = 'none')


write_csv(crit_dists_df_diffs, OUTFILE)


