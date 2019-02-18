# explore occupation gender scores

library(tidyverse)
library(here)

# get gender score 
INFILE <- here('data/study2/occupation_gender_scores.csv')
by_lang_scores_tidy <- read_csv(INFILE)

### THIS IS JUST MISC (STUFF TO GO IN study2_writeup)
BEHAVIORAL_IAT_PATH <- here("data/study0/processed/by_language_df.csv")
iat_behavioral_es <- read_csv(BEHAVIORAL_IAT_PATH) %>%
  rename(language_code = "wiki_language_code") %>%
  select(language_code, median_country_age, 
         prop_male,log_age, es_iat_sex_age_order_explicit_resid,
         es_iat_sex_age_order_implicit_resid, per_women_stem_2012_2017, n_participants)


THEORETICAL_GRAMMAR_PATH <- here("data/study2/general_gender_by_lang.csv")
theoretical_gender <- read_csv(THEORETICAL_GRAMMAR_PATH)  %>%
  select(language_code, wikipedia_grammar_type) %>%
  mutate(wikipedia_grammar_type2 = ifelse(wikipedia_grammar_type %in%  c("none", "CN"),
                                          "No Gender", 
                                          "Gender")) 

LANG_IAT_PATH <- here("data/study1b/iat_es_lang.csv")
iat_lang_es <- read_csv(LANG_IAT_PATH)


full_df <- full_join(by_lang_scores_tidy, iat_behavioral_es) %>%
  left_join(theoretical_gender) %>%
  left_join(iat_lang_es) %>%
  filter(language_code != "tl") # is this telugu or tagalog?


full_df %>%
  select(language_code, n_participants, mean_prop_overlap_occs, 
         median_country_age, es_iat_sex_age_order_explicit_resid, es_iat_sex_age_order_implicit_resid, lang_es_wiki, lang_es_sub) %>%
  gather("measure", "value", -1:-4) %>%
  ggplot(aes(x = mean_prop_overlap_occs, y = value)) +
  geom_point(aes(size = log(n_participants))) +
  facet_wrap(~measure, scale = "free") +
  geom_smooth(method = "lm", mapping = aes(weight = n_participants)) +
  theme_bw()

# implicit behavioral
cor.test(full_df$es_iat_sex_age_order_implicit_resid, full_df$mean_prop_overlap_occs)

lm(es_iat_sex_age_order_implicit_resid ~ mean_prop_overlap_occs + median_country_age,
   data = full_df) %>%
  summary()

# explicit behavioral
cor.test(full_df$es_iat_sex_age_order_explicit_resid, full_df$mean_prop_overlap_occs)

lm(es_iat_sex_age_order_explicit_resid ~ mean_prop_overlap_occs +  median_country_age,
   data = full_df) %>%
  summary()

# language embeddings

cor.test(full_df$lang_es_wiki, full_df$mean_prop_overlap_occs)
cor.test(full_df$lang_es_sub, full_df$mean_prop_overlap_occs)

lm(es_iat_sex_age_order_implicit_resid ~ mean_prop_overlap_occs +  lang_es_wiki + 
     median_country_age,
   data = full_df) %>%
  summary()


#-> mention difference 


### by gender score - nothing here

INFILE2 <- here('data/study2/occupation_gender_scores_by_quantile.csv')
by_lang_scores_tidy2 <- read_csv(INFILE2)

full_df2 <- full_join(by_lang_scores_tidy2, iat_behavioral_es) %>%
  left_join(theoretical_gender) %>%
  left_join(iat_lang_es)  %>%
mutate(gender_median = case_when(gender_quantile > 2 ~ 2, TRUE ~ 1))


full_df2 %>%
  select(language_code, n_participants, mean_prop_overlap_occs, 
         median_country_age, gender_quantile,gender_median, 
         es_iat_sex_age_order_explicit_resid, es_iat_sex_age_order_implicit_resid,
         lang_es_wiki, lang_es_sub) %>%
  gather("measure", "value", -1:-6) %>%
  ggplot(aes(x = mean_prop_overlap_occs, y = value, color = as.factor(gender_median)), group = as.factor(gender_quantile)) +
  geom_point(aes(size = log(n_participants))) +
  facet_wrap(~measure, scale = "free") +
  geom_smooth(method = "lm", mapping = aes(weight = n_participants)) +
  theme_bw()

lm(es_iat_sex_age_order_implicit_resid ~ mean_prop_overlap_occs*as.factor(gender_median) + median_country_age ,
   data = full_df2) %>%
  summary()

### Random effect
INFILE <- here('data/study2/occupation_translations_tidy.csv')
LANG_CODE_PATH <- here("data/study0/processed/lang_name_to_wiki_iso.csv")


lang_codes <- read_csv(LANG_CODE_PATH) %>%
  mutate(language_name = tolower(language_name))

tidy_occs <- read_csv(INFILE)

wide_occs <- tidy_occs %>%
  group_by(language, occupation, word_form_type) %>%
  nest(translation) %>%
  spread(word_form_type, data)

# get score
by_item_lang_scores <-  wide_occs %>%
  mutate(female_overlap_with_male = map2_dbl(female_form, male_form, function(x,y) {length(intersect(x$translation,
                                                                                                     y$translation))/length(x$translation)}),
         male_overlap_with_female = map2_dbl(female_form, male_form, function(x,y) {length(intersect(x$translation,
                                                                                                     y$translation))/length(y$translation)})) %>%
  rowwise() %>%
  mutate(mean_overlap = mean(c(female_overlap_with_male, male_overlap_with_female))) %>%
  select(language, occupation, mean_overlap) %>%
  mutate(mean_overlap_binary = case_when(mean_overlap > 0 ~ 1, TRUE ~ 0)) %>%
  left_join(lang_codes, by = c("language" = "language_name")) %>%
  rename(language_code = wiki_language_code) %>%
  select(-language) %>%
  filter(!is.na(language_code))
  

full_df3 <- full_join(by_item_lang_scores, iat_behavioral_es) %>%
  left_join(theoretical_gender) %>%
  left_join(iat_lang_es) %>%
  mutate_if(is.character, as.factor) %>%
  filter(!(language_code %in% c("tl"))) # should this be tamil or telugu

library(lme4)

lmer(es_iat_sex_age_order_implicit_resid ~ mean_overlap + 
       (1|occupation), data = full_df3) %>%
  summary()



lmer(es_iat_sex_age_order_implicit_resid ~ mean_overlap +  (1|language_code), data = full_df3) %>%
  summary()



 

