
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
  left_join(iat_lang_es)


full_df %>%
  select(language_code, n_participants, mean_prop_overlap_occs, 
         median_country_age, es_iat_sex_age_order_explicit_resid, es_iat_sex_age_order_implicit_resid, lang_es_wiki, lang_es_sub) %>%
  gather("measure", "value", -1:-4) %>%
  ggplot(aes(x = mean_prop_overlap_occs, y = value)) +
  geom_point(aes(size = log(n_participants))) +
  facet_wrap(~measure, scale = "free") +
  geom_smooth(method = "lm", mapping = aes(weight = n_participants)) +
  theme_bw()


lm(es_iat_sex_age_order_implicit_resid ~ mean_prop_overlap_occs +  median_country_age, weights = n_participants,
   data = full_df) %>%
  summary()


lm(es_iat_sex_age_order_implicit_resid ~ mean_prop_overlap_occs *  median_country_age,
   data = full_df) %>%
  summary()


lm(es_iat_sex_age_order_explicit_resid ~ mean_prop_overlap_occs + lang_es_wiki +  median_country_age,
   data = full_df) %>%
  summary()


cor.test(full_df$lang_es_wiki, full_df$es_iat_sex_age_order_explicit_resid)

lm(es_iat_sex_age_order_explicit_resid ~ mean_prop_overlap_occs +  median_country_age, weights = n_participants,
   data = full_df) %>%
  summary()

lm(lang_es_wiki ~ mean_prop_overlap_occs +  median_country_age, weights = n_participants,
   data = full_df) %>%
  summary()

-> mention difference 


### by gender score

INFILE2 <- here('data/study2/occupation_gender_scores_by_quantile.csv')
by_lang_scores_tidy2 <- read_csv(INFILE2)

full_df2 <- full_join(by_lang_scores_tidy2, iat_behavioral_es) %>%
  left_join(theoretical_gender) %>%
  left_join(iat_lang_es)


full_df2 %>%
  select(language_code, n_participants, mean_prop_overlap_occs, 
         median_country_age, gender_quantile, es_iat_sex_age_order_explicit_resid, es_iat_sex_age_order_implicit_resid, lang_es_wiki, lang_es_sub) %>%
  gather("measure", "value", -1:-5) %>%
  ggplot(aes(x = mean_prop_overlap_occs, y = value, color = as.factor(gender_quantile)), group = as.factor(gender_quantile)) +
  geom_point(aes(size = log(n_participants))) +
  facet_wrap(~measure, scale = "free") +
  geom_smooth(method = "lm", mapping = aes(weight = n_participants)) +
  theme_bw()