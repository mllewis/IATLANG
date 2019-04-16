# tidy dfs for sharing
library(tidyverse)
library(here)

OUTFILE_P <- here("writeup/journal/data_for_pre_review/by_participant_df_tidy.csv")
OUTFILE_C <- here("writeup/journal/data_for_pre_review/by_country_df_tidy.csv")
OUTFILE_L <- here("writeup/journal/data_for_pre_review/by_language_df_tidy.csv")
OUTFILE_CL <- here("writeup/journal/data_for_pre_review/by_country_with_language_df_tidy.csv")

#### participant df ####
PARTICIPANT_DF <- here("data/study0/processed/by_participant_df.csv")
iat_behavioral_es_participant <- read_csv(PARTICIPANT_DF) %>%
  select(-education)
write_csv(iat_behavioral_es_participant, OUTFILE_P)

#### country df ####
COUNTRY_BEHAVIORAL_IAT_PATH <- here("data/study0/processed/by_country_df.csv")
iat_behavioral_es_country <- read_csv(COUNTRY_BEHAVIORAL_IAT_PATH) %>%
  select(country_code, country_name, n_participants,  es_iat_sex_age_order_explicit_resid, es_iat_sex_age_order_implicit_resid ,median_country_age, per_women_stem_2012_2017)
write_csv(iat_behavioral_es_country, OUTFILE_C)


#### language df ####
LANGUAGE_BEHAVIORAL_IAT_PATH <- here("data/study0/processed/by_language_df.csv")
iat_behavioral_es <- read_csv(LANGUAGE_BEHAVIORAL_IAT_PATH) %>%
  rename(language_code = "wiki_language_code") %>%
  select(language_code, median_country_age, 
         es_iat_sex_age_order_explicit_resid,
         es_iat_sex_age_order_implicit_resid, 
         per_women_stem_2012_2017, 
         n_participants)

# Study 1
LANG_IAT_PATH <- here("data/study1b/iat_es_lang.csv")
iat_lang_es <- read_csv(LANG_IAT_PATH)

LANG_FAMILY_PATH <- here("data/study0/processed/top_lang_by_country_ethnologue.csv")
lang_family <- read_csv(LANG_FAMILY_PATH) %>%
  select(wiki_language_code, family) %>%
  rename(language_code = "wiki_language_code") %>%
  distinct()

# Study 2 
BY_LANGUAGE_OCCUPATION_PATH  <- here("data/study2/occupation_gender_score_by_language.csv")
occupation_semantics <- read_csv(BY_LANGUAGE_OCCUPATION_PATH)  %>%
  select(-contains("mf"))

OCCUPATION_OVERLAP_PATH <- here('data/study2/occupation_gender_scores.csv')
by_lang_scores <- read_csv(OCCUPATION_OVERLAP_PATH)

LANGUAGE_NAME_PATH <- here("data/study0/processed/lang_name_to_wiki_iso.csv")
language_names <- read_csv(LANGUAGE_NAME_PATH) %>%
  rename(language_code = wiki_language_code) %>%
  distinct(language_code, .keep_all = TRUE)

#Merge data together
all_es <- left_join(iat_behavioral_es, iat_lang_es, by = "language_code") %>%
  left_join(lang_family)   %>%
  left_join(occupation_semantics) %>% 
  left_join(by_lang_scores) %>%
  left_join(language_names) %>%
  select(language_code,language_name,family,n_participants,es_iat_sex_age_order_implicit_resid, es_iat_sex_age_order_explicit_resid, median_country_age, per_women_stem_2012_2017, lang_es_sub, lang_es_wiki, mean_prop_distinct_occs, subt_occu_semantics_fm, wiki_occu_semantics_fm)

#Deal with exclusions, indentified in `analysis/study1b/07_get_prop_iat_words_missing_by_lang.R`.
EXCLUSIONS_PATH <- here("data/study1b/language_exclusions.csv")
exclusions <- read_csv(EXCLUSIONS_PATH)

# Croatian in the wikipedia models exists in both the latin and cyrillic script. Take the average of these two scripts.
hr_new_wiki <- mean(c(filter(iat_lang_es, language_code == "hr") %>%  pull(lang_es_wiki),
                      filter(iat_lang_es, language_code == "sr") %>%  pull(lang_es_wiki)))

all_es_tidy <- all_es %>%
  left_join(exclusions) %>%
  mutate(lang_es_wiki = case_when(exclude_wiki == TRUE ~ NA_real_,
                                  TRUE ~ lang_es_wiki),
         lang_es_sub = case_when(exclude_sub == TRUE ~ NA_real_,
                                 TRUE ~ lang_es_sub)) %>%
  select(-exclude_wiki, -exclude_sub) %>%
  mutate(lang_es_wiki = case_when(language_code == "hr" ~ hr_new_wiki,
                                  TRUE ~ lang_es_wiki),
         lang_es_sub = case_when(language_code == "hr" ~ NA_real_, # sr is not present in from sub
                                 TRUE ~ lang_es_sub))   %>%
  filter(language_code != "zu") 

write_csv(all_es_tidy, OUTFILE_L)

## language 2

LANGUAGE_COUNTRY_IN <- here("data/study0/processed/top_lang_by_country_ethnologue.csv")
unique_langs_per_country <- read_csv(LANGUAGE_COUNTRY_IN)

# average across countries speaking the same language
behavioral_means_by_language <- iat_behavioral_es_country %>%
  left_join(unique_langs_per_country) %>%
  rename(language_code = wiki_language_code) %>%
  left_join(all_es_tidy %>% select(language_code, 
                                   contains("lang_"),
                                   "mean_prop_distinct_occs",
                                   contains("semantics"))) %>%
  ungroup()

# outfile
write_csv(behavioral_means_by_language, OUTFILE_CL)

