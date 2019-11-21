# cache df with all measures in it so can provide desscriptive statistics
# (this is the same df as in study1_writeup, with the addition of study 3_SI data)

library(tidyverse)
library(here)

LANG_IAT_PATH <- here("data/study1b/iat_es_lang.csv")
LANG_IAT_NATIVE_PATH <- here("data/study3_SI/study1b_replication/06_iat_es/iat_es_lang.csv")
LANG_FAMILY_PATH <- here("data/study0/processed/top_lang_by_country_ethnologue.csv")
BEHAVIORAL_IAT_PATH <- here("data/study0/processed/by_language_df.csv")
BY_LANGUAGE_OCCUPATION_PATH  <- here("data/study2/occupation_gender_score_by_language.csv")
BY_LANGUAGE_OCCUPATION_PATH_NATIVE <- here("data/study3_SI/study2_replication/occupation_gender_score_by_language_native_wiki.csv")
OCCUPATION_OVERLAP_PATH <- here('data/study2/occupation_gender_scores.csv')
LANGUAGE_NAME_PATH <- here("data/study0/processed/lang_name_to_wiki_iso.csv")
EXCLUSIONS_PATH <- here("data/study1b/language_exclusions.csv")
EXCLUSIONS_WIKI_NATIVE_PATH <- here("data/study3_SI/study1b_replication/06_iat_es/language_exclusions.csv")


OUTFILE <- here("writeup/journal/SI/data/tidy_measures.csv")

iat_lang_es <- read_csv(LANG_IAT_PATH)

iat_lang_es_native <- read_csv(LANG_IAT_NATIVE_PATH) %>%
  rename("lang_es_wiki_native" = "lang_es_wiki")

lang_family <- read_csv(LANG_FAMILY_PATH) %>%
  select(wiki_language_code, family) %>%
  rename(language_code = "wiki_language_code") %>%
  distinct()

iat_behavioral_es <- read_csv(BEHAVIORAL_IAT_PATH) %>%
  rename(language_code = "wiki_language_code") %>%
  select(language_code, median_country_age,
         prop_male,log_age, es_iat_sex_age_order_explicit_resid,
         es_iat_sex_age_order_implicit_resid, per_women_stem_2012_2017, n_participants)

# Study 2 measures (included here for making single grand table)
occupation_semantics <- read_csv(BY_LANGUAGE_OCCUPATION_PATH)
occupation_semantics_native <- read_csv(BY_LANGUAGE_OCCUPATION_PATH_NATIVE)

by_lang_scores <- read_csv(OCCUPATION_OVERLAP_PATH)

language_names <- read_csv(LANGUAGE_NAME_PATH) %>%
  rename(language_code = wiki_language_code) %>%
  distinct(language_code, .keep_all = TRUE)

# combine lang and behavioral and family info
all_es <- left_join(iat_behavioral_es, iat_lang_es, by = "language_code") %>%
  left_join(iat_lang_es_native) %>%
  left_join(lang_family)   %>%
  left_join(occupation_semantics)  %>% # include study 2 measure here so can make table
  left_join(occupation_semantics_native)  %>%

  left_join(by_lang_scores) %>%
  left_join(language_names) %>%
  select(language_code,language_name,family,n_participants,es_iat_sex_age_order_implicit_resid,
         es_iat_sex_age_order_explicit_resid, median_country_age, per_women_stem_2012_2017, lang_es_sub, lang_es_wiki, lang_es_wiki_native,
         mean_prop_distinct_occs, subt_occu_semantics_fm, wiki_occu_semantics_fm, wiki_native_occu_semantics_fm)


# remove exclusions and fix croatian to be mean of hr and sr (only in wiki)
exclusions <- read_csv(EXCLUSIONS_PATH)


hr_new_wiki <- mean(c(filter(iat_lang_es, language_code == "hr") %>%  pull(lang_es_wiki),
                      filter(iat_lang_es, language_code == "sr") %>%  pull(lang_es_wiki)))

exclusions_wiki_native <- read_csv(EXCLUSIONS_WIKI_NATIVE_PATH)
hr_new_wiki_native <- mean(c(filter(iat_lang_es_native, language_code == "hr") %>%  pull(lang_es_wiki_native),
                             filter(iat_lang_es_native, language_code == "sr") %>%  pull(lang_es_wiki_native)))

all_es_tidy <- all_es %>%
  left_join(exclusions) %>%
  left_join(exclusions_wiki_native) %>%
  mutate(lang_es_wiki = case_when(exclude_wiki == TRUE ~ NA_real_,
                                  TRUE ~ lang_es_wiki),
         lang_es_sub = case_when(exclude_sub == TRUE ~ NA_real_,
                                 TRUE ~ lang_es_sub),
         lang_es_wiki_native = case_when(exclude_wiki_native == TRUE ~ NA_real_,
                                         TRUE ~ lang_es_wiki_native)) %>%
  select(-exclude_wiki, -exclude_sub, -exclude_wiki_native) %>%
  mutate(lang_es_wiki = case_when(language_code == "hr" ~ hr_new_wiki,
                                  TRUE ~ lang_es_wiki),
         lang_es_wiki_native = case_when(language_code == "hr" ~ hr_new_wiki_native,
                                         TRUE ~ lang_es_wiki_native),
         lang_es_sub = case_when(language_code == "hr" ~ NA_real_, # sr is missing from sub
                                 TRUE ~ lang_es_sub))  %>%
  filter(language_code != "zu")  # exclude proportion overlap measure (study 2) in zulu

write_csv(all_es_tidy, OUTFILE)
