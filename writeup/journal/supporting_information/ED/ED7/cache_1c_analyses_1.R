# pre-registered (https://osf.io/3f9ed/) Study 1c analyses, cached for SM
library(tidyverse)
library(here)
library(broom)
library(langcog)

EXPLORATORY_PATH <- here("data/study1c/processed/long_form_EXPLORATORY_behavior_and_language.csv")
CONFIRMATORY_PATH <- here("data/study1c/processed/long_form_confirmatory_behavior_and_language.csv")
OUTFILE <- here("writeup/journal/supporting_information/ED/ED7/ED7_data_1.csv")

es_behavior_tidy <- read_csv(CONFIRMATORY_PATH)   %>%
  mutate(data_type = "confirmatory") %>%
  bind_rows(read_csv(EXPLORATORY_PATH) %>% mutate(data_type = "exploratory")) %>%
  select(-user_id)

#**Difference in word frequency for words in each IAT between corpora.**
FREQUENCY_PATH <- here("data/study1c/processed/iat_word_freq_difference_5.csv")
freq_tidy <- read_csv(FREQUENCY_PATH)


pairwise_means <- es_behavior_tidy %>%
  group_by(data_type, residence, domain, run) %>%
  summarize_if(is.numeric, mean) %>%
  spread(residence, behavioral_effect_resid) %>%
  rename(us_behavior_es = us,
         uk_behavior_es = uk) %>%
  mutate(behavioral_resid_diff = uk_behavior_es - us_behavior_es) %>%
  left_join(freq_tidy)

### All IATs
## simple correlations
get_correlations <- function(current_df){
  cor1 <- tidy(cor.test(current_df$coca_lang_es, current_df$us_behavior_es)) %>%
    mutate(corr = "coca_us")
  cor2 <- tidy(cor.test(current_df$coca_lang_es, current_df$uk_behavior_es)) %>%
    mutate(corr = "coca_uk")
  cor3 <- tidy(cor.test(current_df$bnc_lang_es, current_df$us_behavior_es)) %>%
    mutate(corr =  "bnc_us")
  cor4 <- tidy(cor.test(current_df$bnc_lang_es, current_df$uk_behavior_es)) %>%
    mutate(corr = "bnc_uk")
  cor5 <- tidy(cor.test(current_df$bnc_lang_es, current_df$coca_lang_es)) %>%
    mutate(corr = "coca_bnc_corr")
  cor6 <- tidy(cor.test(current_df$uk_behavior_es, current_df$us_behavior_es)) %>%
    mutate(corr = "uk_us_corr")
  cor7 <- tidy(cor.test(current_df$behavioral_resid_diff, current_df$lang_diff)) %>%
    mutate(corr = "diff_corr")

  bind_rows(list(cor1, cor2, cor3, cor4, cor5, cor6, cor7)) %>%
    select(corr, estimate, statistic, p.value,
           parameter, conf.low, conf.high) %>%
    list()
}

# get correlation for each run
all_corrs <- pairwise_means %>%
  group_by(data_type, run) %>%
  nest() %>%
  mutate(model = map(data, ~ get_correlations(.))) %>%
  select(-data) %>%
  unnest() %>%
  unnest()

mean_all_corrs <- all_corrs %>%
  group_by(corr, data_type) %>%
  multi_boot_standard(col = "estimate") %>%
  ungroup()

mean_all_corrs_tidy <- mean_all_corrs %>%
  mutate(corr =
           fct_recode(corr,
                      "UK Language Bias ~ UK Behavioral Bias" = "bnc_uk",
                      "UK Language Bias ~ US Behavioral Bias" = "bnc_us",
                      "US Language Bias ~ UK Language Bias" = "coca_bnc_corr",
                      "US Language Bias ~ UK Behavioral Bias" = "coca_uk",
                      "US Language Bias ~ US Behavioral Bias" = "coca_us",
                      "UK Behavioral Bias ~ US Behavioral Bias" = "uk_us_corr",
                      "Behavioral Bias Difference ~ Language Bias Difference" = "diff_corr"))

write_csv(mean_all_corrs_tidy, OUTFILE)
