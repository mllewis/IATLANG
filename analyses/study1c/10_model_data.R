# fit linear model on behavioral vs. language effect sizes

library(tidyverse)
library(here)
library(broom)
library(langcog)

BEHAVIORAL_PATH <- here("data/study1c/processed/tidy_behavioral_iat_data.csv")
LANGUAGE_PATH <- here("data/study1c/processed/bnc_vs_coca_es_400_10_x5.csv")
FREQUENCY_PATH <- here("data/study1c/processed/iat_word_freq_difference_5.csv")

### READ IN DATA ### 
# language es (5 runs of each model)
es_lang_raw <- read_csv(LANGUAGE_PATH) 
es_lang_tidy <- es_lang_raw %>%
  spread(model, effect_size) %>%
  rename(coca_lang_es = coca, 
         bnc_lang_es = bnc) %>%
  mutate(lang_diff = bnc_lang_es - coca_lang_es) # get bnc - coca language es difference

# behavioral es 
es_behavioral_raw <- read_csv(BEHAVIORAL_PATH)
es_behavior_tidy <- es_behavioral_raw %>%
  rename(us_behavior_es = us, 
         uk_behavior_es = uk)

# words frequency counts by corpus
freq_tidy <- read_csv(FREQUENCY_PATH)

# join everything together
tidy_iat_data <- es_lang_tidy %>%
  full_join(es_behavior_tidy) %>%
  full_join(freq_tidy) 

### MODEL DATA ###
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
all_corrs <- tidy_iat_data %>%
  group_by(run) %>%
  nest() %>% 
  mutate(model = map(data, ~ get_correlations(.))) %>%
  select(-data) %>%
  unnest() %>%
  unnest() 

# average across runs
mean_all_corrs <- all_corrs %>%
  group_by(corr) %>%
  multi_boot_standard(col = "estimate") %>%
  ungroup() 

ggplot(tidy_iat_data, aes(x = lang_diff, y = behavioral_resid_diff,
                          color = run, group = run)) +
  geom_text(aes(label = domain), size = 2) +
  geom_smooth(method = "lm") + 
  theme_classic()

ggplot(mean_all_corrs, aes(x = corr, y = mean)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_pointrange(aes(ymin = ci_lower, max = ci_upper)) +
  theme_classic()

