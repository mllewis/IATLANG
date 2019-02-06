# fit mixed effect difference model
library(tidyverse)
library(here)
library(lme4)

BEHAVIORAL_PATH <- here("data/study1c/processed/tidy_behavioral_iat_data.csv")
LANGUAGE_PATH <- here("data/study1c/processed/bnc_vs_coca_es_5.csv")
FREQUENCY_PATH <- here("data/study1c/processed/iat_word_freq_difference_5.csv")
CAT_ATT_PATH <- here("data/study1c/processed/category_attribute_pair_stim.csv")

# read in language data
es_lang_raw <- read_csv(LANGUAGE_PATH) 
es_lang_tidy <- es_lang_raw %>%
  spread(model_source, effect_size) %>%
  mutate(fasttext_5_diff = bnc_fasttext_5 - coca_fasttext_5) %>% # get bnc - coca lang difference
  select(domain, fasttext_5_diff)

# read in behavioral data
es_behavior_tidy <- read_csv(BEHAVIORAL_PATH)

# read in frequency data
freq_tidy <- read_csv(FREQUENCY_PATH)

# reading cat-att data
domain_eval_set <- read_csv(CAT_ATT_PATH)

# join everything together
tidy_iat_data <- es_lang_tidy %>%
  full_join(es_behavior_tidy) %>%
  full_join(freq_tidy) %>%
  left_join(domain_eval_set) 

### THE MODEL ##
ggplot(tidy_iat_data, aes(x = fasttext_5_diff, y = behavioral_resid_diff)) +
  geom_text(aes(label = domain)) +
  geom_smooth(method = "lm") + 
  theme_classic()

m1 <- lmer(behavioral_resid_diff ~ fasttext_5_diff + (1|evaluative_label), 
           data = tidy_iat_data) 

m2 <- lmer(behavioral_resid_diff ~ fasttext_5_diff + mean_freq_diff + (1|evaluative_label), 
           data = tidy_iat_data) 

model_comparision <- anova(m1, m2)
if (model_comparision$`Pr(>Chisq)`[2] < .05){
  summary(m2)
} else {
  summary(m1)
}

