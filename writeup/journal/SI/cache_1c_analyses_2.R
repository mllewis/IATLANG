# Mixed-effect model Study 1c analyses, cached for SM
library(tidyverse)
library(here)
library(lme4)
library(sjPlot)

STUDY1C_DATAPATH <- here("data/study1c/processed/long_form_confirmatory_behavior_and_language.csv") 
MODEL_OUTPATH <- here("writeup/journal/SI/data/1c_mixed_effect_model.rda")
  
full_1c_df <- read_csv(STUDY1C_DATAPATH)

full_1c_tidy <- full_1c_df %>% 
  group_by(user_id, domain, residence) %>% # take the mean across model runs
  summarize_if(is.numeric, mean)

study_1c_model <- full_1c_tidy %>%
  # mutate_at(vars(behavioral_effect_resid, lang_diff), scale) %>%
  mutate(residence = factor(residence, levels = c("us", "uk"))) %>%
  lmer(behavioral_effect_resid ~ residence*lang_diff + (1|user_id) + (1|domain), 
       data = .)

tab_model(study_1c_model, 
                show.p = F, 
                show.stat = T, 
                show.se = T,
                string.se = "SE",
                show.ci = F,
                show.r2 = T,
                file = "study_1c_model_print.txt",
                pred.labels = c("(intercept)", "country (uk)",
                                "language bias difference",
                                "country:language bias difference"))



