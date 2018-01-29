# EPI analysis

library(tidyverse)

epi <- read_csv("data/epi.csv") %>%
  rename(ep_index = `2017 Score`) %>% 
  mutate(country_name = str_sub(country_name, 2, -1)) %>%
  select(country_name, ep_index)

all_df <- read_csv("../all_measures_df.csv") %>%
  left_join(epi) %>%
  mutate(ep_index = ifelse(language_name %in% c("English", "Hebrew"), 100, ep_index)) # include Israel

# missing Afghanistan, Croatia, Cyprus
cor.test(all_df$ep_index, all_df$es_behavioral_iat)
cor.test(all_df$ep_index, all_df$wps_index)
cor.test(all_df$ep_index, all_df$exp_mean_diff)

mod <- lm(es_behavioral_iat ~ wps_index + ep_index,  data = all_df)
summary(mod)

filter(all_df, !is.na(ep_index) & !is.na(wps_index)) %>% data.frame()

cor.test(all_df$prop_fem, all_df$wps_index)
