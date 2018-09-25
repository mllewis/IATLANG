# EPI analysis

library("rvest")
library(tidyverse)

```{r, EPI, eval = F}
# FIX ME
epi <- read_csv("analysis/study1/data/epi.csv") %>%
  rename(ep_index = `2017 Score`) %>%
  mutate(country_name = str_sub(country_name, 2, -1)) %>%
  select(country_name, ep_index) %>%
  left_join(countries_to_langs)

k = all_hand_es %>%
  left_join(countries_to_langs) %>%
  select(language_name, country_name, es_hand_translation) %>%
  distinct(country_name, es_hand_translation) %>%
  group_by(country_name)   %>%
  summarize()


m = country_means_career_implicit %>%
  left_join(epi) %>%
  mutate(ep_index = ifelse(country_name %in% c("Australia", "Canada", "New Zealand", "UK", "United States of America"), 100, ep_index)) %>%
  left_join(country_means_career_implicit_with_index, by = "country_name") %>%
  left_join(k, by = "country_name") %>%
  distinct(country_name, es_behavioral_iat.x, wps_index, ep_index, es_hand_translation)

cor.test(m$es_behavioral_iat, m$ep_index)

cor.test(m$ep_index, m$wps_index)

cor.test(all_hand_es$epi_weighted, all_hand_es$es_hand_translation)

mod = lm(es_behavioral_iat_weighted ~ es_hand_translation + epi_weighted, data = all_hand_es)
summary(mod)







```
