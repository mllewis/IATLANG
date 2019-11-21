# identify missing words from translations

library(tidyverse)
library(here)

PATH_SUBT  <-  here("data/study2/subt_calculated_models/")
PATH_WIKI <-  here("data/study2/wiki_calculated_models/")


all_files_subt <- map_df(fs::dir_ls(PATH_SUBT), read_csv) %>%
  select(1:4) %>%
  mutate(model = "subt")

all_files_wiki <- map_df(fs::dir_ls(PATH_WIKI), read_csv) %>%
  select(1:4) %>%
  mutate(model = "wiki")

n_absent <- all_files_subt %>%
  bind_rows(all_files_wiki) %>%
  filter(is.na(V2)) %>%
  count(model, language_code) %>%
  arrange(model, -n) %>%
  rename(n_absent = n)

n_present <- all_files_subt %>%
  bind_rows(all_files_wiki) %>%
  filter(!is.na(V2)) %>%
  count(model, language_code) %>%
  arrange(model, -n) %>%
  rename(n_present = n)

all <- full_join(n_absent, n_present) %>%
  arrange(model)

# ja, zh, hr (wiki)

n_absent %>%
  filter(language_code %in% c("ja", "zh", "hr")) %>%
  select(1:3)
  data.frame()