# get percentage women stem

library(tidyverse)
library(lingtypology)
library(here)
library(janitor)


RAW_PATH <- here("data/study0/raw/EDULIT_DS_06112018192722395.csv")

raw_data <- read_csv(RAW_PATH) %>%
  janitor::clean_names() %>%
  mutate_if(is.character,as.factor) %>%
  select(-time_2, -location, -edulit_ind) %>%
  mutate(indicator_clean = str_split(indicator, "graduating from|\\(%\\)"),
         long_program = unlist(map(indicator_clean, ~.[[2]])),
         full_program = str_split(long_program, ","),
         program = str_trim(unlist(map(full_program, ~.[[1]]))))
         sex = str_trim(unlist(map(full_program, ~.[[2]]))))
         

