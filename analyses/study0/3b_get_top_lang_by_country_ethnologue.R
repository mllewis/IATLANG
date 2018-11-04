## (3) Get percentage language spoken in each count from Ethnologue (from Gary)
# then top lang (top_lang_by_country.csv) 

library(tidyverse)
library(lingtypology)
library(here)
library(janitor)

# infile
COUNTRY_DF_IN <- here("data/study0/processed/by_country_df.csv")
LANGKEY_PATH <- here("data/study0/processed/lang_name_to_wiki_iso.csv")

LANG_IN <- here("data/study0/raw/Table_of_LICs.txt")

# outfile
OUTFILE <- here("data/study0/processed/top_lang_by_country_ethnologue.csv")

############
iso_country_codes <- read_csv(COUNTRY_DF_IN) 
langs <- read_tsv(LANG_IN) %>%
  clean_names() %>%
  select(country_code, language_name, is_primary, l1_users, family)  %>%
  group_by(country_code) %>%
  top_n(1, l1_users)

unique_langs_per_country <- iso_country_codes %>%
  left_join(langs, by = "country_code") %>%
  data.frame() %>%
  select(country_code, country_name, language_name, family)

unique_langs_per_country_tidy <- unique_langs_per_country %>%
  mutate(language_name =  case_when( language_name == "Javanese" ~ "Indonesian",
                                     language_name == "Bavarian" ~ "German",
                                     language_name == "Dari"~ "Persian",
                                     TRUE ~ language_name)) %>%
  rowwise() %>%
  mutate(language_name = str_split(language_name, ",")[[1]][1])

lang_key <- read_csv(LANGKEY_PATH) 

unique_langs_per_country_tidy_with_wiki_names <- unique_langs_per_country_tidy %>%
  left_join(lang_key) %>%
  distinct()

write_csv(unique_langs_per_country_tidy_with_wiki_names, OUTFILE)


