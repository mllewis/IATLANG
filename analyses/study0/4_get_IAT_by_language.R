## (4) Get behavioral IAT scores by language
library(tidyverse)

# infile
COUNTRY_DF_IN <- "../../data/study0/processed/by_country_df.csv"
LANGUAGE_COUNTRY_IN <- "../../data/study0/processed/top_lang_by_country.csv"
LANGKEY_PATH <- "../../../data/study1b/country_langiso_langwiki_key.csv"

# outfile
LANGUAGE_DF_OUT <- "../../data/study0/processed/by_language_df.csv"

############

unique_langs_per_country <- read_csv(LANGUAGE_COUNTRY_IN) %>% # this comes from get_top_lang_by_country.R
  mutate(language_name = ifelse(language_name == "Chinese", "Mandarin", language_name)) %>% # collapse Mandarin and Chinese
  left_join(LANGKEY_PATH)

behavioral_means_by_country <- read_csv(COUNTRY_DF_IN)

# average across countries speaking the same language, weighting by number of participatns
behavioral_means_by_language <- behavioral_means_by_country %>%
    left_join(unique_langs_per_country) %>%
    ungroup() %>%
    select(-country_code, -country_name) %>%
    group_by(language_name) %>%
    summarise_all(mean, na.rm = T) 

write_csv(behavioral_means_by_language, LANGUAGE_DF_OUT)

lang_key <- read_csv(KEY_PATH) %>%
  select(language_name2, wiki_language_code) %>%
  filter(!is.na(language_name2)) %>%
  rename(language_name = language_name2)%>%
  distinct() 


  