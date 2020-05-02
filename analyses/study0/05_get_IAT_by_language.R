## (5) Get behavioral IAT scores by language
library(tidyverse)
library(here)

print("get IAT by language")

# infile
COUNTRY_DF_IN <- here("data/study0/processed/by_country_df.csv")
LANGUAGE_COUNTRY_IN <- here("data/study0/processed/top_lang_by_country_ethnologue.csv")

# outfile
LANGUAGE_DF_OUT <- here("data/study0/processed/by_language_df.csv")

############

unique_langs_per_country <- read_csv(LANGUAGE_COUNTRY_IN)
behavioral_means_by_country <- read_csv(COUNTRY_DF_IN)

# average across countries speaking the same language
by_lang_df <- behavioral_means_by_country %>%
    left_join(unique_langs_per_country) %>%
    ungroup() %>%
    select(-country_code, -country_name, -language_name)

behavioral_means_by_language <- by_lang_df %>%
    select(-n_participants) %>%
    group_by(wiki_language_code) %>%
    summarise_all(mean, na.rm = T)

n_participants_by_language <- by_lang_df %>%
    group_by(wiki_language_code) %>%
    summarize(n_participants = sum(n_participants))

all_data_by_language <- left_join(behavioral_means_by_language,
                                  n_participants_by_language)

write_csv(all_data_by_language, LANGUAGE_DF_OUT)
