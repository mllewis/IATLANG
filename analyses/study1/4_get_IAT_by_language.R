## (4) Get behavioral IAT scores by language
library(tidyverse)

# infile
COUNTRY_DF_IN <- "../../data/study1/processed/by_country_df.csv"
LANGUAGE_COUNTRY_IN <- "../../data/study1/processed/top_lang_by_country.csv"

# outfile
LANGUAGE_DF_OUT <- "../../data/study1/processed/by_language_df.csv"

############

countries_to_langs <- read_csv(LANGUAGE_COUNTRY_IN) # this comes from get_top_lang_by_country.R
behavioral_means_by_country <- read_csv(COUNTRY_DF_IN)

# average across countries speaking the same language, weighting by number of participatns
behavioral_means_by_language <- behavioral_means_by_country %>%
    left_join(unique_langs_per_country) %>%
    ungroup() %>%
    select(-country_code, -country_name) %>%
    group_by(language_name) %>%
    summarise_all(mean, na.rm = T) 

write_csv(behavioral_means_by_language, LANGUAGE_DF_OUT)


  