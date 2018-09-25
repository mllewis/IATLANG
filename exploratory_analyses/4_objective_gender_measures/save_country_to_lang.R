library(tidyverse)

# get country to lang mappings
countries_to_langs <- read_csv("data/other/languages_with_percent.csv") %>% # this comes from get_language_percentage.R
  mutate(country_name = fct_recode(country_name,
                                   "United States of America"= "United States", 
                                   UK = "United Kingdom",
                                   "Russian Federation" = "Russia",
                                   "Republic of Korea" = "South Korea"),
         country_code = fct_recode(country_code, UK = "GB"), 
         wiki_language_code = fct_recode(wiki_language_code,
                                         "zh"= "zh_yue")) # Cantonese isn't in gtranslate

# get one language per country
unique_langs_per_country <- countries_to_langs %>%
  group_by(country_name) %>%
  arrange(-prop_language)  %>%
  slice(1) %>%
  filter(!language_name %in% c("Niuean", "Cantonese")) %>%
  select(country_code, country_name, wiki_language_code)

write_csv(unique_langs_per_country, "data/other/country_to_lang.csv")
