## scrape wikipedia for most frquent names by language

library("rvest")
library(tidyverse)
url <- "https://en.wiktionary.org/wiki/Appendix:Most_popular_given_names_by_country"


get_wiki_data <- function(num){
  
  path = paste0('//*[@id="mw-content-text"]/div/table[', num, ']')
  data <- url %>%
    read_html() %>%
    html_nodes(xpath=path) %>%
    html_table()
  
  data[[1]] %>%
    mutate(gender = ifelse(num %% 2 == 0, "female", "male"))
}

all_names <- map_df(1:6, get_wiki_data)

names_clean <- all_names %>%
  rename(country = `Region (year)`) %>%
  rowwise() %>%
  mutate(country = unlist(str_split(country, "\n"))[[1]]) %>%
  select(1:12) %>%
  gather(num, name, c(-1,-12)) %>%
  select(-num) %>%
  arrange(country, gender) 

names_clean_recoded <-   names_clean %>%
  mutate(country = fct_recode(country,
                                   "Philippines" = "Philippines [4]",
                                   "United Kingdom" = "England and Wales",
                                    "Australia" = "New South Wales, Australia"))
  
countries_to_langs <- read_csv("data/languages_with_percent.csv")

all_names_tidy <- countries_to_langs %>%
  distinct(country_name) %>%
  left_join(names_clean_recoded, by = c("country_name"= "country")) %>% 
  filter(!is.na(name)) %>%
  group_by(gender, country_name) %>%
  slice(1:9)

write_csv(all_names_tidy, "all_tidy_names.csv")

sampled_names <- all_names_tidy %>%
  left_join(countries_to_langs) %>%
  group_by(wiki_language_code, gender) %>%
  sample_n(9) %>%
  rename(target_word = name) %>%
  select(wiki_language_code, gender, target_word)  %>%
  filter(!is.na(wiki_language_code))

write_csv(sampled_names, "data/all_tidy_names_sampled.csv")


