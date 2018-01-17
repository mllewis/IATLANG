## Get percentage language spoken in each country from CIA factbook (languages_with_percent.csv) ##

library(tidyverse)
library(jsonlite)
library(countrycode)
library(lingtypology)

# these data are from https://github.com/opendatajson/factbook.json
get_language_info <- function(country_code){
  d <- read_json(paste0("data/factbook_json/", country_code, ".json"))
  lang_info <- d$`People and Society`$Languages$text
  name <- d$Government$`Country name`$`conventional short form`$text
  
  data.frame(fips_country_code = country_code,
             country_name = name,
             lang_info = lang_info)
}

iso_country_codes <- country_ns$countryres
fips_country_codes <- countrycode(iso_country_codes, "iso2c", "fips105")
fips_country_codes <- c(tolower(fips_country_codes[complete.cases(fips_country_codes)]), "hk", "uk")
country_info <- map_df(fips_country_codes, get_language_info)

# munge country info to get percentages
percent_language <- country_info %>%
  rowwise() %>%
  mutate(lang_info = str_split(lang_info, ",")) %>%
  unnest() %>%
  mutate(lang_info = gsub( "%", "", lang_info)) %>%
  group_by(fips_country_code) %>%
  mutate(n = 1:n(),
         num = grepl("\\d", lang_info)) %>%
  filter(num | n == 1) %>%
  select(-num) %>%
  mutate(percent = as.numeric(str_extract(lang_info, "[0-9]+")),
         lang = gsub('[[:digit:]]+', '', lang_info),
         lang = gsub(".", "", lang, fixed = TRUE)) %>%
  select(fips_country_code, country_name, lang, percent, everything())
  

# write_csv(percent_language, "data/percent_country_language_raw.csv")

# read in cleaned percent_language
clean_percentage_language <- read_csv("data/percent_country_language_clean.csv") %>%
  select(fips_country_code, country_name, lang_clean, percent_clean) %>%
  filter(!is.na(lang_clean)) %>%
  mutate(lang_clean = str_trim(lang_clean),
         prop_language = percent_clean/100) %>%
  select(-percent_clean)

# filter to to languaes spokedn by at least 20% of the country
MIN_PROP <- .20
filtered_percent_language <- clean_percentage_language %>%
  filter(prop_language > MIN_PROP) %>%
  arrange(country_name) 

# wikipedia mappings      
lang_names_to_wiki <- read_csv("data/language_names_to_wiki_codes.csv") %>%
  distinct() # this was coded by hand

languages_with_percent_final <- filtered_percent_language %>%
  mutate(country_code = countrycode(fips_country_code,  "fips105", "iso2c"),
         country_code = ifelse(country_name == "Hong Kong", "HK", country_code))  %>%
  rename(language_name = lang_clean) %>%
  left_join(lang_names_to_wiki, by = "language_name") %>%
  select(country_code, country_name, wiki_language_code, language_name, prop_language)


#write_csv(languages_with_percent_final, "data/languages_with_percent.csv")  
         
