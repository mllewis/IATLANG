## (3) Get percentage language spoken in each country from CIA factbook, 
# then top lang (top_lang_by_country.csv) 

library(tidyverse)
library(jsonlite)
library(lingtypology)
library(here)

# infile
COUNTRY_DF_IN <- here("data/study0/processed/by_country_df.csv")
MIN_PROP <- .2 # min percentage of country speaking langauge

# outfile
OUTFILE <- here("data/study0/processed/top_lang_by_country.csv")

############

# these data are from https://github.com/opendatajson/factbook.json
get_language_info <- function(country_code){
  d <- read_json(here(paste0("data/study0/raw/factbook_json/", country_code, ".json")))
  lang_info <- d$`People and Society`$Languages$text
  name <- d$Government$`Country name`$`conventional short form`$text
  
  data.frame(fips_country_code = country_code,
             country_name = name,
             lang_info = lang_info)
}

iso_country_codes <- read_csv(COUNTRY_DF_IN)  %>%
  filter(country_code != "GB")

distinct(iso_country_codes$country_code)

fips_country_codes <- tolower(countrycode::countrycode(pull(iso_country_codes, country_code),
                                                       "iso2c", "fips"))
country_info <- map_df(c(fips_country_codes, "uk"), get_language_info)

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
  
# read in cleaned percent_language
clean_percentage_language <- percent_language %>%
  select(fips_country_code, country_name, lang, percent) %>%
  filter(!is.na(lang)) %>%
  mutate(lang_clean = str_replace(lang, "\\(o.*", ""),
         lang_clean = str_trim(lang_clean),
         percent_clean = ifelse(is.na(percent), 100, percent),
         prop_language = percent_clean/100) %>%
  select(-lang, -percent, -percent_clean) 

filtered_percent_language <- clean_percentage_language %>%
  filter(prop_language > MIN_PROP) %>%
  arrange(country_name) 

iso_country_codes_with_fips <- iso_country_codes %>%
  select(country_code) %>%
  mutate(fips_country_code = fips_country_codes)  %>%
  add_row(fips_country_code = "uk", country_code = "GB")

# tidy
filtered_percent_language_tidy <- filtered_percent_language %>%
  filter(lang_clean != "spoken by approximately  of the population as a first or second language in ; mainly spoken in areas along the western coast)") %>%
  mutate(lang_clean = ifelse(lang_clean == "English (de facto official)", "English", lang_clean),
         lang_clean = ifelse(lang_clean == "Castilian Spanish", "Spanish", lang_clean),
         lang_clean = ifelse(lang_clean == "Bokmal Norwegian", "Norwegian", lang_clean),
         lang_clean = ifelse(lang_clean == "Bahasa Indonesia", "Indonesian", lang_clean),
         lang_clean = ifelse(lang_clean == "Bahasa Malaysia", "Malaysian", lang_clean),
         lang_clean = ifelse(lang_clean == "Spanish only", "Spanish", lang_clean),
         lang_clean = ifelse(lang_clean == "Afghan Persian or Dari", "Persian", lang_clean),
         lang_clean = ifelse(lang_clean == "Cantonese", "Chinese", lang_clean),
         lang_clean = ifelse(lang_clean == "Standard Chinese or Mandarin", "Chinese", lang_clean),
         lang_clean = ifelse(lang_clean == "Mandarin Chinese", "Chinese", lang_clean),
         lang_clean = ifelse(lang_clean == "Filipino" ,"Tagalog", lang_clean)) %>%
  left_join(iso_country_codes_with_fips) %>%
  select(country_code, fips_country_code, country_name, lang_clean, prop_language) %>%
  rename(language_name = lang_clean)

# get one language per country
unique_langs_per_country <- filtered_percent_language_tidy %>%
  group_by(country_code) %>%
  arrange(-prop_language)  %>%
  slice(1) %>%
  select(country_code, language_name) 


write_csv(unique_langs_per_country, OUTFILE)


get_lang <- function(cn){
  lang = lang.country(cn, official = T)[1] 
  
  data.frame(country_name = cn,
             lang= lang)
  
}

wals <- map_df(iso_country_codes$country_name, 
               get_lang) %>%
  rename(language_nameWALS= lang) %>%
  mutate(language_nameWALS = ifelse(is.na(language_nameWALS, ))

unique_langs_per_country %>%
  select(country_name, language_name) %>%
  rename(language_nameCIA = language_name) %>%
  full_join(wals) %>%
  filter(language_nameCIA != language_nameWALS)


