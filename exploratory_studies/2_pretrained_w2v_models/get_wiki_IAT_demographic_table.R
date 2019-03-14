# Get table that has country name/id, language name/iso and wiki_code for countries
# in project implicit data ("country_langiso_langwiki_key.csv")

library(tidyverse)
library(countrycode)
library(lingtypology)
library(feather)
library(data.table)

## Read in project implicit IAT country codebook table
# note! countryres is coded sometimes as a number and sometimes as a 2 digit code 
# countryres_num only contains numbers
# here I rename countyres in the data -> countryres_mixed, and convert to only letter codes
# including the numeric codes adds 121067 participants

pi_countryres_codebook  <- read_csv("../../data/other/IAT_countryres_codebook.csv") 
all_codes_to_country_name <-  pi_countryres_codebook %>%
  mutate(countryres_num = as.character(countryres_num)) %>%
  gather("type", "mixed_countryres", -2) %>%
  select(-type)

country_name_to_letter_code  <- pi_countryres_codebook %>%
    select(country_name, countryres) %>%
    filter(!grepl("\\d", countryres)) 

## Read in project implicit IAT data
d_raw <- read_feather("../../data/IAT/Gender-Career/Gender-Career\ IAT.public.2005-2016.feather")  %>%
  rename(mixed_countryres = countryres)

# get table mapping mixed country codes to letter country codes 
mixed_countryres_to_country_res <- d_raw %>%
      distinct(mixed_countryres, countryres_num) %>%
      data.table() %>%
      merge(all_codes_to_country_name, all.x = TRUE) %>% # num and letter to name
      merge(country_name_to_letter_code, all.x=TRUE, by = "country_name") %>% # name to letter
      mutate_all(as.factor) %>%
      distinct(mixed_countryres, countryres) 

## Write to csv
#write_csv(mixed_countryres_to_country_res, "../../data/other/mixed_countryres_to_country_res_key.csv")

## Merge project implicit IAT country codes with corresponding languages, and get language iso codes
missing_language_coded <- read_csv("../../data/other/missing_language_isos_coded.csv") %>%
  select(country_name, language_name) ## merge in missing links which are hand coded (see below)

demo_data <- mixed_countryres_to_country_res %>%
  distinct(countryres) %>%
  mutate(country_name = countrycode(countryres, "iso2c",   "country.name"),
         country_name = replace(country_name, countryres == "UK", "UK"),
         country_name = replace(country_name, countryres == "TP", "East Timor")) %>%
  rowwise() %>%
  mutate(language_name = unlist(lang.country(country_name, list = T, official = T))[1]) %>%
  left_join(missing_language_coded, by="country_name") %>%
  mutate(language_name = ifelse(is.na(language_name.x), language_name.y, language_name.x),
         language_name = replace(language_name, countryres == "CI", "French")) %>%
  select(-contains(".")) %>%
  mutate(language_iso = iso.lang(language_name),
         language_iso = replace(language_iso, language_name == "Pashto", "pus"),
         language_iso = replace(language_iso, language_name == "Azerbaijani", "aze"),
         language_iso = replace(language_iso, language_name == "Malay", "may"),
         language_iso = replace(language_iso, language_name == "Greenlandic", "kay"),
         language_iso = replace(language_iso, language_name == "Persian", "fas"),
         language_iso = replace(language_iso, language_name == "Hebrew", "heb"),
         language_iso = replace(language_iso, language_name == "Cantonese", "chi"),
         language_iso = replace(language_iso, language_name == "Mongolian", "mon"),
         language_iso = replace(language_iso, language_name == "Albanian", "alb"),
         language_iso = replace(language_iso, language_name == "Slovene", "slv"),
         language_iso = replace(language_iso, language_name == "Arabic", "ara"),
         language_iso = replace(language_iso, language_name == "Chinese", "chi"),
         language_iso = replace(language_iso, language_name == "Tokelau", "tkl"),
         language_iso = replace(language_iso, language_name == "Uzbek", "uzb"))

# missing links
#missing_language_isos <- demo_data %>%
#  filter(is.na(language_iso)) 
#write_csv(missing_language_isos, "../../data/other/missing_language_isos.csv")

## Read in wiki codes
wiki_codes <- read_csv("../../data/other/wikipedia_language_code_key.csv") 

## Merge country and language data with wiki data
demo_with_wiki <- demo_data %>%                          
  left_join(wiki_codes, by = "language_name") %>%
   mutate(wiki_language_code = replace(wiki_language_code, language_name == "Standard Arabic", "ar"),
          wiki_language_code = replace(wiki_language_code, language_name == "Chadian Arabic", "ar"),
          wiki_language_code = replace(wiki_language_code, language_name == "Mandarin Chinese", "zh"),
          wiki_language_code = replace(wiki_language_code, language_name == "Modern Greek", "el"),
          wiki_language_code = replace(wiki_language_code, language_name == "Yue Chinese", "zh"),
          wiki_language_code = replace(wiki_language_code, language_name == "Libyan Arabic", "ar"),
          wiki_language_code = replace(wiki_language_code, language_name == "Plateau Malagasy", "mg"),
          wiki_language_code = replace(wiki_language_code, language_name == "Standard Malay", "ms"),
          wiki_language_code = replace(wiki_language_code, language_name == "Norwegian", "no"),
          wiki_language_code = replace(wiki_language_code, language_name == "Abkhaz", "ab"),
          wiki_language_code = replace(wiki_language_code, language_name == "Northern Tosk Albanian", "sq"),
          wiki_language_code = replace(wiki_language_code, language_name == "Central Khmer", "km"),
          wiki_language_code = replace(wiki_language_code, language_name == "Southern Sotho", "st"),
          wiki_language_code = replace(wiki_language_code, language_name == "Slovene", "sl"),
          wiki_language_code = replace(wiki_language_code, language_name == "Castellano", "es")) %>%
  ungroup() %>%
  mutate_all(as.factor)
          
## Write to csv
write_csv(demo_with_wiki, "../../data/other/country_langiso_langwiki_key.csv")
