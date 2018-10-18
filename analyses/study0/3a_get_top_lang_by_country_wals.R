## (3) Get percentage language spoken in each country Glottolog (lingtypology package)
# then top lang (top_lang_by_country.csv) 

library(tidyverse)
library(lingtypology)
library(here)

# infile
COUNTRY_DF_IN <- here("data/study0/processed/by_country_df.csv")

# outfile
OUTFILE <- here("data/study0/processed/top_lang_by_country.csv")

############
iso_country_codes <- read_csv(COUNTRY_DF_IN) 

get_lang <- function(cn){
  lang = lang.country(cn, official = T)[1] 
  
  data.frame(country_name = cn,
             lang = lang)
  
}

wals <- map_df(iso_country_codes$country_name, 
               get_lang) %>%
  rename(language_nameWALS = lang)

# add in missing languages

# all_langs <- unique_langs_per_country %>%
#  select(country_name, language_name) %>%
#  rename(language_nameCIA = language_name) %>%
#  full_join(wals) %>%
#  filter(language_nameCIA != language_nameWALS)  %>%
#  filter(!(country_code %in% c("CN", "TW", "GR")))

select(country_code, language_name) 


write_csv(unique_langs_per_country, OUTFILE)


