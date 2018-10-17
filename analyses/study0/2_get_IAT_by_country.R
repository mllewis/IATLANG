## (2) Get IAT scores by country and merge in objective measures (GGI/per women stem); also saves N participants/country
library(tidyverse)
library(here)

# infile 
PARTICIPANT_DF_IN <- here("data/study0/processed/by_participant_df.csv")
AGE_DATA_PATH <- here("data/study0/raw/median_country_age_world_factbook.csv") #https://www.cia.gov/library/publications/the-world-factbook/rankorder/2177rank.html
OBJECTIVE_MEASURES_DF <- here("data/study0/raw/stoet_data.csv") #http://journals.sagepub.com/doi/10.1177/0956797617741719

# outfile
COUNTRY_DF_OUT <- here("data/study0/processed/by_country_df.csv")


############

# add country median age data
age_data <- read_csv(AGE_DATA_PATH) %>%
  mutate(country_code = countrycode::countrycode(country_name, 'country.name', 'iso2c'))  %>%
  select(-country_name) %>%
  rename(median_country_age = median_age)

iat_behavioral_tidy <- read_csv(PARTICIPANT_DF_IN) %>%
  mutate(education = as.numeric(education)) %>%
  left_join(age_data) 

# mean iat scores by country
behavioral_means_by_country <- iat_behavioral_tidy %>%
  group_by(country_code, country_name) %>%
  summarise_all(mean, na.rm = T) %>%
  rename(prop_male = sex)

# get N participants by country
behavioral_Ns_by_country <- iat_behavioral_tidy %>%
  count(country_code, country_name) %>%
  rename(n_participants = n)

# objective measures by country
stoet_data <- read_csv(OBJECTIVE_MEASURES_DF) 

tidy_objective_by_country <- stoet_data %>% 
  mutate(country_code = countrycode::countrycode(country_name, 'country.name', 'iso2c')) %>%
  select(country_code, everything()) %>%
  select(country_code, ggi, per_women_stem) %>% 
  rename(ggi_stoet = ggi)

# make country df
country_df <- behavioral_means_by_country %>%
  left_join(tidy_objective_by_country) %>% # note that 10 countries are missing from stoet data
  left_join(behavioral_Ns_by_country)

write_csv(country_df, COUNTRY_DF_OUT)
