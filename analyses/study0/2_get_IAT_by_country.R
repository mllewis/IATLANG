## (2) Get IAT scores by country and merge in objective measures (GGI/per women stem); also saves N participants/country
library(tidyverse)

# infile 
PARTICIPANT_DF_IN <- "../../data/study1/processed/by_participant_df.csv"
OBJECTIVE_MEASURES_DF <- "../../data/study1/raw/stoet_data.csv" #http://journals.sagepub.com/doi/10.1177/0956797617741719

# outfile
COUNTRY_DF_OUT <- "../../data/study1/by_country_df.csv"

############

iat_behavioral_tidy <- read_csv(PARTICIPANT_DF_IN) %>%
  mutate(education = as.numeric(education))

# mean iat scores by country
behavioral_means_by_country <- iat_behavioral_tidy %>%
  group_by(country_code, country_name) %>%
  summarise_all(mean, na.rm = T) %>%
  rename(prop_male = sex)

# objective measures by country
stoet_data <- read_csv(OBJECTIVE_MEASURES_DF) 

tidy_objective_by_country <- stoet_data %>% 
  mutate(country_code = countrycode::countrycode(country_name, 'country.name', 'iso2c')) %>%
  select(country_code, everything()) %>%
  select(country_code, ggi, per_women_stem) %>% 
  rename(ggi_stoet = ggi)

# make country df
country_df <- behavioral_means_by_country %>%
  left_join(tidy_objective_by_country) # note that 10 countries are missing from stoet data

write_csv(country_df, COUNTRY_DF_OUT)
