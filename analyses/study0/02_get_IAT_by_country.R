## (2) Get IAT scores by country and merge in objective measures (GGI/per women stem); also saves N participants/country
library(tidyverse)
library(here)
library(langcog)


print("Get by country DF")

# infile 
PARTICIPANT_DF_IN <- here("data/study0/processed/by_participant_df.csv")
AGE_DATA_PATH <- here("data/study0/raw/median_country_age_world_factbook.csv") #https://www.cia.gov/library/publications/the-world-factbook/rankorder/2177rank.html
OBJECTIVE_MEASURES_DF <- here("data/study0/processed/per_women_stem_by_country.csv") 

# outfile
COUNTRY_DF_OUT <- here("data/study0/processed/by_country_df.csv")


############

# add country median age data
age_data <- read_csv(AGE_DATA_PATH) %>%
  mutate(country_code = countrycode::countrycode(country_name, 'country.name', 'iso2c'))  %>%
  select(-country_name) %>%
  rename(median_country_age = median_age)

iat_behavioral_tidy <- read_csv(PARTICIPANT_DF_IN) %>%
  mutate(education = as.numeric(education),
         country_name = case_when(country_name == "Hong Kong" ~ "China", # hong kong has same code as china
                                  TRUE ~ country_name)) %>%
  left_join(age_data) 

# mean iat scores by country
behavioral_means_by_country <- iat_behavioral_tidy %>%
  group_by(country_code, country_name) %>%
  summarise_all(mean, na.rm = T) %>%
  rename(prop_male = sex)

# raw age by country with CIs (for SM)
raw_age_with_ci <- iat_behavioral_tidy %>%
  mutate(age = exp(log_age)) %>%
  group_by(country_code, country_name) %>%
  multi_boot_standard(col = "age")  %>%
  rename(mean_age = mean,
         age_ci_lower = ci_lower,
         age_ci_upper = ci_upper)

# get N participants by country
behavioral_Ns_by_country <- iat_behavioral_tidy %>%
  count(country_code, country_name) %>%
  rename(n_participants = n)

# objective measures by country
stem_data <- read_csv(OBJECTIVE_MEASURES_DF) 

# make country df
country_df <- behavioral_means_by_country %>%
  left_join(raw_age_with_ci) %>%
  left_join(stem_data) %>% # note that 10 countries are missing from stoet data
  left_join(behavioral_Ns_by_country, by = c("country_code", "country_name"))

write_csv(country_df, COUNTRY_DF_OUT)
