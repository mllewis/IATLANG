## (1) Preprocess Career-Gender IAT data from Project Implicit by participant
library(tidyverse)
library(feather)
library(modelr)
library(here)

# set params
MIN_PARTICIPANTS_PER_COUNTRY <- 400 # after all exclusions

# infile
RAW_IAT_PATH <- here("data/study0/raw/Gender-Career\ IAT.public.2005-2016.feather") # this file is "Gender-Career IAT.public.2005-2016.sav" in feather form (taken from: https://osf.io/gmewy/); it is not in the repository because it is too big.
IAT_COUNTRY_CODES_PATH <- here("data/study0/raw/project_implicit_country_codes.csv")   # these are from Gender-Career_IAT_public_2005-2016_codebook.xlsx; country_name are relabeled from original

# outfile
PARTICIPANT_DF_OUT <- here("data/study0/processed/by_participant_df.csv")

############
# countryres -> country_name -> country_code
project_implicit_countries <- read_csv(IAT_COUNTRY_CODES_PATH) %>%
  mutate(country_code = countrycode::countrycode(country_name, 'country.name', 'iso2c'))  %>%
  select(countryres, country_code, country_name) %>%
  mutate(country_code = ifelse(countryres == "HK", "CN", country_code))
  
# read in raw iat data
raw_iat_behavioral <- read_feather(RAW_IAT_PATH) %>%
  select(D_biep.Male_Career_all,sex, countryres, PCT_error_3467, 
         Mn_RT_all_3467, Mn_RT_all_3, Mn_RT_all_4, Mn_RT_all_6, 
         Mn_RT_all_7, assocareer, assofamily, N_ERROR_3, N_ERROR_4,
         N_ERROR_6, N_ERROR_7, N_3, N_4, N_6, N_7,  age, edu_14, Order) %>%
  rename(overall_iat_D_score = D_biep.Male_Career_all,
         education = edu_14, 
         order = Order) %>%
  mutate(explicit_dif = assocareer - assofamily,
         sex = ifelse(sex == "m", 1, ifelse(sex == "f", 0, NA)),
         log_age = log(age)) %>%
  left_join(project_implicit_countries)  %>% # merge in country codes
  select(-countryres)

# get complete observations
raw_iat_behavioral_complete <- raw_iat_behavioral %>%
  filter(!is.na(sex),
         !is.na(age),
         !is.na(order),
         !is.na(country_code), 
         !is.na(overall_iat_D_score))

# do behavioral_exclusions
# same exclusions as Nosek, Banjali, & Greenwald (2002), pg. 104. 
iat_behavioral_filtered <- raw_iat_behavioral_complete %>%
  filter(Mn_RT_all_3467 <= 1500, # RTs
         Mn_RT_all_3 <= 1800,
         Mn_RT_all_4 <= 1800,
         Mn_RT_all_6 <= 1800,
         Mn_RT_all_7 <= 1800) %>%
  filter(N_ERROR_3/N_3 <=.25, # errors
         N_ERROR_4/N_4 <=.25,
         N_ERROR_6/N_6 <=.25,
         N_ERROR_7/N_7 <=.25)

# filter to only those countries with enough data 
country_ns <- iat_behavioral_filtered %>%
  count(country_code)  %>%
  filter(n >= MIN_PARTICIPANTS_PER_COUNTRY) %>%
  arrange(-n)

iat_behavioral_filtered_dense_country <- raw_iat_behavioral_complete %>%
  select(overall_iat_D_score, sex, log_age, education, order, 
         explicit_dif, explicit_dif, country_name, country_code) %>%
  filter(country_code %in% country_ns$country_code) 

# add residuals - residualizing out sex, order, and age
mod1 <- lm(explicit_dif ~ as.factor(sex) + log_age + as.factor(order), data = iat_behavioral_filtered_dense_country)
mod2 <- lm(overall_iat_D_score ~ as.factor(sex)  + log_age + as.factor(order), data = iat_behavioral_filtered_dense_country)

participant_df  <- iat_behavioral_filtered_dense_country %>%
  add_residuals(mod1, "es_iat_sex_age_order_explicit_resid") %>% 
  add_residuals(mod2, "es_iat_sex_age_order_implicit_resid")  %>%
  select(country_code, country_name, sex, 
           log_age, education, everything())

# save file to csv
write_csv(participant_df, PARTICIPANT_DF_OUT)
