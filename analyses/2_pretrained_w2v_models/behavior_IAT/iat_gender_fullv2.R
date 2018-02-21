

# load packages
library(knitr)
library(rmarkdown)
library(tidyverse)
library(langcog)
library(stringr)
library(forcats)
library(broom)
library(haven)
library(countrycode)
library(feather)

MIN_PARTICIPANTS_PER_COUNTRY <- 400

# Read in career-gender IAT data. 

#d <- read_sav("../data/IAT/Gender-Career/Gender-Career IAT.public.2005-2016.sav") 
#write_feather(d, "../data/IAT/Gender-Career/Gender-Career IAT.public.2005-2016.feather") %>%
raw_iat_behavioral <- read_feather("../../../data/IAT/Gender-Career/Gender-Career IAT.public.2005-2016.feather") %>%
  rename(mixed_countryres = countryres) %>%
  select(D_biep.Male_Career_all, sex, age, mixed_countryres, PCT_error_3467, 
         Mn_RT_all_3467, Mn_RT_all_3, Mn_RT_all_4, Mn_RT_all_6, 
         Mn_RT_all_7, assocareer, assofamily, N_3, N_4, N_6, N_7) %>%
    rename(overall_iat_D_score = D_biep.Male_Career_all) 

country_key <- read_csv("../../../data/other/mixed_countryres_to_country_res_key.csv")

raw_iat_behavioral_complete <- raw_iat_behavioral %>%
                                filter(sex %in% c("f", "m"),
                                       !is.na(mixed_countryres), 
                                       mixed_countryres != ".",
                                       mixed_countryres != "nu",  # not clear what this refers to (it's lower case and not in codebook- Niue seems unlikely)
                                       !is.na(overall_iat_D_score)) %>%
                                left_join(country_key) %>%
                                select(-mixed_countryres)
                                      

country_ns <- raw_iat_behavioral_complete %>%
  left_join(country_key) %>%
  count(countryres)  %>%
  filter(n >= MIN_PARTICIPANTS_PER_COUNTRY) %>%
  arrange(-n)

raw_iat_behavioral_complete_dense_country <- raw_iat_behavioral_complete %>%
  filter(countryres %in% country_ns$countryres)

# same exclusions as Nosek, Banjali, & Greenwald (2002), pg. 104. 
iat_behavioral <- raw_iat_behavioral_complete_dense_country %>%
  filter(Mn_RT_all_3467 <= 1500, # RTs
         Mn_RT_all_3 <= 1800,
         Mn_RT_all_4 <= 1800,
         Mn_RT_all_6 <= 1800,
         Mn_RT_all_7 <= 1800) %>%
  filter(N_ERROR_3/N_3 <=.25, # errors
         N_ERROR_4/N_4 <=.25,
         N_ERROR_6/N_6 <=.25,
         N_ERROR_7/N_7 <=.25)

country_means_career <- iat_behavioral %>%
  group_by(countryres) %>%
  multi_boot_standard(col = "overall_iat_D_score") 


country_means_career_as <- iat_behavioral %>%
  group_by(countryres) %>%
  do(tidy(lm(overall_iat_D_score ~ age + sex, d = .)))  %>%
  mutate(ci_lower_as_intercept = estimate - (1.96*std.error),
         ci_upper_as_intercept = estimate + (1.96*std.error)) %>%
  filter(term == "(Intercept)") %>%
  select(countryres, estimate, ci_lower_as_intercept, ci_upper_as_intercept) %>%
  rename(estimate_as_intercept = estimate)

country_means_career_a <- iat_behavioral %>%
  group_by(countryres) %>%
  do(tidy(lm(overall_iat_D_score ~ age, d = .)))  %>%
  mutate(ci_lower_a_intercept = estimate - (1.96*std.error),
         ci_upper_a_intercept = estimate + (1.96*std.error)) %>%
  filter(term == "(Intercept)") %>%
  select(countryres, estimate, ci_lower_a_intercept, ci_upper_a_intercept) %>%
  rename(estimate_a_intercept = estimate)


# Join together and save
all_d_by_country <- country_means_career %>%
  left_join(country_means_career_as) %>%
  left_join(country_means_career_a)

write_csv(all_d_by_country,"../../../data/IAT/Gender-Career/by_country_means_400.csv") 


