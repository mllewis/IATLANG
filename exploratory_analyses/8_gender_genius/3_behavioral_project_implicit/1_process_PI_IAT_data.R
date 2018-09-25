# process project implicit IAT data - by country and by participant df

library(tidyverse)
library(feather)
library(modelr)
library(haven)


MIN_PARTICIPANTS_PER_COUNTRY <- 400
RAWDATA_FILE <-  "data/Gender-Science IAT.public.2003-2017.sav"
RAWDATA_FEATHER <- "data/Gender-Science IAT.public.2003-2017.feather"
PI_COUNTRY_NAMES <- "data/Gender-Science_country_codes.csv"
GENDER_MEASURES_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/IATLANG/analyses/4_gender_measures/data/gender_measures/all_gender_measures2.csv"
AGE_DATA_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/IATLANG/analyses/7_age_controls/median_country_age_world_factbook.csv"
LANGUAGE_PERCENT_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/IATLANG/writeup/cogsci2018/analysis/study2b/data/languages_with_percent.csv"

OUTFILE_DF_BY_COUNTRY <- "data/gender_science_by_country2.csv"
OUTFILE_DF_BY_PARTICIPANT <-  "data/gender_science_by_participant.csv"
OUTFILE_DF_BY_LANGUAGE <-  "data/gender_science_by_language2.csv"
  
# Save SPSS file to feather so it's easier to work with
raw_iat_behavioral <- read_sav(RAWDATA_FILE) 
#write_feather(d, RAWDATA_FEATHER)

# Read in raw iat data
#raw_iat_behavioral <- read_feather(RAWDATA_FEATHER) 

raw_iat_behavioral_tidy <- raw_iat_behavioral %>%
  select(D_biep.Male_Science_all, sex, country, countryres, countryres_num, PCT_error_3467, 
         Mn_RT_all_3467, Mn_RT_all_3, Mn_RT_all_4, Mn_RT_all_6, 
         Mn_RT_all_7, larts, lscience, N_ERROR_3, N_ERROR_4,
         N_ERROR_6, N_ERROR_7, N_3, N_4, N_6, N_7,  age, edu_14, occupation, religionid, Order) %>%
  rename(overall_iat_D_score = D_biep.Male_Science_all,
         education = edu_14, 
         order = Order) %>%
  mutate(explicit_dif = larts - lscience) %>%
  separate(occupation, c("occupation", "occupation2"), sep = "-") 
## scale chanes on larts lscience measure, but not sure how based on code back - should check this!

# Tidy country info (three different country variables were used at different points)
project_implicit_countries <- read_csv(PI_COUNTRY_NAMES)  # these are from Gender-Career_IAT_public_2005-2016_codebook.xlsx; country_name are relabeled from original
raw_iat_behavioral_tidy_clean_cont <- raw_iat_behavioral_tidy %>%
      mutate(country = ifelse(country %in% c("", "."), NA, country),
             countryres = ifelse(countryres %in% c("", "."), NA, countryres),
             countryres = ifelse(is.na(countryres), country, countryres),
             countryres = ifelse(is.na(countryres), countryres_num, countryres)) %>%
      left_join(project_implicit_countries, by = c("countryres" = "country")) %>%
      select(-countryres, -country, -countryres_num) %>%
      mutate(country_name = as.factor(country_name)) 

# Get complete observations 
raw_iat_behavioral_complete <- raw_iat_behavioral_tidy_clean_cont %>%
  filter(sex %in% c("f", "m"),
         !is.na(country_name), 
         !is.na(overall_iat_D_score),
         !is.na(age) & age > 0) %>%
  mutate(log_age = log(age))

country_ns <- raw_iat_behavioral_complete %>%
  count(country_name)  %>%
  filter(n >= MIN_PARTICIPANTS_PER_COUNTRY) %>%
  arrange(-n)

raw_iat_behavioral_complete_dense_country <- raw_iat_behavioral_complete %>%
  filter(country_name %in% country_ns$country_name)

# Do behavioral_exclusions
# same exclusions as Nosek, Banjali, & Greenwald (2002), pg. 104. 
iat_behavioral_filtered <- raw_iat_behavioral_complete_dense_country %>%
  filter(Mn_RT_all_3467 <= 1500, # RTs
         Mn_RT_all_3 <= 1800,
         Mn_RT_all_4 <= 1800,
         Mn_RT_all_6 <= 1800,
         Mn_RT_all_7 <= 1800) %>%
  filter(N_ERROR_3/N_3 <=.25, # errors
         N_ERROR_4/N_4 <=.25,
         N_ERROR_6/N_6 <=.25,
         N_ERROR_7/N_7 <=.25)

# Add residuals
mod1 <- lm(explicit_dif ~ as.factor(sex)  + log_age + as.factor(order), data = iat_behavioral_filtered)
mod2 <- lm(overall_iat_D_score ~ as.factor(sex)  + log_age + as.factor(order) , data = iat_behavioral_filtered)
mod3 <- lm(overall_iat_D_score ~ education + as.factor(sex)  + log_age + as.factor(order) , data = iat_behavioral_filtered)

iat_behavioral_with_resids  <- iat_behavioral_filtered %>%
  add_residuals(mod1, "es_iat_sex_age_order_explicit_resid") %>% 
  add_residuals(mod2, "es_iat_sex_age_order_implicit_resid") %>%
  add_residuals(mod3, "es_iat_sex_age_order_implicit_resid2") 

iat_behavioral_tidy <- iat_behavioral_with_resids %>%
  mutate(country_name = as.factor(country_name),
         country_name = fct_recode(country_name,
                            "United States of America"= "U.S.A.",
                            "Netherlands"= "Netherlands, The",
                            "Hong Kong" = "Hong Kong S.A.R.",
                            "Republic of Korea" = "Korea",
                            "UK" = "United Kingdom")) 

# SAVE PARTICIPANT DF
participant_df <- iat_behavioral_tidy %>%
  select(1,2,9:10, 19:29)
write_csv(participant_df, OUTFILE_DF_BY_PARTICIPANT)


# SAVE COUNTRY DF
# Add median age and ggi data
country_age_data <- read_csv(AGE_DATA_PATH)
country_gender_data <- read_csv(GENDER_MEASURES_PATH) %>%
  select(country_code, country_name, ggi) 

# mean scores by country
behavioral_means_by_country <- iat_behavioral_tidy %>%
  mutate(sex_num = ifelse(sex == "m", 1, ifelse(sex == "f", 0, NA))) %>%
  group_by(country_name) %>%
  summarise(es_behavioral_iat_simple =  mean(overall_iat_D_score),
            es_behavioral_explicit_simple = mean(explicit_dif, na.rm = T),
            es_behavioral_iat_resid_simple =  mean(es_iat_sex_age_order_implicit_resid, na.rm = T),
            es_behavioral_iat_resid_simple2 =  mean(es_iat_sex_age_order_implicit_resid2, na.rm = T),
            es_behavioral_explicit_resid_simple = mean(es_iat_sex_age_order_explicit_resid, na.rm = T),
            participant_age = mean(log_age, na.rm = T),
            participant_education = mean(education, na.rm = T),
            participant_religosity = mean(religionid, na.rm = T),
            participant_sex = mean(sex_num, na.rm = T))  %>%
  left_join(country_age_data) %>%
  left_join(country_gender_data)

write_csv(behavioral_means_by_country, OUTFILE_DF_BY_COUNTRY)

# SAVE LANGUAGE DF
countries_to_langs <- read_csv(LANGUAGE_PERCENT_PATH) %>% # this comes from get_language_percentage.R
  mutate(country_name = fct_recode(country_name,
                                   "United States of America"= "United States", 
                                   UK = "United Kingdom",
                                   "Russian Federation" = "Russia",
                                   "Republic of Korea" = "South Korea"),
         country_code = fct_recode(country_code, UK = "GB"), 
         wiki_language_code = fct_recode(wiki_language_code,
                                         "zh"= "zh_yue")) # Cantonese isn't in gtranslate

# get one language per country
unique_langs_per_country <- countries_to_langs %>%
  group_by(country_name) %>%
  arrange(-prop_language)  %>%
  slice(1) %>%
  filter(!language_name %in% c("Niuean", "Cantonese")) %>%
  select(country_name, country_code, language_name, wiki_language_code) 

# average across countries speaking the same language, weighting by number of participatns
behavioral_means_by_language <- behavioral_means_by_country %>%
  left_join(unique_langs_per_country) %>%
  filter(!is.na(language_name)) %>% # exclude Namibia 
  group_by(wiki_language_code, language_name) %>%
  summarise(es_behavioral_iat_simple = mean(es_behavioral_iat_simple, na.rm = T),
            es_behavioral_explicit_simple = mean(es_behavioral_explicit_simple, na.rm = T),
            es_behavioral_iat_resid_simple = mean(es_behavioral_iat_resid_simple, na.rm = T),
            es_behavioral_iat_resid_simple2 =  mean(es_behavioral_iat_resid_simple2, na.rm = T),
            es_behavioral_explicit_resid_simple = mean(es_behavioral_explicit_resid_simple, na.rm = T),
            ggi = mean(ggi, na.rm = T),
            participant_age = mean(participant_age),
            participant_education = mean(participant_education),
            participant_religosity = mean(participant_religosity),
            participant_sex = mean(participant_sex),
            median_country_age = mean(median_age)) 

write_csv(behavioral_means_by_language, OUTFILE_DF_BY_LANGUAGE)

  