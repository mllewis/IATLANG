## Make country, language, participant df with all variabes
#(1) IAT BY COUNTRY SCORES
#(2) IAT BY LANGUAGE SCORES
#(3) SEMANTICS SCORES BY LANGUAGE
#(4) OBJECTIVE GENDER SCORES BY LANGUAGE

library(tidyverse)
library(feather)
library(modelr)

# set params
MIN_PARTICIPANTS_PER_COUNTRY <- 400

RAW_IAT_PATH <- "../../writeup/cogsci2018/analysis/study1/data/Gender-Career IAT.public.2005-2016.feather"
IAT_COUNTRY_CODES_PATH <- "../../writeup/cogsci2018/analysis/study1/data/project_implicit_country_codes.csv"
LANGUAGE_PERCENT_PATH <- "../../writeup/cogsci2018/analysis/study2b/data/languages_with_percent.csv"
LANGUAGE_ES_PATH <- "../../analyses/4_gender_measures/data/other/all_es_wide.csv"
GENDER_MEASURE_PATH <- "../../analyses/4_gender_measures/data/gender_measures/all_gender_measures2.csv"
SCIENCE_PATH <- "stoet_data.csv"

PARTICIPANT_DF_OUT <- "by_participant_df.csv"
COUNTRY_DF_OUT <- "by_country_df.csv"
LANGUAGE_DF_OUT <- "by_language_df.csv"

### (0) IAT IMPLICIT AND EXPLICIT BY PARICIPANT ###
# countryres -> country_name
project_implicit_countries <- read_csv(IAT_COUNTRY_CODES_PATH)  # these are from Gender-Career_IAT_public_2005-2016_codebook.xlsx; country_name are relabeled from original

# read in raw iat data
raw_iat_behavioral <- read_feather(RAW_IAT_PATH) %>%
  select(D_biep.Male_Career_all,sex, countryres, PCT_error_3467, 
         Mn_RT_all_3467, Mn_RT_all_3, Mn_RT_all_4, Mn_RT_all_6, 
         Mn_RT_all_7, assocareer, assofamily, N_ERROR_3, N_ERROR_4,
         N_ERROR_6, N_ERROR_7, N_3, N_4, N_6, N_7,  age, edu_14, occupation, religionid) %>%
  rename(overall_iat_D_score = D_biep.Male_Career_all,
         education = edu_14) %>%
  separate(occupation, c("occupation", "occupation2"), sep = "-") %>%
  mutate(explicit_dif = assocareer - assofamily,
         sex = ifelse(sex == "m", 1, ifelse(sex == "f", 0, NA))) %>%
  left_join(project_implicit_countries)  # merge in country codes
# this file is "Gender-Career/Gender-Career IAT.public.2005-2016.sav" in feather form (taken from: https://osf.io/gmewy/); it is not in the repository because it is too big.

# get complete observations
raw_iat_behavioral_complete <- raw_iat_behavioral %>%
  filter(!is.na(sex),
         !is.na(countryres), 
         countryres != ".",
         countryres != "nu",  # not clear what this refers to (it's lower case and not in codebook- Niue seems unlikely)
         !is.na(overall_iat_D_score))

# filter to only those countries with enough data 
country_ns <- raw_iat_behavioral_complete %>%
  count(countryres)  %>%
  filter(n >= MIN_PARTICIPANTS_PER_COUNTRY) %>%
  arrange(-n)

raw_iat_behavioral_complete_dense_country <- raw_iat_behavioral_complete %>%
  filter(countryres %in% country_ns$countryres) 

# do behavioral_exclusions
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

# add residuals
mod1 <- lm(overall_iat_D_score ~ sex + age, data = iat_behavioral_filtered)
mod2 <- lm(overall_iat_D_score ~ sex + age + religionid + explicit_dif , data = iat_behavioral_filtered)
mod3 <- lm(explicit_dif ~ sex + age, data = iat_behavioral_filtered)
mod4 <- lm(explicit_dif ~ sex + age + religionid + overall_iat_D_score , data = iat_behavioral_filtered)

iat_behavioral_tidy  <- iat_behavioral_filtered %>%
  add_residuals(mod1, "es_iat_sex_age_resid") %>% 
  add_residuals(mod2, "es_iat_sex_age_religion_explicit_resid")  %>%
  add_residuals(mod3, "es_explicit_sex_age_implicit_resid") %>% 
  add_residuals(mod4, "es_explicit_sex_age_religion_iat_resid")
  
  
### (1) IAT IMPLICIT AND EXPLICIT BY COUNTRY ###

# mean scores by country
behavioral_means_by_country <- iat_behavioral_tidy %>%
  group_by(country_name) %>%
  summarise(es_behavioral_iat = mean(overall_iat_D_score)/sd(overall_iat_D_score),
            es_behavioral_explicit = mean(explicit_dif, na.rm = T)/sd(explicit_dif, na.rm = T),
            es_behavioral_iat_resid1 = mean(es_iat_sex_age_resid, na.rm = T)/sd(es_iat_sex_age_resid, na.rm = T),
            es_behavioral_explicit_resid1 = mean(es_explicit_sex_age_implicit_resid, na.rm = T)/sd(es_explicit_sex_age_implicit_resid, na.rm = T),
            es_behavioral_iat_resid2 = mean(es_iat_sex_age_religion_explicit_resid, na.rm = T)/sd(es_iat_sex_age_religion_explicit_resid, na.rm = T),
            es_behavioral_explicit_resid2 = mean(es_explicit_sex_age_religion_iat_resid, na.rm = T)/sd(es_explicit_sex_age_religion_iat_resid, na.rm = T),
            participant_age = mean(age, na.rm = T),
            participant_education = mean(education, na.rm = T),
            participant_religosity = mean(religionid, na.rm = T),
            participant_sex = mean(sex, na.rm = T))

# for weighted averages when averaging across country
country_ns_final <- iat_behavioral_tidy %>%
  group_by(countryres) %>%
  summarize(n_implicit = length(which(!is.na(overall_iat_D_score))),
            n_explicit = length(which(!is.na(explicit_dif))))


### (2) IAT BY LANGUAGE ###
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
    left_join(country_ns_final, by = c("country_code"= "countryres")) %>% 
    group_by(wiki_language_code, language_name) %>%
    mutate(normalized_n_implicit = n_implicit/sum(n_implicit),
           normalized_n_explicit = n_explicit/sum(n_explicit)) %>%
    summarise(es_behavioral_iat_weighted = weighted.mean(es_behavioral_iat, 
                                                       normalized_n_implicit, na.rm = T),
              es_behavioral_iat = mean(es_behavioral_iat),
              es_behavioral_explicit_weighted = weighted.mean(es_behavioral_explicit, 
                                                            normalized_n_explicit, na.rm = T),
              es_behavioral_explicit = mean(es_behavioral_explicit,na.rm = T),
              es_behavioral_iat_resid1 = mean(es_behavioral_iat_resid1,na.rm = T),
              es_behavioral_explicit_resid1 = mean(es_behavioral_explicit_resid1,na.rm = T),
              es_behavioral_iat_resid2 = mean(es_behavioral_iat_resid2,na.rm = T),
              es_behavioral_explicit_resid2 = mean(es_behavioral_explicit_resid2,na.rm = T),
              participant_age = mean(participant_age),
              participant_education = mean(participant_education),
              participant_religosity = mean(participant_religosity),
              participant_sex = mean(participant_sex)) %>%
  filter(!is.na(es_behavioral_iat_weighted)) 
  
  
### (3) LANGUAGE SEMANTICS EFFECT SIZE ###
language_means_career_implicit_hand_by_language <- read_csv(LANGUAGE_ES_PATH) %>%
    select(-career_behavioral_iat, -wps_index) 
  

### (4) OBJECTIVE GENDER MEASURES ### 
objective_country_measures_by_country <- read_csv(GENDER_MEASURE_PATH) %>%
 select(-sigi, -sigi_physical,-contains("schooling"),  -contains("ggi_"), -sigi_son) 

all_gender_measures_transformed_by_country <- objective_country_measures_by_country %>%
  select(-sigi_fam,  -gii, -gdi)


### (5) SCIENCE MEASURES ### 
stoet_data <- read_csv(SCIENCE_PATH) 
tidy_science_by_country <- stoet_data %>% 
  mutate(country_code= countrycode::countrycode(country_name, 'country.name', 'iso2c')) %>%
  select(country_code, everything()) %>%
  select(-country_name) %>% 
  rename(ggi_stoet = ggi)

tidy_science_by_language <- stoet_data %>%
  mutate(country_code= countrycode::countrycode(country_name, 'country.name', 'iso2c')) %>%
  select(country_code, everything()) %>%
  select(-country_name) %>% 
  rename(ggi_stoet = ggi) %>%
  left_join(unique_langs_per_country %>% select(country_code, wiki_language_code)) %>%
  select(-country_code) %>%
  group_by(wiki_language_code) %>%
  summarize_all(mean)


### Make participant df

participant_df <- iat_behavioral_tidy %>%
    select(1,2,3,20:22, 24:25, 27:31)

write_csv(participant_df, PARTICIPANT_DF_OUT)

### Make country df
country_df <- full_join(behavioral_means_by_country, 
                        all_gender_measures_transformed_by_country) %>%
  left_join(unique_langs_per_country %>% select(country_name, wiki_language_code)) %>%
  left_join(tidy_science_by_country, by = "country_code")

write_csv(country_df, COUNTRY_DF_OUT)

### Make language df
lang_df <- full_join(behavioral_means_by_language, 
                     language_means_career_implicit_hand_by_language, by = "wiki_language_code") %>%
  left_join(tidy_science_by_language, by = "wiki_language_code")

write_csv(lang_df, LANGUAGE_DF_OUT)


  