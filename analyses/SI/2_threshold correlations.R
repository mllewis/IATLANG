## get correlations for a range of cuffoff values of min participants per country

# load packages
library(tidyverse)
library(feather)
library(modelr)
library(here)

HELPERS <- here("writeup/journal/helpers/psych_to_mat.R")
OUTFILE <- here("data/SI/corrs_by_exclusions.csv")

source(HELPERS)

get_by_language_df <- function(min_per_country, exclude_explicit){
  ### PARTICIPANT ### 
  RAW_IAT_PATH <- here("data/study0/raw/Gender-Career\ IAT.public.2005-2016.feather") # this file is "Gender-Career IAT.public.2005-2016.sav" in feather form (taken from: https://osf.io/gmewy/); it is not in the repository because it is too big.
  IAT_COUNTRY_CODES_PATH <- here("data/study0/raw/project_implicit_country_codes.csv")   # these are from Gender-Career_IAT_public_2005-2016_codebook.xlsx; country_name are relabeled from original

  PARTICIPANT_DF_OUT <- here("data/SI/processed/by_participant_df_temp.csv")
  
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
  raw_iat_behavioral_complete1 <- raw_iat_behavioral %>%
    filter(!is.na(sex),
           !is.na(age),
           !is.na(order),
           !is.na(country_code), 
           !is.na(overall_iat_D_score)) 
  
  if (exclude_explicit){
    raw_iat_behavioral_complete <-raw_iat_behavioral_complete1 %>%
      filter(!is.na(explicit_dif))
  } else {
    raw_iat_behavioral_complete <-raw_iat_behavioral_complete1
  }
  
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
    filter(n >= min_per_country) %>%
    arrange(n) 
  
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
  
  ### COUNTRY ### 
  AGE_DATA_PATH <- here("data/study0/raw/median_country_age_world_factbook.csv") #https://www.cia.gov/library/publications/the-world-factbook/rankorder/2177rank.html
  OBJECTIVE_MEASURES_DF <- here("data/study0/processed/per_women_stem_by_country.csv") 
  
  # add country median age data
  age_data <- read_csv(AGE_DATA_PATH) %>%
    mutate(country_code = countrycode::countrycode(country_name, 'country.name', 'iso2c'))  %>%
    select(-country_name) %>%
    rename(median_country_age = median_age)
  
  iat_behavioral_tidy <- participant_df%>%
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
  stem_data <- read_csv(OBJECTIVE_MEASURES_DF) 
  
  # make country df
  country_df <- behavioral_means_by_country %>%
    left_join(stem_data) %>% # note that 10 countries are missing from stoet data
    left_join(behavioral_Ns_by_country, by = c("country_code", "country_name"))  # hong kong has same code as china
  
  ### LANGUAGE ### 
  LANGUAGE_COUNTRY_IN <- here("data/study0/processed/top_lang_by_country_ethnologue.csv")
  unique_langs_per_country <- read_csv(LANGUAGE_COUNTRY_IN)
  
  # average across countries speaking the same language
  behavioral_means_by_language <- country_df %>%
    left_join(unique_langs_per_country) %>%
    ungroup() %>%
    select(-country_code, -country_name, -language_name) %>%
    group_by(wiki_language_code) %>%
    summarise_all(mean, na.rm = T) 
  
  list(behavioral_means_by_language, min_per_country, exclude_explicit)
}
get_corrs <- function(participant_vars) {
  iat_behavioral_es <- participant_vars[[1]] %>%
    rename(language_code = "wiki_language_code") %>%
    select(language_code, median_country_age, 
           prop_male,log_age, es_iat_sex_age_order_explicit_resid,
           es_iat_sex_age_order_implicit_resid, per_women_stem_2012_2017, n_participants)
  
  LANG_IAT_PATH <- here("data/study1b/iat_es_lang.csv")
  iat_lang_es <- read_csv(LANG_IAT_PATH)
  all_es <- left_join(iat_behavioral_es, iat_lang_es, by = "language_code")
  
  # remove exclusions and fix croatian to be mean of hr and sr (only in wiki)
  EXCLUSIONS_PATH <- here("data/study1b/language_exclusions.csv") 
  exclusions <- read_csv(EXCLUSIONS_PATH)
  
  hr_new_wiki <- mean(c(filter(iat_lang_es, language_code == "hr") %>%  pull(lang_es_wiki),
                        filter(iat_lang_es, language_code == "sr") %>%  pull(lang_es_wiki)))
  
  all_es_tidy <- all_es %>%
    left_join(exclusions) %>%
    mutate(lang_es_wiki = case_when(exclude_wiki == TRUE ~ NA_real_,
                                    TRUE ~ lang_es_wiki),
           lang_es_sub = case_when(exclude_sub == TRUE ~ NA_real_,
                                   TRUE ~ lang_es_sub)) %>%
    select(-exclude_wiki, -exclude_sub) %>%
    mutate(lang_es_wiki = case_when(language_code == "hr" ~ hr_new_wiki,
                                    TRUE ~ lang_es_wiki),
           lang_es_sub = case_when(language_code == "hr" ~ NA_real_, # sr is missing from sub
                                   TRUE ~ lang_es_sub)) 
  
  # corr of lanng, behavioral, etc.
  all_corr_vars <- all_es_tidy %>%
    select(lang_es_sub, lang_es_wiki, es_iat_sex_age_order_explicit_resid, 
           es_iat_sex_age_order_implicit_resid, per_women_stem_2012_2017, median_country_age) %>%
    rename(`Residualized Behavioral IAT` = "es_iat_sex_age_order_implicit_resid",
           `Residualized Explicit Bias` = "es_iat_sex_age_order_explicit_resid",
           `Language IAT (Subtitles)` = "lang_es_sub",
           `Language IAT (Wikipedia)` = "lang_es_wiki",
           `Percent Women in STEM` = "per_women_stem_2012_2017",
           `Median Country Age` = "median_country_age") 
  
  simple_corr <- psych::corr.test(all_corr_vars, adjust = "none")$r %>%
    as_tibble(rownames = "rowname") %>%
    gather("var2", "simple_r", -rowname)
  
  simple_corr_p <- psych::corr.test(all_corr_vars, adjust = "none")$p %>%
    as_tibble(rownames = "rowname") %>%
    gather("var2", "simple_p", -rowname)
  
  partial_psych_obj <- psych::partial.r(data = all_corr_vars, x = 1:5, y = 6) 
  partial_corr <- psych::corr.p(partial_psych_obj, n = nrow(all_corr_vars) - 1, 
                                adjust = "none")$r %>%
    psych_to_mat() %>%
    as_tibble(rownames = "rowname") %>%
    gather("var2", "partial_r", -rowname)
  
  partial_corr_p <- psych::corr.p(partial_psych_obj, n = nrow(all_corr_vars) - 1, 
                                  adjust = "none")$p %>%
    psych_to_mat() %>%
    as_tibble(rownames = "rowname") %>%
    gather("var2", "partial_p", -rowname)
  
  tidy_corrs <- simple_corr %>%
    left_join(simple_corr_p) %>%
    left_join(partial_corr) %>%
    left_join(partial_corr_p) %>%
    filter(rowname %in% c("Residualized Explicit Bias",  "Residualized Behavioral IAT", "Percent Women in STEM")|
           var2 %in% c("Residualized Explicit Bias",  "Residualized Behavioral IAT", "Percent Women in STEM"))
  
  
  out_df <- tidy_corrs %>%
    rowwise() %>%
    mutate(rowname = sort(c(rowname, var2))[1],
           var2 = sort(c(rowname, var2))[2]) %>%
    distinct(rowname, var2, .keep_all = T) %>%
    mutate(min_per_country = participant_vars[[2]],
           exclude_explicit =  participant_vars[[3]],
           n_sub = nrow(all_es %>% filter(!is.na(lang_es_sub))),
           n_wiki = nrow(all_es %>% filter(!is.na(lang_es_wiki))))
  
  out_df
}

wrapper_function <- function(x, y){
  out_corrs <- get_by_language_df(x, y) %>%
    get_corrs()
  
  write_csv(out_corrs, OUTFILE, append = T)
}

threshold_values <- cross_df(
  list(min = seq(200, 1000, 50),
       exp_exclude = c(TRUE, FALSE)))

walk2(threshold_values$min, threshold_values$exp_exclude, wrapper_function)