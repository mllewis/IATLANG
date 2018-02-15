# pre-process gender measures
library(tidyverse)
library(wbstats)
library(countrycode)
library(broom)
# README: https://docs.google.com/document/d/1dQUGOP-7t_7BvdwQIN5BlgoSfH6h-X8rIBdxsVt8FBM/edit

get_corr <- function(test1, test2, df){
  
  df %>%
    select(test1, test2) %>%
    do(tidy(cor.test(unlist(.[,1]), unlist(.[,2])))) %>%
    mutate(test1 = test1, 
           test2 = test2)
}

### READ IN ALL HDI MEASURES
hdi <- read_csv("data/gender_measures/HDI_complete.csv") %>%
  mutate_at(3:28, as.numeric) %>%
  mutate(mean_value = rowMeans(.[,3:28], na.rm = T))   %>%
  mutate(country_code = countrycode(country, "country.name", "iso2c")) %>%
  select(kpi_name, country_code, mean_value) %>%
  spread(kpi_name, mean_value, -2)

#### MERGE TOGETHER
country_to_langs <- read_csv("data/other/country_to_lang.csv")
country_to_langs[country_to_langs$country_name == "UK", "country_code"] = "GB"

# read in bias measures
bias_measures <- read_csv("data/other/all_es_wide.csv") %>%
  select(-wps_index) %>%
  full_join(country_to_langs, by = c("wiki_language_code"))%>%
  left_join(hdi)  

MEASURES <- names(bias_measures)[9:109]
ALPHA <- .1

### BY COUNTRY
# add age residuals
mod1 <- lm(career_behavioral_iat ~ `Median Age` + `Adult Mortality Rate, Male`, 
           data = bias_measures)

mod2 <- lm(career_google ~ `Median Age` + `Adult Mortality Rate, Male`, data = bias_measures)

bias_measures = bias_measures %>%
  modelr::add_residuals(mod1) %>%
  rename(career_behavioral_iat_resid = resid) %>%
  modelr::add_residuals(mod2) %>%
  rename(career_google_resid = resid)

career_google_resid_corrs = map2_df(rep("career_google_resid", length(MEASURES)), 
                               MEASURES, get_corr, bias_measures)  %>%
  select(test2, estimate, p.value) %>%
  mutate(sig_cg = ifelse(p.value < ALPHA, 1, 0)) %>%
  rename_at(vars(estimate, p.value), funs(paste0(., "_cg")))


career_google_corrs = map2_df(rep("career_google", length(MEASURES)), 
                              MEASURES, get_corr, bias_measures)  %>%
  select(test2, estimate, p.value) %>%
  mutate(sig_cgr = ifelse(p.value < ALPHA, 1, 0)) %>%
  rename_at(vars(estimate, p.value), funs(paste0(., "_cgr")))

weapons_google_corrs = map2_df(rep("weapons_google", length(MEASURES)), 
                               MEASURES, get_corr, bias_measures)  %>%
  select(test2, estimate, p.value) %>%
  mutate(sig_wg = ifelse(p.value < ALPHA, 1, 0)) %>%
  rename_at(vars(estimate, p.value), funs(paste0(., "_wg")))

flowers_google_corrs = map2_df(rep("flowers_google", length(MEASURES)), 
                               MEASURES, get_corr, bias_measures)  %>%
  select(test2, estimate, p.value) %>%
  mutate(sig_fg = ifelse(p.value < ALPHA, 1, 0)) %>%
  rename_at(vars(estimate, p.value), funs(paste0(., "_fg")))

career_behavioral_iat_corrs = map2_df(rep("career_behavioral_iat", length(MEASURES)), 
                                      MEASURES, get_corr, bias_measures)  %>%
  select(test2, estimate, p.value) %>%
  mutate(sig_cb = ifelse(p.value < ALPHA, 1, 0)) %>%
  rename_at(vars(estimate, p.value), funs(paste0(., "_cb")))

career_behavioral_iat_resid_corrs = map2_df(rep("career_behavioral_iat_resid", length(MEASURES)), 
                                      MEASURES, get_corr, bias_measures)  %>%
  select(test2, estimate, p.value) %>%
  mutate(sig_cbr = ifelse(p.value < ALPHA, 1, 0)) %>%
  rename_at(vars(estimate, p.value), funs(paste0(., "_cbr")))

career_hand_corrs = map2_df(rep("career_hand", length(MEASURES)), 
                            MEASURES, get_corr, bias_measures)  %>%
  select(test2, estimate, p.value) %>%
  mutate(sig_ch = ifelse(p.value < ALPHA, 1, 0)) %>%
  rename_at(vars(estimate, p.value), funs(paste0(., "_ch")))

all <- list(career_google_corrs,  career_google_resid_corrs, 
            career_behavioral_iat_corrs, career_behavioral_iat_resid_corrs,
            career_hand_corrs, weapons_google_corrs, 
            flowers_google_corrs) %>%
  reduce(left_join, by = "test2") %>%
  mutate(sum_sig = rowSums(.[,c(4, 7,10,13,16,19, 22)], na.rm = T))  %>%
  filter(sum_sig > 0)

filter(all, sig_cbr  == 1 & sig_ch  == 1 ) %>%
  select(-contains("wg"), -contains("fg"), -contains("sum"))%>%
  arrange(p.value_cg)

"Gross Enrollment Ratio, Primary Education"
"Median Age"
"Share of Seats in Parliament"

lm(career_behavioral_iat ~ `Share of Seats in Parliament` + 
     `Median Age`, bias_measures) %>%
  summary()

#### LANGUAGE LEVEL
lang_df = all_countries %>%
  left_join(bias_measures, 
            by = "wiki_language_code") %>%
  select(-contains(".y"))  %>%
  #select(c(-10:-13, -16)) %>% 
  select(1:5, 8,  everything())  %>%
  group_by(wiki_language_code) %>%
  summarize_at(vars(career_google:`Youth Unemployment Rate, Female to Male Ratio`), mean, na.rm = T) 
  #filter(wiki_language_code != "vi")



career_google_corrs = map2_df(rep("career_google", length(MEASURES)), 
                              MEASURES, get_corr, lang_df)  %>%
  select(test2, estimate, p.value) %>%
  mutate(sig_cg = ifelse(p.value < ALPHA, 1, 0)) %>%
  rename_at(vars(estimate, p.value), funs(paste0(., "_cg")))

weapons_google_corrs = map2_df(rep("weapons_google", length(MEASURES)), 
                               MEASURES, get_corr, lang_df)  %>%
  select(test2, estimate, p.value) %>%
  mutate(sig_wg = ifelse(p.value < ALPHA, 1, 0)) %>%
  rename_at(vars(estimate, p.value), funs(paste0(., "_wg")))

flowers_google_corrs = map2_df(rep("flowers_google", length(MEASURES)), 
                               MEASURES, get_corr, lang_df)  %>%
  select(test2, estimate, p.value) %>%
  mutate(sig_fg = ifelse(p.value < ALPHA, 1, 0)) %>%
  rename_at(vars(estimate, p.value), funs(paste0(., "_fg")))

career_behavioral_iat_corrs = map2_df(rep("career_behavioral_iat", length(MEASURES)), 
                                      MEASURES, get_corr, lang_df)  %>%
  select(test2, estimate, p.value) %>%
  mutate(sig_cb = ifelse(p.value < ALPHA, 1, 0)) %>%
  rename_at(vars(estimate, p.value), funs(paste0(., "_cb")))

career_hand_corrs = map2_df(rep("career_hand", length(MEASURES)), 
                            MEASURES, get_corr, lang_df)  %>%
  select(test2, estimate, p.value) %>%
  mutate(sig_ch = ifelse(p.value < ALPHA, 1, 0)) %>%
  rename_at(vars(estimate, p.value), funs(paste0(., "_ch")))

all <- list(career_google_corrs,  career_behavioral_iat_corrs, 
            career_hand_corrs, weapons_google_corrs, 
            flowers_google_corrs) %>%
  reduce(left_join, by = "test2") %>%
  mutate(sum_sig = rowSums(.[,c(4, 7,10,13,16)], na.rm = T))  %>%
  filter(sum_sig > 0)


filter(all, sig_cb == 1 & sig_cg == 1 ) %>%
  #slice(c(2,7,8,9,12,13,14,15,21)) %>%
  #data.frame()
  select(-contains("wg"), -contains("fg"), -contains("sum")) %>%
  arrange(p.value_cb)

lm(career_behavioral_iat ~  career_hand + `Median Age`, lang_df) %>%
  summary()
