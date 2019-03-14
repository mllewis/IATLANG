### SANITY CHECK: comparing_gender_measures_languages_v1.Rmd vs. comparing_gender_measures2.Rmd

# load packages
library(tidyverse)
library(broom)
library(corrplot)

ALPHA <- .1

# V1:
## behavior (N = 31) - by language
lang_codes <- read_csv("/Users/mollylewis/Documents/research/Projects/IATLANG/writeup/cogsci2018/analysis/study2b/data/language_names_to_wiki_codes.csv")

behavioral_wps <- read_csv("/Users/mollylewis/Documents/research/Projects/IATLANG/writeup/cogsci2018/analysis/all/all_measures_df.csv") %>%
  left_join(lang_codes) %>%
  group_by(wiki_language_code) %>%
  summarise(career_behavioral_iat =
              weighted.mean(es_behavioral_iat,
                            normalized_n, na.rm = T)) 

## language (N = 27)
PROP_MISSING_CUTOFF <- .2

career_hand <- read.csv("/Users/mollylewis/Documents/research/Projects/IATLANG/writeup/cogsci2018/analysis/study2b/data/career_effect_sizes_hand_translations.csv", col.names = c("wiki_language_code", "test_id", "test_name", "es"),  header = F, fill = TRUE) %>%
  mutate(test_version = "career_hand")

career_google <- read.csv("/Users/mollylewis/Documents/research/Projects/IATLANG/writeup/cogsci2018/analysis/study2b/google_translate_and_names_analysis/data/career_effect_sizes_google_translations.csv", col.names = c("wiki_language_code", "test_id", "test_name", "es"),  header = F, fill = TRUE) %>%
  mutate(test_version = "career_google")

flowers_google <- read.csv("../6_flowers/data/flowers_effect_sizes_google.csv", 
                           col.names = c("wiki_language_code", "test_id", "test_name", "es"), header = F, fill = TRUE)  %>%
  mutate(test_version = "flowers_google")

prop_missing_raw <- read_csv("../6_flowers/data/prop_google_translate_missing.csv")  %>%
  filter(language_code != "zh_yue") %>%
  rowwise() %>%
  mutate(mean_prop_missing = mean(c(prop_missing_career, prop_missing_flowers))) %>%
  arrange(-mean_prop_missing)

prop_missing <- prop_missing_raw %>%
  filter((prop_missing_career < PROP_MISSING_CUTOFF) & 
           (prop_missing_flowers < PROP_MISSING_CUTOFF))

all_es_wide <- bind_rows(list(career_hand, career_google, 
                         flowers_google))  %>%
  select(-test_id, -test_name) %>%
  filter(wiki_language_code %in% prop_missing$language_code) %>%
  spread(test_version, es) %>%
  filter(!(wiki_language_code %in% c("zh", "hi")))

## objective gender gender measures (N = 48)
all_gender_measures1 <- read_csv("data/gender_measures/all_gender_measures.csv") %>%
    select(-sigi, -sigi_physical, -wb_cpia, -contains("schooling"), -gpi_literacy, -contains("ggi_"), -sigi_son)
  
all_gender_measures_transformed1 <- all_gender_measures1 %>%
  mutate(sigi_fam_log = log(sigi_fam),
         gii_log = log(gii),
         gdi_exp = gdi^10) %>%
  select(-sigi_fam,  -gii, -gdi) %>%
  mutate(sigi_fam_log = ifelse(is.infinite(sigi_fam_log), NA,  sigi_fam_log))

## Merge all three together (N = 31)
all_es_wide_with_behavior <- all_es_wide %>%
  left_join(behavioral_wps)

all_countries <- read_csv("data/other/country_to_lang.csv")
all_countries[all_countries$country_name == "UK", "country_code"] = "GB"

full_df_partial = all_countries %>%
  left_join(all_es_wide_with_behavior, 
            by = "wiki_language_code") %>%
  left_join(all_gender_measures_transformed1,  by = "country_code") %>%
  select(-contains(".y"))  %>%
  #select(c(-10:-13, -16)) %>%
  select(1:5, 8,  everything())  %>%
  group_by(wiki_language_code.x) %>%
  summarize_at(vars(career_google:gdi_exp), mean, na.rm = T) 


## GET CORR
full_df_partial_arranged <- select(full_df_partial,
                                   "career_behavioral_iat",
                                   "career_google",
                                   "career_hand",
                                   "flowers_google",
                                   "gdi_exp" ,
                                   "gii_log" ,
                                   "ggi" ,
                                   "wps" , "sigi_fam_log")

corr_mat <- cor(full_df_partial_arranged, 
                use = "pairwise.complete.obs")

p.mat <- cor.mtest(full_df_partial_arranged, 
                   conf.level = (1-ALPHA),  
                   use = "pairwise.complete.obs")$p

cols = rev(colorRampPalette(c("red", "white", "blue"))(100))

corrplot(corr_mat, method = "color",  col = cols,
         type = "upper", order = "original", number.cex = .7,
         addCoef.col = "black", 
         p.mat = p.mat, sig.level = ALPHA, insig = "blank", 
         tl.col = "black", tl.srt = 90,
         diag = FALSE)

# V2:

## Behavior (N = 39)
key <- read_csv("../../data/other/country_langiso_langwiki_key.csv") %>%
  select(countryres, wiki_language_code)

# average across countries weighting by number of participatns
implicit_behavioral_means_by_country_with_language <- country_means_career_implicit %>%
  left_join(max_prop_es_translations) %>%
  left_join(country_ns_final, by = c("country_code"= "countryres")) %>% 
  group_by(language_name) %>%
  mutate(normalized_n = n/sum(n)) %>%
  select(wiki_language_code, language_name, country_name, 
         es_behavioral_iat, normalized_n) 

bias_measures <- read_csv("../../data/IAT/Gender-Career/by_country_means_400.csv") %>%
  select(-contains("ci")) %>%
  left_join(key) %>%
  group_by(wiki_language_code) %>%
  summarise(career_behavioral_iat = mean(mean)) 

## Language (N = 25)
language_measures <- read_csv("data/other/all_es_wide.csv") %>%
  select(-wps_index, -career_behavioral_iat)

## Objective (N = 74)
all_gender_measures2 <- read_csv("data/gender_measures/all_gender_measures2.csv") %>%
  select(-sigi, -sigi_physical,  -contains("ggi_"), -sigi_son)

all_gender_measures_transformed2 <- all_gender_measures2 %>%
  mutate(sigi_fam_log = log(sigi_fam)) %>%
  select(-sigi_fam,) %>%
  mutate(sigi_fam_log = ifelse(is.infinite(sigi_fam_log), NA,  sigi_fam_log)) 

all_gender_measures_transformed2_by_lang <- all_gender_measures_transformed2 %>%
  left_join(key, by = c( "country_code" = "countryres" )) %>%
  group_by(wiki_language_code) %>%
  summarize_at(vars(gdi:sigi_fam_log), mean, na.rm = T)

## Merge together (N = 39)
full_df = bias_measures %>%
  left_join(language_measures) %>%
  left_join(all_gender_measures_transformed2_by_lang) %>%
  select(-weapons_google)

## GET CORR
corr_mat <- cor(full_df[,c(-1)], 
                use = "pairwise.complete.obs")

p.mat <- cor.mtest(full_df[,c(-1)], 
                   conf.level = (1-ALPHA),  
                   use = "pairwise.complete.obs")$p

cols = rev(colorRampPalette(c("red", "white", "blue"))(100))

corrplot(corr_mat, method = "color",  col = cols,
         type = "upper", order = "original", number.cex = .7,
         addCoef.col = "black", 
         p.mat = p.mat, sig.level = ALPHA, insig = "blank", 
         tl.col = "black", tl.srt = 90,
         diag = FALSE)


## compare
# behavior (note that V1 is weighted by the number of participants)
# r = .91
 behavioral_wps %>%
  full_join(bias_measures, by = "wiki_language_code") %>%
   as.data.frame()
  do(tidy(cor.test(.$career_behavioral_iat.x, .$career_behavioral_iat.y)))

# language          
# r = 1
language_measures %>%
  left_join(all_es_wide, by = "wiki_language_code") %>%
  do(tidy(cor.test(.$career_google.x, .$career_google.y)))

# objective
# r = 1
all_gender_measures_transformed1 %>%
  left_join(all_gender_measures_transformed2, by = "country_code") %>%
  do(tidy(cor.test(.$wps.x, .$wps.y)))

