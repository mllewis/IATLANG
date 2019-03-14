library(tidyverse)
library(langcog)
library(feather)
library(countrycode)

)


##################################################
############### ANALYSIS VERSION #################
##################################################
setwd("/Users/mollylewis/Documents/research/Projects/IATLANG/2_pretrained_w2v_models/embedding_IAT/")

# grammar
gender_dataA <- read_csv("../language_bias/gender_grammar.csv") %>%
  rename(wikipedia_grammar_type = Wikipedia) %>%
  select(language_code, language_name, wikipedia_grammar_type) %>%
  filter(!is.na(wikipedia_grammar_type)) %>%
  mutate(wikipedia_grammar_type2 = ifelse(wikipedia_grammar_type == "none", "none", "MF"))

############ READ IN DATA ###############
countries_langs <- read_csv("../../data/other/countries_lang.csv") %>%
  mutate(language_name = ifelse(language_name == "Spanish; Castilian", "Spanish", language_name),
         language_name = ifelse(language_name == "Dutch; Flemish", "Dutch", language_name))

# behavioral - language level
IAT_behavior_measures_imp <- read_csv("../behavior_IAT/IAT_behavior_measures.csv") %>%
  left_join(countries_langs %>% select(country_name, language_name, language_code)) %>%
  filter(type == "country_means_D_score") %>%
  group_by(type, language_code) %>%
  summarize(mean_iat = mean(mean)) 

############ MERGE TOGETHER AND GET MEANS ###############
full_d <- IAT_behavior_measures_imp %>%
  full_join(lang_bias, by = "language_code") %>%
  full_join(gender_dataA, by = "language_code") %>%
  select(-contains("test"))

# get means
iat_means <-  full_d %>%
  filter(!is.na(wikipedia_grammar_type2)) %>%
  group_by(type, wikipedia_grammar_type2) %>%
  multi_boot_standard(col = "mean", na.rm = T)

ggplot(iat_means, 
       aes(x = wikipedia_grammar_type2, 
           y = mean, fill = wikipedia_grammar_type2,
           color = wikipedia_grammar_type2)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),  
                  position = position_dodge(width = .9))+ 
  theme_minimal()


##################################################
############### PAPER VERSION ####################
##################################################
setwd("/Users/mollylewis/Documents/research/Projects/IATLANG/writeup/cogsci2018/")

############ READ IN DATA ###############
# ------# 
#pi_country_codes <- read_csv("analysis/study1/data/project_implicit_country_codes.csv")
  
# behavioral means by language
raw_iat_behavioral <- read_feather("analysis/study1/data/Gender-Career IAT.public.2005-2016.feather") %>%
  select(D_biep.Male_Career_all,sex, countryres, PCT_error_3467, Mn_RT_all_3467, Mn_RT_all_3, Mn_RT_all_4, Mn_RT_all_6, Mn_RT_all_7, assocareer, assofamily,N_ERROR_3, N_ERROR_4, N_ERROR_6, N_ERROR_7, N_3, N_4, N_6, N_7) %>%
  rename(overall_iat_D_score = D_biep.Male_Career_all) 

MIN_PARTICIPANTS_PER_COUNTRY <- 400

raw_iat_behavioral_complete <- raw_iat_behavioral %>%
  filter(sex %in% c("f", "m"),
         !is.na(countryres), 
         countryres != ".",
         countryres != "nu", 
         !is.na(overall_iat_D_score)) 

country_ns <- raw_iat_behavioral_complete %>%
  count(countryres)  %>%
  filter(n >= MIN_PARTICIPANTS_PER_COUNTRY) %>%
  arrange(-n) 
raw_iat_behavioral_complete_dense_country <- raw_iat_behavioral_complete %>%
  filter(countryres %in% country_ns$countryres)

iat_behavioral <- raw_iat_behavioral_complete_dense_country %>%
  filter(Mn_RT_all_3467 <= 1500, #RTs
         Mn_RT_all_3 <= 1800,
         Mn_RT_all_4 <= 1800,
         Mn_RT_all_6 <= 1800,
         Mn_RT_all_7 <= 1800) %>%
  filter(N_ERROR_3/N_3 <=.25, # errors
         N_ERROR_4/N_4 <=.25,
         N_ERROR_6/N_6 <=.25,
         N_ERROR_7/N_7 <=.25)

iat_behavioral_tidy <- iat_behavioral %>%
  mutate(country_name = countrycode(countryres,
                                    "iso2c",
                                    "country.name"),
         country_name = replace(country_name, 
                                country_name == "Viet Nam", "Vietnam"),
         country_name = replace(country_name, 
                                country_name == "Taiwan, Province of China", "Taiwan"),
         country_name = replace(country_name, 
                                countryres == "UK", "UK"))

country_ns_final <- count(iat_behavioral_tidy, countryres) 
# ------ # 


country_means_career_implicit <- iat_behavioral_tidy %>%
  group_by(country_name) %>%
  summarize(mean_iat = mean(overall_iat_D_score),
            es_behavioral_iat = mean(overall_iat_D_score)/sd(overall_iat_D_score)) 

countries_to_langs2 <- read_csv("analysis/study2b/data/languages_with_percent.csv") %>% # this comes from get_language_percentage.R
  mutate(country_name = fct_recode(country_name,
                                   "United States of America"= "United States", 
                                   GB = "United Kingdom",
                                   "Russian Federation" = "Russia",
                                   "Republic of Korea" = "South Korea"),
         wiki_language_code = fct_recode(wiki_language_code,
                                         "zh" = "zh_yue")) # Cantonese isn't in gtranslate

# get one language per country
unique_langs_per_country <- countries_to_langs2 %>%
  group_by(country_name) %>%
  arrange(-prop_language)  %>%
  slice(1)

language_ns <- country_ns_final %>%
  left_join(unique_langs_per_country, by = c("countryres"= "country_code")) %>%
  group_by(wiki_language_code) %>%
  summarise(n_speakers = sum(n))
  

max_prop_es_translations <- unique_langs_per_country %>%
  select(country_name, country_code, language_name, wiki_language_code) 

# get behaviora IAT by language (compare to IAT_behavior_measures_imp)
implicit_behavioral_means_by_language <- country_means_career_implicit %>%
  left_join(max_prop_es_translations) %>%
  left_join(country_ns, by = c("country_code"= "countryres")) %>%
  filter(!is.na(language_name)) %>%
  group_by(wiki_language_code) %>%
  summarize(es_behavioral_iat = mean(es_behavioral_iat),
            mean_iat = mean(mean_iat)) %>%
  ungroup() %>%
  mutate(wiki_language_code = as.character(wiki_language_code))

gender_dataP <- read_csv("analysis/study3/data/gender_grammar.csv") %>%
  rename(wikipedia_grammar_type = Wikipedia) %>%
  select(language_code, language_name, wikipedia_grammar_type) %>%
  filter(!is.na(wikipedia_grammar_type)) %>%
  mutate(wikipedia_grammar_type2 = ifelse(wikipedia_grammar_type == "none", "none", "MF"))
  #mutate(wikipedia_grammar_type2 = ifelse(wikipedia_grammar_type %in% c("none", "CN"), "none", "MF"))
  
## get means by grammar type
iat_means <- implicit_behavioral_means_by_language %>%
  full_join(gender_dataP, by = c("wiki_language_code" = "language_code")) %>%
  select(-contains("test")) %>%
  mutate(wikipedia_grammar_type2 = fct_recode(wikipedia_grammar_type2, 
                                              "Grammatical Gender"= "MF", 
                                              "No Grammatical Gender" = "none"),
         wiki_language_code = as.character(wiki_language_code)) %>%
  left_join(language_ns) %>%
  filter(!wiki_language_code %in% c("hi"))

iat_means_with_grammar <- iat_means %>% 
  filter(!is.na(wikipedia_grammar_type2)) %>%
  group_by(wikipedia_grammar_type2) %>%
  multi_boot_standard(col = "es_behavioral_iat", na.rm = T) %>%
  ungroup()  
  
ggplot(iat_means_with_grammar, 
       aes(x = wikipedia_grammar_type2, 
           y = mean, 
           fill = wikipedia_grammar_type2, 
           color = wikipedia_grammar_type2)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),  
                  position = position_dodge(width = .9))

ggplot(iat_means, 
       aes(x = jitter(as.numeric(wikipedia_grammar_type2), .9), 
           y = es_behavioral_iat, 
           fill = wikipedia_grammar_type2, 
           color = wikipedia_grammar_type2)) +
  ggplot2::xlim(0, 3) +
  geom_violin()
  geom_point(aes(size = log(n_speakers), x = as.numeric(wikipedia_grammar_type2))) +
  xlab("Grammar Type") +
  geom_text(aes(label = wiki_language_code)) +
  theme_bw()

mf <- filter(iat_means, wikipedia_grammar_type2 == "Grammatical Gender")
none <- filter(iat_means, wikipedia_grammar_type2 == "No Grammatical Gender")

t.test(mf$es_behavioral_iat, none$es_behavioral_iat, paired = F)


#weights::wtd.t.test(mf$es_behavioral_iat, none$es_behavioral_iat,
#           weight = log(mf$n_speakers), weighty = log(none$n_speakers),
#           samedata=F)

####
both = full_join(IAT_behavior_measures_imp %>% rename(mean_iat_analysis = mean_iat),
          implicit_behavioral_means_by_language %>% rename(mean_iat_paper = mean_iat), 
          by = c("language_code" = "wiki_language_code")) %>%
  ungroup() %>%
  select(-type) %>%
  full_join(gender_dataP, by = "language_code") %>%
  filter(!is.na(wikipedia_grammar_type2)) %>%
  select(-language_name.y)

cor.test(both$es_behavioral_iat, both$mean_iat_paper)

both %>%
  #filter(!language_code %in% c("hi", "hu", "vi")) %>% # <- these three languages
  #filter(!language_code %in% c( "hi")) %>% # <- these three languages
  gather(measure, value, c(-1, -3, -6:-7)) %>%
  group_by(wikipedia_grammar_type2, measure) %>%
  multi_boot_standard(col = "value", na.rm = T) %>%
  ungroup() %>%
  ggplot(aes(x = wikipedia_grammar_type2, 
             y = mean, 
             fill = wikipedia_grammar_type2, 
             color = wikipedia_grammar_type2)) +
  facet_wrap(~ measure, scale = "free") +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),  
                  position = position_dodge(width = .9))



all_hand_es <- implicit_behavioral_means_by_language %>%
  group_by(wiki_language_code, language_name) %>%
  summarise(es_behavioral_iat_weighted = weighted.mean(es_behavioral_iat, 
                                                       normalized_n, na.rm = T),
            es_behavioral_iat = mean(es_behavioral_iat)) %>%
  left_join(language_means_career_implicit_hand, by = "wiki_language_code")  %>%
  filter(language_name != "Cantonese" & !is.na(es_behavioral_iat_weighted))  %>%
  ungroup()

#### 
iat_means <- all_hand_es %>%
  full_join(gender_dataP, by = c("wiki_language_code" = "language_code")) %>%
  select(-contains("test")) %>%
  mutate(wikipedia_grammar_type2 = fct_recode(wikipedia_grammar_type2, 
                                              "Grammatical Gender"= "MF", 
                                              "No Grammatical Gender" = "none"),
         wiki_language_code = as.character(wiki_language_code)) %>%
  left_join(language_ns)

mf <- filter(iat_means, es_behavioral_iat_weighted, wikipedia_grammar_type2 == "Grammatical Gender")
none <- filter(iat_means, es_behavioral_iat_weighted, wikipedia_grammar_type2 == "No Grammatical Gender")

t.test(mf$es_behavioral_iat_weighted, none$es_behavioral_iat_weighted, paired = F)


google_expanded_es <- google_expanded_es %>%
  full_join(gender_dataP, by = c("wiki_language_code" = "language_code")) %>%
  select(-contains("test")) %>%
  mutate(wikipedia_grammar_type2 = fct_recode(wikipedia_grammar_type2, 
                                              "Grammatical Gender"= "MF", 
                                              "No Grammatical Gender" = "none"),
         wiki_language_code = as.character(wiki_language_code)) %>%
  left_join(language_ns)

mf <- filter(google_expanded_es, wikipedia_grammar_type2 == "Grammatical Gender")
none <- filter(google_expanded_es, wikipedia_grammar_type2 == "No Grammatical Gender")

cor.test(mf$es_hand_translation, mf$es_behavioral_iat)
cor.test(none$es_hand_translation, none$es_behavioral_iat)



 
### in sum: 
# behavioral is significant when you exclude hindi (but not otherwise, p .08)
# language hand is marginal (p = .07)
# google expanded is siginficant (p = .02)
# strenght of corrlation doesn pan out



