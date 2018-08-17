# tidy behavioral data from cimpian and stroage

library(tidyverse)
library(modelr)

OUTFILE <- "all_cimpian_behavioral.csv"
MINPARTICIPANTS <- 10

lang_key <- read_tsv("data/language_name_to_google.csv")

# Study 2
IAT_behavioral_path1 <-"../../../data/IAT/Gender-Genius/InternationalIAT_LanguageData.csv"

IAT_behavioral_path_raw1 <- read_csv(IAT_behavioral_path1)

IAT_behavioral_tidy1 <- IAT_behavioral_path_raw1 %>%
  mutate(lang1 = tolower(PrimaryLanguage),
         #lang2 = tolower(lang_other_clean),
         Gender = tolower(Gender),
         subid = as.factor(1:n()))  %>%
  rename(iat_score = IATScore, 
         gender = Gender,
         age = Age,
         country = Country,
        # ses = SES, 
         condition = ConditionC,
         conservatism = Conservatism,
         status = Status, 
         children = Children) %>%
  mutate(lang = ifelse(lang1 == "other", lang2, lang1)) %>%
       #  region = as.factor(countrycode::countrycode(country, "country.name", "region"))) %>%
  left_join(lang_key %>% select(language_code, lang)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(condition = as.factor(condition), 
         log_age = log(age)) %>%
  select(subid, iat_score, lang, language_code, gender, condition, log_age, country, conservatism, status, children) 


## Study1
IAT_behavioral_path2 <- "../../../data/IAT/Gender-Genius/IAT_Study1_Combined_Master_Dataset_LanguageData.csv"

IAT_behavioral_path_raw2 <- read_csv(IAT_behavioral_path2)

IAT_behavioral_tidy2 <- IAT_behavioral_path_raw2 %>%
  mutate(lang = tolower(PrimaryLanguage),
         Gender = tolower(Gender),
         subid = as.factor(SubjectID))  %>%
  rename(iat_score = IATScore, 
         gender = Gender,
         age = Age,
         country = Country,
         condition = Condition,
         #sexism = Sexism,
         #race = Race,
         #politicalparty = PoliticalParty,
         conservatism = Conservatism,
         status = Status, 
         children = Children)%>%
         #income = Income) %>%
  left_join(lang_key %>% select(language_code, lang)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(condition = as.factor(condition),
         conservatism = as.factor(conservatism),
         children = as.numeric(children),
         log_age = log(age)) %>%
  select(subid, iat_score, lang, language_code, gender, condition, log_age, country, conservatism, status, children) 

IAT_behavioral_tidy <- bind_rows(IAT_behavioral_tidy1, IAT_behavioral_tidy2)

# add residuals
mod12 <- lm(iat_score ~ as.factor(gender) + log_age + as.factor(condition), data = IAT_behavioral_tidy)
mod1 <- lm(iat_score ~ as.factor(gender) + log_age + as.factor(condition), data = IAT_behavioral_tidy1)
mod2 <- lm(iat_score ~ as.factor(gender) + log_age + as.factor(condition), data = IAT_behavioral_tidy2)


IAT_behavioral_tidy_with_resids12  <- IAT_behavioral_tidy %>%
  add_residuals(mod12, "iat_resid_storage_genius_12")
IAT_behavioral_tidy_with_resids2 <-IAT_behavioral_tidy2 %>% 
  add_residuals(mod2, "iat_resid_storage_genius_2")
IAT_behavioral_tidy_with_resids1<-IAT_behavioral_tidy1 %>% 
  add_residuals(mod1, "iat_resid_storage_genius_1")

targ_langs12 <- IAT_behavioral_tidy_with_resids12 %>%
  count(language_code) %>%
  filter(n >= MINPARTICIPANTS) %>%
  filter(!is.na(language_code)) %>%
  pull(language_code)

behavioral_means_tidy12 <- IAT_behavioral_tidy_with_resids12 %>%
  filter(language_code %in% targ_langs12) %>%
  group_by(language_code)  %>%
  summarize(iat_resid_storage_genius_12 = mean(iat_resid_storage_genius_12, na.rm = T))
  
targ_langs1 <- IAT_behavioral_tidy_with_resids1 %>%
  count(language_code) %>%
  filter(n >= MINPARTICIPANTS) %>%
  filter(!is.na(language_code)) %>%
  pull(language_code)

behavioral_means_tidy1 <- IAT_behavioral_tidy_with_resids1 %>%
  filter(language_code %in% targ_langs1) %>%
  group_by(language_code)  %>%
  summarize(iat_resid_storage_genius_1 = mean(iat_resid_storage_genius_1, na.rm = T))


targ_langs2 <- IAT_behavioral_tidy_with_resids2 %>%
  count(language_code) %>%
  filter(n >= MINPARTICIPANTS) %>%
  filter(!is.na(language_code)) %>%
  pull(language_code)

behavioral_means_tidy2 <- IAT_behavioral_tidy_with_resids2 %>%
  filter(language_code %in% targ_langs2) %>%
  group_by(language_code)  %>%
  summarize(iat_resid_storage_genius_2 = mean(iat_resid_storage_genius_2, na.rm = T))


all_cimpian_behavioral <- full_join(behavioral_means_tidy1, behavioral_means_tidy2) %>%
  full_join(behavioral_means_tidy12)

write_csv(all_cimpian_behavioral, OUTFILE)

