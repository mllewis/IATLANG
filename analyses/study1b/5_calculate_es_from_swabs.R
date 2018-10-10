# calculate overall effect size from swabs

# load packages
library(tidyverse)

INFILE <- "../../data/study2b/iat_swabs.csv"
OUTFILE <- "../../data/study2b/iat_es_lang.csv"

# finish es calculation to account for gendered languages
swabs_career <- read_csv(INFILE,
                         col_names = c("language_code", "model_source",
                                       "category_type", "mean_swab", "attribute",
                                       "sd", "gender"))

es_career_swabs_career <- swabs_career %>%
  filter(!(category_type == "category_1" & attribute == "F"),
         !(category_type == "category_2" & attribute == "M")) %>%
  select(-attribute, -gender) %>%
  group_by(language_code, model_source) %>%
  spread(category_type, mean_swab) %>%
  summarize_all(mean, na.rm = T) %>%
  mutate(sYXab_num = category_1 - category_2,
         sYXab = sYXab_num/sd)  %>%
  select(model_source, language_code,  sYXab) %>%
  spread(model_source, sYXab) %>%
  rename(lang_es_sub = sub,
         lang_es_wiki = wiki)

### write to csv
write_csv(es_career_swabs_career, OUTFILE)