

# V1:
## behavior
lang_codes <- read_csv("/Users/mollylewis/Documents/research/Projects/IATLANG/writeup/cogsci2018/analysis/study2b/data/language_names_to_wiki_codes.csv")

behavioral_wps <- read_csv("/Users/mollylewis/Documents/research/Projects/IATLANG/writeup/cogsci2018/analysis/all/all_measures_df.csv") %>%
  left_join(lang_codes) %>%
  group_by(wiki_language_code) %>%
  summarise(career_behavioral_iat =
              weighted.mean(es_behavioral_iat,
                            normalized_n, na.rm = T),
            wps_index = mean(wps_index)) 

all_es <- bind_rows(list(career_hand, career_google, 
                         flowers_google, weapons_google))  %>%
  select(-test_id, -test_name) %>%
  filter(wiki_language_code %in% prop_missing$language_code)

all_es_wide <- all_es %>%
  spread(test_version, es) %>%
  left_join(behavioral_wps)

bias_measures <- all_es_wide
  
# objective gender gender measures
all_gender_measures <- read_csv("data/gender_measures/all_gender_measures.csv") %>%
    select(-sigi, -sigi_physical, -wb_cpia, -contains("schooling"), -gpi_literacy, -contains("ggi_"), -sigi_son)
  
