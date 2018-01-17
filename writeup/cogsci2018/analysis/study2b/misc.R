
# Read in behavior and language biases
countries_to_langs <- read_csv("analysis/study2b/data/languages_with_percent.csv") %>% # this comes from get_language_percentage.R
  mutate(country_name = fct_recode(country_name,
                                   "United States of America"= "United States", 
                                   UK = "United Kingdom",
                                   "Russian Federation" = "Russia",
                                   "Republic of Korea" = "South Korea"),
         wiki_language_code = fct_recode(wiki_language_code,
                                         "zh"= "zh_yue")) # Cantonese isn't in gtranslate

language_means_career_implicit_hand <- read.csv("analysis/study2b/data/career_effect_sizes_hand_translations.csv", 
                                                col.names = c("wiki_language_code", "test_id", "test_name", "es_hand_translation"), header = F,
                                                fill = TRUE)  %>%
  select(-test_id, -test_name)
language_means_career_implicit_google <- read.csv("analysis/study2b/data/career_effect_sizes_google_translations.csv", header = F,
                                                  col.names = c("wiki_language_code", "test_id", "test_name", "es_google_translation"), 
                                                  fill = TRUE)  %>%
  select(-test_id, -test_name) %>% # these come from 2_get_IAT_scores_from_wiki_models.R
  filter(wiki_language_code != "th" | wiki_language_code != "he" | wiki_language_code != "zu")

d <- country_means_career_implicit %>%
  left_join(countries_to_langs, by = "country_name") %>%
  left_join(language_means_career_implicit_hand, by = "wiki_language_code") %>%
  left_join(language_means_career_implicit_google) %>%
  select(language_name, wiki_language_code, country_name, 
         prop_language, mean_iat, es_behavioral_iat, es_hand_translation, es_google_translation)

lang_participant_totals <- countries_to_langs %>%
  left_join(country_ns, by = c("country_code" = "countryres"))  %>%
  group_by(language_name) %>%
  summarize(total_per_lang = sum(n, na.rm = T))

weighted_es_translations <- d %>%
  group_by(country_name) %>% 
  summarize(es_hand_translation_weighted = weighted.mean(es_hand_translation, 
                                                         prop_language, na.rm = T),
            es_google_translation_weighted = weighted.mean(es_google_translation, 
                                                           prop_language, na.rm = T),
            es_google_translation_weighted2 = ifelse(is.na(es_hand_translation_weighted), NA, es_google_translation_weighted))

max_prop_es_translations <- d %>%
  group_by(country_name) %>%
  arrange(-prop_language) %>%
  slice(1) %>%
  select(country_name, es_hand_translation, es_google_translation)

all <- country_means_career_implicit %>%
  left_join(weighted_es_translations) %>%
  left_join(max_prop_es_translations) %>%
  left_join(countries_to_langs, by = "country_name") %>%
  group_by(language_name) %>%
  summarize_at(vars(mean_iat:es_google_translation), mean, na.rm = T) %>%
  left_join(lang_participant_totals) %>%
  filter(language_name != "Cantonese" & language_name != "Pashto")

all %>%
  gather(key, value, c(-1:-3, -9)) %>% 
  group_by(key) %>%
  mutate(k = sqrt(total_per_lang)) %>%
  do(tidy(lm(mean_iat ~ value, weights = log(total_per_lang), data = .))) %>%
  arrange(p.value) %>%
  filter(term == "value")

all %>%
  gather(key, value, c(-1:-3, -9)) %>% 
  group_by(key) %>%
  #mutate(k = sqrt(total_per_lang)) %>%
  do(tidy(lm(es_behavioral_iat ~ value, weights = log(total_per_lang), data = .))) %>%
  arrange(p.value) %>%
  filter(term == "value")


all %>%
  gather(key, value, -1:-3) %>% 
  group_by(key) %>%
  do(tidy(cor.test(.$mean_iat, .$value))) %>%
  arrange(p.value) %>%
  select(key, estimate, p.value)

all %>%
  gather(key, value, -1:-3) %>% 
  group_by(key) %>%
  do(tidy(cor.test(.$es_behavioral_iat, .$value))) %>%
  arrange(p.value) %>%
  select(key, estimate, p.value)

all %>%
  gather(key, value, -1:-3) %>% 
  ggplot(aes(x = value, y = es_behavioral_iat)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~key)

all %>%
  gather(key, value, -1:-3) %>% 
  ggplot(aes(x = value, y = es_behavioral_iat)) +
  geom_text(aes(label = language_name), size = 2) +
  geom_smooth(method = "lm") +
  facet_grid(~key)

corrr::correlate(all[,-1:-4])