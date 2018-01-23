
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




library(gapminder)

pop_data <- gapminder %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  select(country, pop) %>%
  ungroup() %>%
  mutate(country = fct_recode(country,
                                   "United States of America"= "United States", 
                                    "UK" = "United Kingdom",
                                   "Republic of Korea" = "Korea, Rep."))

country_ns <- raw_iat_behavioral_complete %>%
  count(countryres)  %>%
  filter(n >= MIN_PARTICIPANTS_PER_COUNTRY) %>%
  arrange(-n)

d_with_pop = d %>%
  left_join(pop_data, by = c("country_name" = "country"))  %>%
  left_join(left_join(countries_to_langs, country_ns, 
                      by = c("country_code" = "countryres")) %>% 
            select(country_name, n)) %>%
  distinct() 
d_with_pop[d_with_pop$country_name == "Cyprus", "pop"] = 1170125
d_with_pop[d_with_pop$country_name == "Russian Federation", "pop"] = 144463451
d_with_pop[d_with_pop$country_name == "Hong Kong", "pop"]  = 7389500
d_with_pop[d_with_pop$country_name == "United Arab Emirates", "pop"]  = 9400000
d_with_pop[d_with_pop$country_name == "UK", "n"]  = 638082


country_normalized = d_with_pop %>%
  group_by(language_name) %>%
  mutate(normalized_n = n/sum(n),
         normalized_pop = log(pop)/sum(log(pop)),
         country_weights = normalized_n*normalized_pop) %>%
  distinct() %>%
  group_by(country_name) %>%
  mutate(country_weights = country_weights/sum(country_weights))
  

weighted_all <- d_with_pop %>%
  left_join(country_normalized) %>%
  group_by(language_name) %>%
  summarize(es_behavioral_iat_weighted = weighted.mean(es_behavioral_iat, 
                                                       normalized_n, na.rm = T),
            iat_mean_weighted = weighted.mean(mean_iat, 
                                              normalized_n, na.rm = T),
            n = sum(n)) %>%
  left_join(countries_to_langs) %>%
  left_join(language_means_career_implicit_hand) %>%
  left_join(language_means_career_implicit_google) %>%
  distinct(language_name, .keep_all = T)


cor.test(weighted_all$es_behavioral_iat_weighted, weighted_all$es_hand_translation)
cor.test(weighted_all$es_behavioral_iat_weighted, weighted_all$es_google_translation)

m = lm(es_behavioral_iat_weighted ~ es_hand_translation, 
       data = weighted_all)
summary(m)

  m = lm(es_behavioral_iat_weighted ~ es_google_translation, weights = log(n), 
       data = weighted_all)
summary(m)

all <- implicit_behavioral_means_by_language %>%
  group_by(wiki_language_code, language_name) %>%
  summarize(es_behavioral_iat_weighted = weighted.mean(es_behavioral_iat, 
                                                       normalized_n, na.rm = T),
            es_behavioral_iat = mean(es_behavioral_iat)) %>%
  left_join(language_means_career_implicit_google, by = "wiki_language_code")  %>%
  left_join(language_means_career_implicit_hand, by = "wiki_language_code")  %>%
  filter(language_name != "Cantonese" & !is.na(es_behavioral_iat_weighted))  

behavior_lang_cor_imp <- cor.test(all$es_google_translation, all$es_hand_translation)
behavior_lang_cor_imp <- cor.test(all$es_google_translation, all$es_behavioral_iat_weighted)

ggplot(all, aes(x = es_behavioral_iat_weighted, y = es_google_translation)) +
  #geom_smooth(method='lm')
  geom_text(aes(label = wiki_language_code)) +
  
  
  ### NAME STUFF ##
  
  # get language means
  language_means_career_implicit_hand_names <- read.csv("analysis/study2b/data/career_effect_sizes_hand_translations_names.csv", 
                                                        col.names = c("wiki_language_code", "test_id", "test_name", "es_hand_translation"), 
                                                        header = F,
                                                        fill = TRUE)  %>%
  select(-test_id, -test_name)

#parity stuff

behavior_lang_cor_imp <- cor.test(all$es_hand_translation, all$es_behavioral_iat_weighted)
ggplot(all %>%, aes(x = es_hand_translation, y = es_behavioral_iat_weighted)) +
  geom_text(aes(label = language_name))

gdi <- read_csv("analysis/study1/data/GDI_index_2016.csv", na = "..") %>%
  select(1:2) %>%
  mutate(country_name = fct_recode(country_name, 
                                   "Hong Kong" = "Hong Kong, China (SAR)",
                                   "Republic of Korea" = "Korea (Republic of)",
                                   "UK" = "United Kingdom",
                                   "United States of America" = "United States"))
wps <- read_tsv("analysis/study1/data/WPS_index.csv") %>%
  mutate(country_name = fct_recode(country_name, 
                                   "Republic of Korea" = "Korea, Republic of",
                                   "UK" = "United Kingdom"))

country_means_career_implicit_with_indices <- country_means_career_implicit %>%
  left_join(gdi) %>%
  left_join(wps) 

cor.test(country_means_career_implicit_with_indices$es_behavioral_iat, 
         country_means_career_implicit_with_indices$wps_index)



```{r}
google_expanded_es_with_grammar <- google_expanded_es %>%
  left_join(gender_data %>% select(language_code, wikipedia_grammar_type2), by = c("wiki_language_code" = "language_code")) %>%
  filter(!is.na(wikipedia_grammar_type2) )

ggplot(google_expanded_es_with_grammar, 
       aes(x = es_google_translation_ex, y = es_behavioral_iat_weighted, color = wikipedia_grammar_type2)) +
  geom_smooth(method = "lm", alpha = .2) +
  geom_text_repel(aes(label = language_name), size = 2.3) + 
  geom_point() +
  facet_wrap(~wikipedia_grammar_type2)

google_expanded_es_with_grammar %>%
  group_by(wikipedia_grammar_type2)%>%
  do(tidy(cor.test(.$es_google_translation_ex, .$es_behavioral_iat_weighted)))


hand_es_with_grammar <- all %>%
  left_join(gender_data %>% select(language_code, wikipedia_grammar_type2), by = c("wiki_language_code" = "language_code")) %>%
  filter(!is.na(wikipedia_grammar_type2) )

ggplot(hand_es_with_grammar, 
       aes(x = es_hand_translation, y = es_behavioral_iat_weighted, color = wikipedia_grammar_type2)) +
  geom_smooth(method = "lm", alpha = .2) +
  geom_text_repel(aes(label = language_name), size = 2.3) + 
  geom_point() +
  facet_wrap(~wikipedia_grammar_type2)

```



## es behaviorv2
iat_means_with_grammar <- all_hand_es %>%
  full_join(gender_data, by = c("wiki_language_code" = "language_code")) %>%
  select(-contains("test")) %>%
  mutate(wikipedia_grammar_type2 = fct_recode(wikipedia_grammar_type2, 
                                              "Grammatical Gender"= "MF", 
                                              "No Grammatical Gender" = "none")) %>%
  filter(!is.na(wikipedia_grammar_type2) & !is.na(es_hand_translation)) %>%
  group_by(wikipedia_grammar_type2) %>%
  multi_boot_standard(col = "es_behavioral_iat_weighted", na.rm = T) %>%
  ungroup()

ggplot(iat_means_with_grammar, 
       aes(x = wikipedia_grammar_type2, 
           y = mean, 
           fill = wikipedia_grammar_type2, 
           color = wikipedia_grammar_type2)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),  
                  position = position_dodge(width = .9))

## es hand
iat_means_with_grammar <- all_hand_es %>%
  full_join(gender_data, by = c("wiki_language_code" = "language_code")) %>%
  select(-contains("test")) %>%
  mutate(wikipedia_grammar_type2 = fct_recode(wikipedia_grammar_type2, 
                                              "Grammatical Gender"= "MF", 
                                              "No Grammatical Gender" = "none")) %>%
  filter(!is.na(wikipedia_grammar_type2)) %>%
  group_by(wikipedia_grammar_type2) %>%
  multi_boot_standard(col = "es_hand_translation", na.rm = T) %>%
  ungroup()

ggplot(iat_means_with_grammar, 
       aes(x = wikipedia_grammar_type2, 
           y = mean, 
           fill = wikipedia_grammar_type2, 
           color = wikipedia_grammar_type2)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),  
                  position = position_dodge(width = .9))

#m <- lm( es_behavioral_iat_weighted ~ es_hand_translation * wikipedia_grammar_type2,data = iat_means_with_grammar)

#m <- lm( es_behavioral_iat_weighted ~ es_google_translation_ex + wikipedia_grammar_type2,data = iat_means_with_grammar_google)

#summary(m)

```

```{r, include = F, eval = F}
# es google expanded
language_means_career_implicit_g <- read.csv("analysis/study2b/data/career_effect_sizes_google_translations_expanded.csv", 
                                             col.names = c("wiki_language_code", "test_id", "test_name", "es_google_translation_ex"), 
                                             header = F, fill = TRUE)  %>% select(-test_id, -test_name)

google_expanded_es <- implicit_behavioral_means_by_language %>%
  group_by(wiki_language_code, language_name) %>%
  summarise(es_behavioral_iat_weighted = weighted.mean(es_behavioral_iat, 
                                                       normalized_n, na.rm = T),
            es_behavioral_iat = mean(es_behavioral_iat)) %>%
  left_join(language_means_career_implicit_g, by = "wiki_language_code")  %>%
  filter(language_name != "Cantonese" & !is.na(es_behavioral_iat_weighted))  %>% ungroup()

iat_means_with_grammar_google <- google_expanded_es %>%
  full_join(gender_data, by = c("wiki_language_code" = "language_code")) %>%
  select(-contains("test")) %>%
  mutate(wikipedia_grammar_type2 = fct_recode(wikipedia_grammar_type2, 
                                              "Grammatical Gender"= "MF", 
                                              "No Grammatical Gender" = "none")) %>%
  filter(!is.na(wikipedia_grammar_type2)) %>%
  group_by(wikipedia_grammar_type2) %>%
  multi_boot_standard(col = "es_google_translation_ex", na.rm = T) %>%
  ungroup()

ggplot(iat_means_with_grammar_google, 
       aes(x = wikipedia_grammar_type2, 
           y = mean, 
           fill = wikipedia_grammar_type2, 
           color = wikipedia_grammar_type2)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),  
                  position = position_dodge(width = .9))

```

```{r, eval = F}
all <- country_means_career_explicit_es %>%
  left_join(max_prop_es_translations) %>%
  left_join(countries_to_langs, by = "country_name") %>%
  group_by(wiki_language_code.x) %>%
  summarize_at(vars(mean_diff:es_behavioral_iat), mean, na.rm = T) 

full_d <- all %>%
  full_join(gender_data, by = c("wiki_language_code.x" = "language_code")) %>%
  select(-contains("test")) %>%
  mutate(wikipedia_grammar_type2 = fct_recode(wikipedia_grammar_type2, 
                                              "Grammatical Gender"= "MF", 
                                              "No Grammatical Gender" = "none"))

iat_means2 <-  full_d %>%
  filter(!is.na(wikipedia_grammar_type2)) %>%
  group_by(wikipedia_grammar_type2) %>%
  multi_boot_standard(col = "es_behavioral_iat", na.rm = T)

ggplot(iat_means2, aes(x = wikipedia_grammar_type2, 
                       y = mean, fill = wikipedia_grammar_type2, color = wikipedia_grammar_type2)) +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),  
                  position = position_dodge(width = .9)) +
  theme_minimal() +
  ggtitle("Gender IAT Behavioral versus Grammar Type") +
  ylab("IAT Behavioral Effect size") +
  xlab("Grammar Type") +
  scale_colour_manual(values = c("red", "blue")) +
  theme(text = element_text(size = TEXT_SIZE),
        legend.position = "none")
```


