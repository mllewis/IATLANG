library(tidyverse)

langdat <- read_csv("../../data/IAT/Gender-Genius/InternationalIAT_LanguageData.csv")


langdat_tidy  <- langdat %>%
  mutate(language = tolower(PrimaryLanguage),
         language2 = tolower(LanguageOther))  %>%
  mutate(id = 1:n())

count(langdat_tidy, language) %>%
  arrange(-n)

count(langdat_tidy, language2) %>%
  arrange(-n) %>%
  data.frame()


freq_languages <- c("english", "spanish", "hindustani", "french", "portuguese", "tamil", "italian", "malayalam", "hindi")

lang1s <- filter(langdat_tidy, language %in% freq_languages) %>%
  select(id, language)

lang2s <- filter(langdat_tidy, language2 %in% freq_languages) %>%
  select(id, language2)

langdat_tidy  %>%
  mutate(language = ifelse(language == "other", language2, language))%>%
  mutate(lang_type = ifelse(language == "other", "secondar", "primary")) %>%
  #filter(id %in% c(lang1s$id, lang2s$id)) %>%
  select(id, language, language2, Country, lang_type) %>%
  count(language, Country) %>%
  arrange(-n) %>%
  slice(1:30) %>%
  data.frame()


target_langs <- c("english", "spanish", "hindi", "urdu", "french",
                  "portuguese", "spanish", 
                  "tamil", "malayalam", "italian")