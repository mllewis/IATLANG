# IAT Google translations


library(feather)
library(tidyverse)
library(data.table)
library(googleLanguageR)

# translate prompt using google translate
get_essay_words_translation <- function(targ_lang, word, output_path){
  
  if(targ_lang == "ar"){print(as.character(word))}
  
  if (targ_lang == "en"){
    translated_text <- word
  } else {
    translated_text <- gl_translate(as.character(word), 
                                    target = targ_lang, 
                                    source = "en")$translatedText
  }
  
  new_row <- data.frame(lang = targ_lang,
                        target_word = word,
                        translated_word = translated_text)
  
  write_csv(new_row, append = TRUE, path = output_path)
}



####################### SET PARAMS ######################
OUTPUT_PATH <- "google_translated_words.csv"
gl_auth("/Users/mollylewis/Documents/research/Projects/L2ETS/studies/study2/analyses/3_translation_analyses/prompt_embeddings/L2ETS\ prompt\ translations-a8edd99b5aa9.json") # authenticate google api

################## GET TARGET WORDS ######################
words <- read_csv("../tidy_translations.csv") %>%
  distinct(target_word)

# mapping of countries to languages
countries_langs <- read_csv("../../../data/other/countries_lang.csv") %>%
  mutate(language_name = ifelse(language_name == "Spanish; Castilian", "Spanish", language_name),
         language_name = ifelse(language_name == "Dutch; Flemish", "Dutch", language_name))

## countries we have IAT data for (N = 44)
iat_countries <- read_csv("../../../data/other/iat_behavior_langs.csv") %>%
  left_join(countries_langs %>% select(country_code, language_code), 
            by = c("countryres"= "country_code")) %>%
  filter(!is.na(language_code))
IAT_LANGS <- unique(iat_countries$language_code)


################## GET ALL TRANSLATIONS ######################
lw_combos <- expand.grid(IAT_LANGS, sort(words$target_word)) %>%
  rename(langs = Var1, words = Var2)

walk2(as.character(lw_combos$langs),  
      as.character(lw_combos$words), 
      get_essay_words_translation,
      OUTPUT_PATH) 
