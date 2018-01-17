## get translations from google API for IAT words (google_translated_words.csv) ##

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
OUTPUT_PATH <- "data/google_translated_words.csv"
gl_auth("/Users/mollylewis/Documents/research/Projects/L2ETS/studies/study2/analyses/3_translation_analyses/prompt_embeddings/L2ETS\ prompt\ translations-a8edd99b5aa9.json") # authenticate google api

# langs
iat_langs <- read_csv("data/language_names_to_wiki_codes.csv")$wiki_language_code[c(-25, -34)]

# words
target_words <- read_csv("data/tidy_translations.csv") %>%
  distinct(target_word) %>%
  unlist(use.names = F)

################## GET ALL TRANSLATIONS ######################
lw_combos <- expand.grid(iat_langs, sort(target_words)) %>%
  rename(langs = Var1, words = Var2)

walk2(as.character(lw_combos$langs),  
      as.character(lw_combos$words), 
      get_essay_words_translation,
      OUTPUT_PATH) 
