# get male/female centroids in ecah langauge so can calculate embedding gender bias

library(tidyverse)
library(data.table)

MODEL_PATH <- "/Volumes/wilbur_the_great/fasttext_models/"
TRANSLATION_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/IATLANG/analyses/2_pretrained_w2v_models/embedding_IAT/tidy_translations.csv"
# note! these are the old translations!
OUTFILE <- "gender_centroids_wiki_xling.csv"
MALE_WORDS <- c("son", "his","him","he", "brother","boy", "man", "male")
FEMALE_WORDS <- c("daughter", "hers", "her", "she",  "sister", "girl", "woman", "female")
# try temoving pronounts + husband/wife
# try removing pronouns from iat languages
LANGS <- list("en", "it", "fr", "de", "no")

target_translations <- read_csv(TRANSLATION_PATH) 
  
get_centroids <- function(lang, model_path, translations, male_words, female_words){
    print(lang)
  
    full_path <- paste0(model_path, "wiki.", lang, ".vec")
    
    model <- fread(
      full_path,
      header = FALSE,
      skip = 1,
      quote = "",
      encoding = "UTF-8",
      data.table = TRUE,
      col.names = c("target_word",
                  unlist(lapply(2:301, function(x) paste0("V", x)))))
  
    boy_translations <- translations %>%
      filter(language_code == lang) %>%
      filter(target_word %in% male_words) %>%
      distinct(target_word, .keep_all = T) %>%
      pull(translation) 
    
    girl_translations <- translations %>%
      filter(language_code == lang) %>%
      filter(target_word %in% female_words) %>% 
      distinct(target_word, .keep_all = T) %>%
      pull(translation) 
  
    tidy_translation <- data.frame(male_targets = boy_translations,
                                   female_targets = girl_translations) %>%
      gather("word_type", "target_word") %>%
      mutate(english_word = c(male_words, female_words)) %>%
      select(english_word, word_type, target_word) %>%
      separate(target_word, 
               c("t1", "t2", "t3", "t4"), "/") %>%
      gather("translation_id", "target_word", -1,-2) %>%
      filter(!is.na(target_word)) %>%
      separate(target_word, 
               c("w1", "w2", "w3", "w4", "w5"), " ") %>%
      gather("word_id", "translation", -1:-3) %>%
      filter(!is.na(translation))
  
    words_with_model <- tidy_translation %>%
      left_join(model, by = c("translation" = "target_word"))
  
    lang_centroids <- words_with_model %>%
      select(-translation) %>%
      group_by(english_word, word_type, translation_id) %>% # sum across word ids
      summarise_at(vars(V2:V301), sum, na.rm = TRUE) %>%
      group_by(english_word, word_type) %>% 
      summarize_at(vars(V2:V301), mean, na.rm = TRUE) %>% # mean across words 
      group_by(word_type) %>% 
      summarize_at(vars(V2:V301), mean, na.rm = TRUE) %>% # mean across word in word_types
      mutate(wiki_lang_code =  lang) %>%
      select(wiki_lang_code, word_type, everything())
  
    lang_centroids
}

all_centroids <- map_df(LANGS,
                     get_centroids,
                     MODEL_PATH,
                     target_translations,
                     MALE_WORDS,
                     FEMALE_WORDS)

write_csv(all_centroids, OUTFILE)
