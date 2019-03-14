# get gender score for each occupation word in each language

library(tidyverse)
library(data.table)


MODEL_PATH <- "/Volumes/wilbur_the_great/fasttext_models/"
CENTROID_PATH <- "../miseresky/gender_centroids_wiki_xling.csv"
OCCUPATION_TRANSLATIONS <- "gabriel_translations_clean.csv"
OUTFILE  <- "embedding_gender_bias_occupations_gabriel.csv"
LANGS <- list("en", "fr", "de")

all_centroids <- read_csv(CENTROID_PATH)
all_translations <- read_csv(OCCUPATION_TRANSLATIONS) 

get_gender_scores <- function(lang, model_path, translations, centroids, outfile) {
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

  these_centroids <- centroids %>%
      filter(wiki_lang_code == lang) %>%
    rename(target_word = word_type)
  
  these_translations <- translations %>%
    filter(wiki_lang_code == lang) %>%
    select(-wiki_lang_code)
  
  tidy_translation <- these_translations %>%
    separate(translation, 
             c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8"), "/") %>%
    gather("translation_id", "translation", -1) %>%
    filter(!is.na(translation)) %>%
    separate(translation, 
             c("w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8"), " ") %>%
    gather("word_id", "translation", -1:-2) %>%
    filter(!is.na(translation))
  
  words_with_model <- tidy_translation %>%
    left_join(model, by = c("translation" = "target_word"))
  
  target_words_in_model <- words_with_model %>%
    select(-translation) %>%
    group_by(target_word, translation_id) %>% # sum across word ids
    summarise_at(vars(V2:V301), sum, na.rm = TRUE) %>%
    group_by(target_word) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) %>% # mean across words 
    mutate(wiki_lang_code =  lang) %>%
    select(wiki_lang_code, target_word, everything())
  
    all_word_coordinates <-  target_words_in_model %>%
      bind_rows(these_centroids) 
    
    word_word_dists <- coop::cosine(t(as.matrix(all_word_coordinates[,-1:-2]))) # this is fast
    
    wide_word_word_dists <- word_word_dists %>%
      as.data.frame()  %>%
      mutate(word1 = all_word_coordinates$target_word) %>%
      select(word1, everything())
    
    names(wide_word_word_dists)  = c("word1", all_word_coordinates$target_word)
    
    long_word_word_dists <- gather(wide_word_word_dists, "word2", "cos_dist", -word1) %>%
      select(word1, word2, everything())
    
    crit_dists <- long_word_word_dists  %>%
      filter(word1 %in% c("male_targets", "female_targets")) %>%
      filter(!is.na(cos_dist)) %>%
      spread(word1, cos_dist) %>%
      mutate(male_score = male_targets - female_targets,
             wiki_lang_code = lang) %>%
      rename(target_word = word2) %>%
      select(wiki_lang_code,target_word, everything())
    
    write_csv(crit_dists, OUTFILE, append = T)
}

walk(LANGS,get_gender_scores, MODEL_PATH, all_translations, all_centroids, OUTFILE)