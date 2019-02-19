# get frequency measure 

library(tidyverse)
library(here)
library(data.table)
library(gtrendsR)
library(reticulate)

use_python("/usr/local/bin/python")
google_package <- reticulate::import("google")
gensim_package <- reticulate::import("gensim")

INFILE <- here('data/study2/occupation_translations_tidy.csv')
LANGKEY <- here("data/study0/processed/lang_name_to_wiki_iso.csv")
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/wiki."
OUTFILE <- here('data/study2/occupation_gender_freqs.csv')

google_package$search("Geeksforgeeks")

raw_df <- gtrends(keyword = "nurssse", 
        gprop = "web",
        time = "today 12-m") 
 
raw_hits <- raw_df$interest_over_time %>% 
    pull(hits) 

googlesearch import search 

tidy_occs <- read_csv(INFILE)

lang_key <- read_csv(LANGKEY)  %>%
  rename(language = language_name) %>%
  mutate(language = tolower(language)) 

translations <- read_csv(INFILE)  %>%
  mutate(language = case_when(language == "Serbian (latin)\n" ~ "serbian (latin)",
                              TRUE ~ language)) %>%
  left_join(lang_key, by = "language")

all_langs <- list.files(MODEL_PREFIX) %>%
  str_split("\\.") %>%
  map_chr(~.[2]) 


#### loop over languages and get word vectors ####
get_freq_in_from_model <- function(current_lang, trans_df, model_prefix, outmodel_path){
  
  print(paste0("===== ", current_lang, " ====="))
  model_path <- paste0(model_prefix, current_lang, ".vec")
  # read in model from temp_filename
  model <- fread(model_path,    
                 skip = 1,
                 key = "V1",
                 encoding = "UTF-8",
                 data.table = TRUE,
                 verbose = F,
                 quote = "")
  
  # get model of the words we care about 
  translated_word_list <- trans_df %>%
    filter(wiki_language_code == current_lang)  %>%
    rename(language_code = wiki_language_code)

  relevant_vectors <- translated_word_list %>%
    merge(model  %>% rename(translation = V1) %>%
            mutate(rownum = 1:n()),  # get vectors for relevant words only
          by = "translation", all.x = TRUE) %>%
    select(translation, rownum)
  
  # write raw vectors
  write_csv(relevant_vectors, paste0(out_model_prefix, "raw/wiki.", current_lang, "_raw.csv"))
  
  calculated_vectors <- relevant_vectors %>%
    select(-translation) %>%
    group_by(language_code, word, gender, translation_id) %>% # mean across word ids
    summarise_at(vars(V2:V301), mean, na.rm = TRUE) %>%
    group_by(language_code, word, gender) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) # mean across words
  
  # write calculated vectors
  write_csv(calculated_vectors, paste0(out_model_prefix, "calculated/wiki.", 
                                       current_lang, "_calculated.csv"))
  
}

# get all subsetted models
map(all_langs, 
     get_freq_in_from_model, 
     translations, 
     MODEL_PREFIX,
     OUTFILE)



write_csv(XX, OUTFILE)

