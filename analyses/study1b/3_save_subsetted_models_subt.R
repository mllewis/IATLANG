# load packages
library(tidyverse)
library(data.table)


INFILE <- "../../data/study2b/iat_translations_tidy.csv"
LANGKEY <- "../../data/study2b/country_langiso_langwiki_key.csv"
MODEL_PREFIX <- "/Volumes/wilbur_the_great/subtitle_models/"
OUTMODEL_PREFIX <- "../../data/study2b/subt_subsetted_models/"

lang_key <- read_csv(LANGKEY) %>%
  mutate(language = tolower(language_name)) %>%
  select(language, wiki_language_code) %>%
  distinct()

translations <- read_csv(INFILE)  %>%
    left_join(lang_key, by = "language") %>%
    select(wiki_language_code, word, gender, word_id, translation_id, translation) 

all_langs <- list.files(MODEL_PREFIX) %>%
  str_split("\\.") %>%
  map_chr(~.[2]) 

#### loop over languages and get word vectors ####
save_subsetted_model <- function(current_lang, 
                                 trans_df, 
                                 model_prefix, 
                                 out_model_prefix){
  
  print(paste0("===== ", current_lang, " ====="))
  model_path <- paste0(model_prefix, "sub.", current_lang, ".vec")
  # read in model from temp_filename
  model <- fread(model_path,    
                 skip = 1,
                 key = "V1",
                 encoding = "UTF-8",
                 data.table = TRUE,
                 verbose = F)
  
  # get model of the words we care about 
  translated_word_list <- trans_df %>%
    filter(wiki_language_code == current_lang)  %>%
    rename(language_code = wiki_language_code)
  
  relevant_vectors <- translated_word_list %>%
    merge(model  %>% rename(translation = V1),  # get vectors for relevant words only
          by = "translation", all.x = TRUE)
  
  # write raw vectors
  write_csv(relevant_vectors, paste0(out_model_prefix, "raw/sub.", current_lang, "_raw.csv"))
  
  calculated_vectors <- relevant_vectors %>%
    select(-translation) %>%
    group_by(language_code, word, gender, translation_id) %>% # sum across word ids
    summarise_at(vars(V2:V301), sum, na.rm = TRUE) %>%
    group_by(language_code, word, gender) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) # mean across words
  
  # write calculated vectors
  write_csv(calculated_vectors, paste0(out_model_prefix, "calculated/sub.", 
                                       current_lang, "_calculated.csv"))
  
}

 
# get all subsetted models
walk(all_langs, 
     save_subsetted_model, 
     translations, 
     MODEL_PREFIX,
     OUTMODEL_PREFIX)
