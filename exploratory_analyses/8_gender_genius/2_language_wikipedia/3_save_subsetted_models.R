# load packages
library(tidyverse)
library(data.table)


INFILE <- "data/tidy_iat_translations.csv"
LANGKEY <- "../../../data/other/country_langiso_langwiki_key.csv"
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/wiki."

lang_key <- read_csv(LANGKEY) %>%
  mutate(language = tolower(language_name)) %>%
  select(language, wiki_language_code)%>%
  add_row(language = "tamil", wiki_language_code = "ta") %>%
  distinct()

translations <- read_csv(INFILE)  %>%
    left_join(lang_key, by = "language") %>%
    select(wiki_language_code, word, gender, word_id, translation_id, translation) 

#### loop over languages and get word vectors ####
save_subsetted_model <- function(current_lang, trans_df, model_prefix){
  
  print(paste0("===== ", current_lang, " ====="))
  model_path <- paste0(model_prefix, current_lang, ".vec")
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
  relevant_raw_vector_path <- "data/subsetted_models/raw/"
  write_csv(relevant_vectors, paste0(relevant_raw_vector_path, "wiki.", current_lang, "_raw.csv"))
  
  calculated_vectors <- relevant_vectors %>%
    select(-translation) %>%
    group_by(language_code, word, gender, translation_id) %>% # sum across word ids/ or mean?
    #summarise_at(vars(V2:V301), mean, na.rm = TRUE) %>%
    summarise_at(vars(V2:V301), sum, na.rm = TRUE) %>%
    group_by(language_code, word, gender) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) # mean across words
  
  # write calculated vectors
  relevant_calculated_vector_path <-  "data/subsetted_models/calculated/"
  write_csv(calculated_vectors, paste0(relevant_calculated_vector_path, "wiki.", 
                                       current_lang, "_calculated_sum.csv"))
  
}

# get all subsetted models
#unique(translations$wiki_language_code),
walk(unique(translations$wiki_language_code)[-26], save_subsetted_model, translations, MODEL_PREFIX)
