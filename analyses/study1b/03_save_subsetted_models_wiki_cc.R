# load packages
library(tidyverse)
library(data.table)
library(here)

print("save subsetted models")

INFILE <- here("data/study1b/iat_translations_tidy_wiki.csv")
LANGKEY <- here("data/study0/processed/lang_name_to_wiki_iso.csv")
MODEL_PREFIX <- "/Volumes/wilbur_the_great/wiki_cc_models/wiki_cc."
OUTMODEL_PREFIX <- here("data/study1b/wiki_cc_subsetted_models/")

lang_key <- read_csv(LANGKEY)  %>%
  rename(language = language_name) %>%
  mutate(language = tolower(language))

translations <- read_csv(INFILE)  %>%
    left_join(lang_key, by = "language") %>%
    select(wiki_language_code, word, gender, word_id, translation_id, 
           translation) 

all_langs <- unique(translations$wiki_language_code)

#### loop over languages and get word vectors ####
save_subsetted_model <- function(current_lang, trans_df, model_prefix, out_model_prefix){
  
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
    merge(model  %>% rename(translation = V1),  # get vectors for relevant words only
          by = "translation", all.x = TRUE)
  
  # write raw vectors
  write_csv(relevant_vectors, paste0(out_model_prefix, "raw/wiki.cc.", current_lang, "_raw.csv"))
  
  calculated_vectors <- relevant_vectors %>%
    select(-translation) %>%
    group_by(language_code, word, gender, translation_id) %>% # mean across word ids
    summarise_at(vars(V2:V301), mean, na.rm = TRUE) %>%
    group_by(language_code, word, gender) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) # mean across words
  
  # write calculated vectors
  write_csv(calculated_vectors, paste0(out_model_prefix, "calculated/wiki.cc.", 
                                       current_lang, "_calculated.csv"))
  
}

all_langs <- c( "hu", "zu")
# get all subsetted models
walk(all_langs,
  #all_langs, #unique(translations$wiki_language_code), 
     save_subsetted_model, 
     translations, 
     MODEL_PREFIX,
     OUTMODEL_PREFIX)


