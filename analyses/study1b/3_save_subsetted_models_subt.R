# load packages
library(tidyverse)
library(data.table)
library(here)

print("save subsetted models")

INFILE <- here("data/study1b/iat_translations_tidy_subt.csv")
LANGKEY <- here("data/study0/processed/lang_name_to_wiki_iso.csv")
MODEL_PREFIX <- "/Volumes/wilbur_the_great/subtitle_models/"
OUTMODEL_PREFIX <- here("data/study1b/subt_subsetted_models/")

lang_key <- read_csv(LANGKEY)  %>%
  rename(language = language_name) %>%
  mutate(language = tolower(language))

translations <- read_csv(INFILE)  %>%
    left_join(lang_key, by = "language") %>%
    select(wiki_language_code, language, word, gender,
           translation_id, translation, translation_type) 

all_langs <- list.files(MODEL_PREFIX) %>%
  str_split("\\.") %>%
  map_chr(~.[2]) 

#### loop over languages and get word vectors ####
get_multi <- function(multi_df){
  
 # print(multi_df)
  # check if concatenated exists
  concats <- multi_df %>%
    filter(translation_type == "concatenated_multi", !is.na(V2))
  
  # if concatenated version exits return it, otherwise return individual words
  if(nrow(concats) > 0){
    tidy_multi_df <-  multi_df %>%
      filter(translation_type == "concatenated_multi")
  } else {
    tidy_multi_df <- multi_df %>%
      filter(translation_type != "concatenated_multi") 
  }
  tidy_multi_df
}

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
  
  all_relevant_vectors <- translated_word_list %>%
    merge(model  %>% rename(translation = V1),  # get vectors for relevant words only
          by = "translation", all.x = TRUE)

  single_cases <- all_relevant_vectors %>%
    filter(translation_type == "single")
  
  # get concatenated phrase if present, otherwise get individual words
  multi_cases <- all_relevant_vectors %>%
    filter(translation_type != "single") %>%
    group_by(word, translation_id) %>%
    nest() %>%
    mutate(x =  map(data, get_multi)) %>%
    unnest(x)
  
  # combine multi and invidual word cases (if multi exists)
  if(nrow(multi_cases) > 0){
    
   processed_vectors <-  single_cases %>%
      bind_rows(multi_cases) %>%
      arrange(language_code, word, translation_id)  %>%
      select(-translation_type)
  
  } else {
    
    processed_vectors <-  single_cases %>%
      arrange(language_code, word, translation_id)  %>%
      select(-translation_type)
  }
  
  # write raw vectors
  write_csv(processed_vectors, paste0(out_model_prefix, "raw/sub.multiword.", current_lang, "_raw.csv"))
  
  calculated_vectors <- processed_vectors %>%
    select(-translation) %>%
    group_by(language_code, word, gender, translation_id) %>% # mean across word ids
    summarise_at(vars(V2:V301), mean, na.rm = TRUE) %>%
    group_by(language_code, word, gender) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) # mean across words
  
  # write calculated vectors
  write_csv(calculated_vectors, paste0(out_model_prefix, "calculated/sub.multiword.", 
                                       current_lang, "_calculated.csv"))
  
}

# get all subsetted models
walk("ar", 
     save_subsetted_model, 
     translations, 
     MODEL_PREFIX,
     OUTMODEL_PREFIX)
