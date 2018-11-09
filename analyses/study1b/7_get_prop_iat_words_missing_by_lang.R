# For the raw and calculated vectors, get proportion missing for each language-model combo

library(tidyverse)
library(here)

print("get exclusions")

CUTOFF <- .2 # maximum prop missing from calculated vectors
FILEOUT <- here("data/study1b/language_exclusions.csv") 

SUB_CALCULATED_PATH <- here("data/study1b/subt_subsetted_models/calculated/")
SUB_RAW_PATH <- here("data/study1b/subt_subsetted_models/raw/")
WIKI_CALCULATED_PATH <- here("data/study1b/wiki_subsetted_models/calculated/")
WIKI_RAW_PATH <- here("data/study1b/wiki_subsetted_models/raw/")
TARGET_LANGS_PATH <- here("data/study0/processed/top_lang_by_country_ethnologue.csv")

get_prop_missing <- function(file_path){
  vec_df <- read_csv(file_path) %>%
    summarize(prop_na = sum(is.na(V2))/n())
    
  file_info <- unlist(str_split(str_split(file_path, "//")[[1]][2], "_|\\."))
  file_info <- file_info[file_info != "multiword"]
  
  vec_df %>%
      mutate(model_type = file_info[1],
             language_code = file_info[2],
             vector_type = file_info[3])
}

all_missing <- c(list.files(SUB_CALCULATED_PATH, full.names = T),
  list.files(SUB_RAW_PATH, full.names = T),
  list.files(WIKI_CALCULATED_PATH,full.names = T),
  list.files(WIKI_RAW_PATH, full.names = T)) %>%
  map_df(get_prop_missing) 

target_langs <- read_csv(TARGET_LANGS_PATH) 

all_missing_tidy <- all_missing %>%
  filter(language_code %in% c(target_langs$wiki_language_code, "sr")) %>% # sr/hr count as one language (averaged together)
  complete(language_code, model_type, vector_type) %>%
  mutate_if(is.character, as.factor)  %>%
  mutate(prop_na = replace_na(prop_na, 1)) 

exclusions <- all_missing_tidy %>% 
  filter(vector_type == "calculated") %>%
  filter(prop_na > CUTOFF)

exclusions_tidy <- exclusions %>%
  select(language_code, model_type) %>%
  mutate(value = TRUE) %>%
  spread("model_type", "value") %>%
  rename(exclude_sub = sub, 
         exclude_wiki = wiki) %>%
  replace_na(list(exclude_sub = FALSE, exclude_wiki = FALSE))


write_csv(exclusions_tidy, FILEOUT)

  