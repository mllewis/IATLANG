# For the raw and calculated vectors, get proportion missing for each language-model combo

library(tidyverse)
library(here)

print("get exclusions")

CUTOFF <- .2 # maximum prop missing from calculated vectors
FILEOUT <- here("exploratory_studies/16_wiki_native/data/06_iat_es/language_exclusions.csv")

WIKI_NATIVE_CALCULATED_PATH <- here("exploratory_studies/16_wiki_native/data/05_subsetted_models/calculated/")
WIKI_NATIVE_RAW_PATH <- here("exploratory_studies/16_wiki_native/data/05_subsetted_models/raw/")

TARGET_LANGS_PATH <- here("data/study0/processed/top_lang_by_country_ethnologue.csv")

get_prop_missing <- function(file_path){
  vec_df <- read_csv(file_path) %>%
    summarize(prop_na = sum(is.na(V2))/n())

  file_info <- unlist(str_split(str_split(file_path, "//")[[1]][2], "_|\\."))
  file_info <- file_info[file_info != "multiword"]
  this_model_type <- ifelse(file_info[2] == "cc", "wiki_cc", file_info[1])
  file_info <- file_info[file_info != "cc"]

  vec_df %>%
      mutate(model_type = this_model_type,
             language_code = file_info[2],
             vector_type = file_info[3])
}

all_missing <- c(list.files(WIKI_NATIVE_CALCULATED_PATH, full.names = T),
  list.files(WIKI_NATIVE_RAW_PATH, full.names = T)) %>%
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
  rename(exclude_wiki_native = wiki) %>%
  replace_na(list(exclude_wiki = FALSE))


write_csv(exclusions_tidy, FILEOUT)

