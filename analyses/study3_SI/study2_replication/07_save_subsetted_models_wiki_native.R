# load packages
library(tidyverse)
library(data.table)
library(here)

print("save occupation subsetted models")

INFILE <- here("data/study2/occupation_translations_tidy_wiki.csv")
LANGKEY <- here("data/study0/processed/lang_name_to_wiki_iso.csv")
MODEL_PREFIX <- "/Volumes/wilbur_the_great/native_models/04_models/"
FULL_IAT_MODEL_PATH <- here("data/study3_SI/study1b_replication/05_subsetted_models/calculated/wiki.")
OUTMODEL_PREFIX <- here("data/study3_SI/study2_replication/wiki_native_calculated_models/")

IAT_GENDER_WORDS <-  c("male", "man", "boy", "brother", "he", "him", "his", "son",
                      "female", "woman", "girl", "sister", "she", "her", "hers", "daughter")

LANGS_TO_EXCLUDE <- c("zu") #based on study 1b

lang_key <- read_csv(LANGKEY)  %>%
  rename(language = language_name) %>%
  mutate(language = tolower(language))

translations <- read_csv(INFILE)  %>%
    mutate(language = case_when(language == "Serbian (latin)\n" ~ "serbian", TRUE ~ language)) %>%
    left_join(lang_key, by = "language") %>%
    select(wiki_language_code, word, gender, word_id, translation_id,
           translation)

all_langs <- unique(translations$wiki_language_code)  %>%
  setdiff(LANGS_TO_EXCLUDE)

#### loop over languages and get word vectors ####
save_subsetted_model_occupation <- function(current_lang, trans_df, model_prefix, other_target_words,
                                 other_model_prefix, out_model_prefix){

  print(paste0("===== ", current_lang, " ====="))
  model_path <- paste0(model_prefix, current_lang, "_native_wiki_corpus_dim20_minCount1_ep15.vec")
  # read in model from temp_filename
  model <- fread(model_path,
                 skip = 1,
                 key = "V1",
                 encoding = "UTF-8",
                 data.table = TRUE,
                 verbose = F,
                 quote = "",
                 nrows = 900000) #for large files

  # get model of the words we care about
  translated_word_list <- trans_df %>%
    filter(wiki_language_code == current_lang)  %>%
    rename(language_code = wiki_language_code)

  relevant_vectors <- translated_word_list %>%
    merge(model  %>% rename(translation = V1),  # get vectors for relevant words only
          by = "translation", all.x = TRUE)

  calculated_vectors <- relevant_vectors %>%
    select(-translation) %>%
    group_by(language_code, word, gender, translation_id) %>% # mean across word ids
    summarise_at(vars(V2:V201), mean, na.rm = TRUE) %>%
    group_by(language_code, word, gender) %>%
    summarize_at(vars(V2:V201), mean, na.rm = TRUE) # mean across words

  # get target IAT words from study 1b
  iat_model_path <- paste0(other_model_prefix, current_lang, "_calculated.csv")
  other_calculated_model <- read_csv(iat_model_path) %>%
    filter(word %in% other_target_words)

  all_calcuated_vectors <-  bind_rows(calculated_vectors, other_calculated_model)

  # write calculated vectors
  write_csv(all_calcuated_vectors, paste0(out_model_prefix, "/wiki_native.",
                                       current_lang, "_calculated_occupation.csv"))

}

# get all subsetted models
walk(all_langs,
     save_subsetted_model_occupation,
     translations,
     MODEL_PREFIX,
     IAT_GENDER_WORDS,
     FULL_IAT_MODEL_PATH,
     OUTMODEL_PREFIX)
