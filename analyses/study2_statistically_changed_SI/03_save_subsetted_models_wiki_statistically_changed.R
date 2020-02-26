# load packages (spanish only)
library(tidyverse)
library(data.table)
library(here)

print("save subsetted models")

INFILE <- here("data/study1b/iat_translations_tidy_wiki.csv")
LANGKEY <- here("data/study0/processed/lang_name_to_wiki_iso.csv")
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/es_native_wiki_corpus_dim200_minCount1_ep15_"
OUTMODEL_PREFIX <- here("data/study1b_statistically_changed_SI/wiki_statistically_changed_models/")

lang_key <- read_csv(LANGKEY)  %>%
  rename(language = language_name) %>%
  mutate(language = tolower(language))

translations <- read_csv(INFILE)  %>%
  left_join(lang_key, by = "language") %>%
  select(wiki_language_code, word, gender, word_id, translation_id,
         translation)

model_types <- c("masculinized", "feminized", "orig")

#### loop over languages and get word vectors ####
save_subsetted_model <- function(model_type, trans_df, current_lang, model_prefix, out_model_prefix){

  print(paste0("===== ", current_lang, " ====="))
  model_path <- paste0(model_prefix, model_type, ".vec")
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
  write_csv(relevant_vectors, paste0(out_model_prefix, "raw/statistically_manipulated_", current_lang,"_", model_type, "_raw.csv"))

  calculated_vectors <- relevant_vectors %>%
    select(-translation) %>%
    group_by(language_code, word, gender, translation_id) %>% # mean across word ids
    summarise_at(vars(V2:V201), mean, na.rm = TRUE) %>%
    group_by(language_code, word, gender) %>%
    summarize_at(vars(V2:V201), mean, na.rm = TRUE) # mean across words

  # write calculated vectors
  write_csv(calculated_vectors,paste0(out_model_prefix, "calculated/statistically_manipulated_", current_lang,"_", model_type, "_calculated.csv"))

}

# get all subsetted models
walk(model_types[3],
     save_subsetted_model,
     translations,
     "es",
     MODEL_PREFIX,
     OUTMODEL_PREFIX)

