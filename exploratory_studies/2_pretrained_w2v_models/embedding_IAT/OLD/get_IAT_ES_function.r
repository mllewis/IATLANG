## Called from bash script
# 
library(tidyverse)
library(feather)
source("IAT_utils2.R")

relevant_calculated_vector_path <- "../../data/models/wikipedia/subsetted_career_models/calculated/"
es_file_path <- "career_effect_sizes.csv"

word_list <- list(test_name = "WEAT_6", # not identical to caliskan (caliskan used proper names)
                  bias_type = "gender-bias-career-family",
                  category_1 = c("male", "man", "boy", "brother", "he", "him", "his", "son"),
                  category_2 = c("female", "woman", "girl", "sister", "she", "her", "hers", "daughter"),
                  attribute_1 = c("executive", "management", "professional", "corporation", "salary", 
                                  "office", "business", "career"),
                  attribute_2 = c("home", "parents", "children", "family", "cousins", "marriage", 
                                  "wedding", "relatives"))

tidy_translations <- read_csv("tidy_translations.csv")

countries <- c("ja")
walk(countries, get_model_and_ES)
get_model_and_ES <- function(current_lang){
  print(current_lang)
  
model_file_full_path <- paste0("temp/zzzz_failed_langs/complete_models/",current_lang, "-tempmodel.txt")

if (current_lang == "en"){
  model <- fastrtext::load_model("/Users/mollylewis/Documents/research/Projects/IATLANG/data/models/wikipedia/wiki.en.bin")
  subsetted_model <- fastrtext::get_word_vectors(model,  unique(tidy_translations$target_word)) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(target_word = rowname)
#tryCatch({
      # read in model from temp_filename
} else {
      model <- data.table::fread(model_file_full_path,
                     skip = 1,
                     key = "V1",
                     encoding = "UTF-8",
                     data.table = TRUE)
      
      # get model of the words we care about
      translated_word_list <- tidy_translations %>%
        filter(language_code == current_lang)

      
      # get subsetted model (processing mutliple words, etc) and write to file
      write_subsetted_models(translated_word_list, model, current_lang)
      
      # read back in subsetted file
      subsetted_model <- read_csv(paste0(relevant_calculated_vector_path,
                                         "wiki.", current_lang, "_calculated_career.csv")) %>%
        select(-language_code)
}
      
      # calculate ES
      ES <- tryCatch({
        get_ES(word_list, subsetted_model) %>%
          mutate(language_code = current_lang) %>%
          select(language_code, everything())
      }, warning = function(w) {
        NA
      }, error = function(e) {
        NA
      })
      
      

      write_csv(ES, es_file_path, append = TRUE)

      #file.rename(model_file_full_path, paste0("temp/zzzz_failed_langs/temp2/", current_lang, "-tempmodel.txt"))
      
#}, error = function(e){
#    NA
#})
}

