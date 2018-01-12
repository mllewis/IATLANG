## Called from bash script
# 
library(tidyverse)
library(feather)
source("IAT_utils2.R")

relevant_calculated_vector_path <- "../../data/models/wikipedia/subsetted_career_models/calculated/"
es_file_path <- "career_effect_sizes.csv"
model_file_name <- list.files("temp/")
model_file_full_path <- paste0("temp/",model_file_name[1])
current_lang <- model_file_name %>%
      str_split( "-") %>%
      unlist() %>%
     .[1]

word_list <- list(test_name = "WEAT_6", # not identical to caliskan (caliskan used proper names)
                  bias_type = "gender-bias-career-family",
                  category_1 = c("male", "man", "boy", "brother", "he", "him", "his", "son"),
                  category_2 = c("female", "woman", "girl", "sister", "she", "her", "hers", "daughter"),
                  attribute_1 = c("executive", "management", "professional", "corporation", "salary", 
                                  "office", "business", "career"),
                  attribute_2 = c("home", "parents", "children", "family", "cousins", "marriage", 
                                  "wedding", "relatives"))

tidy_translations <- read_csv("tidy_translations.csv")

tryCatch({
      # read in model from temp_filename
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
      
      
      if (is.na(ES)){
        # save the whole model only if something went wrong
        write_feather(model, paste0("failed_langs/", current_lang, "_fullmodel.feather"))
      } else {
        print(ES)
        # delete local file to save memory
        write_feather(model, paste0("zzzz_failed_langs/", current_lang, "_fullmodel.feather"))
        write_csv(ES, es_file_path, append = TRUE)
        #file.remove(model_file_full_path) # move this
      }
      file.rename(model_file_full_path, paste0("temp/zzzz_failed_langs/temp2/", current_lang, "-tempmodel.txt"))
      
}, error = function(e){
    NA
})

