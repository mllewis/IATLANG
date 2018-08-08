# load packages
library(tidyverse)
source("IAT_utils2.R")

LANGS <- c("fa", "de", "nl", "pt", "zh", "es", "id", "da", "it", "ko", "ru", "tr", "he", "ro", "ja", "ar", "fi", 
           "fr", "ms", "pl", "sv")

#### loop over langs and get effect sizes #### 
get_wiki_es_separate_gender <- function(current_lang, word_list){
  
  # read back in subsetted file
  relevant_calculated_vector_path <- "subsetted_models/calculated/"
  
  subsetted_model <- read_csv(paste0(relevant_calculated_vector_path, 
                                     "wiki.", current_lang, "_calculated.csv")) %>%
    select(-language_code) %>%
    mutate(word = str_replace_all(word, "-", " ")) %>%
    rename(target_word = word)
  
  # check if it's a gendered langauge
  gendered_words <- c(pluck(word_list$attribute_1), pluck(word_list$attribute_2))
  multi_gender <- subsetted_model %>%
    filter(target_word %in% gendered_words) %>%
    distinct(gender) %>%
    nrow() > 1
  
  if(multi_gender){
    subsetted_model_f <- subsetted_model %>%
      filter(gender %in% c("F", "N")) %>%
      select(-gender)
    
    ES_F <- get_swabs_only(word_list, subsetted_model_f, "F") %>%
      mutate(language_code = current_lang,
             gender = "F") %>%
      select(language_code, everything()) 
    
    subsetted_model_m <- subsetted_model %>%
      filter(gender %in% c("M", "N")) %>%
      select(-gender)
    
    ES_M<- get_swabs_only(word_list, subsetted_model_m, "M") %>%
      mutate(language_code = current_lang,
             gender = "M") %>%
      select(language_code, everything()) 
    
    bind_rows(ES_F, ES_M)
    
  } else {
    ES_N <- get_swabs_only(word_list, subsetted_model %>%
                     select(-gender), "N") %>%
      mutate(language_code = current_lang,
             gender = "N") %>%
      select(language_code, everything()) 
    ES_N
    
  }
  
}


### Career ###
OUTFILE_CAREER <- "es_career_separate_no_pronouns.csv"

word_list_career <- list(test_name = "WEAT_6", # not identical to caliskan (caliskan used proper names)
                    bias_type = "gender-bias-career-family",
                    #category_1 = c("male", "man", "boy", "brother", "he", "him", "his", "son"),
                    #category_2 = c("female", "woman", "girl", "sister", "she", "her", "hers", "daughter"),
                    category_1 = c("male", "man", "boy", "brother","son"),
                    category_2 = c("female", "woman", "girl", "sister",  "daughter"),
                    attribute_1 = c("executive", "management", "professional", "corporation", "salary", 
                                    "office", "business", "career"),
                    attribute_2 = c("home", "parents", "children", "family", "cousins", "marriage", 
                                    "wedding", "relatives"))

swabs_career <- map_df(LANGS, get_wiki_es_separate_gender, 
                word_list_career)

es_career_swabs_career <- swabs_career %>%
  filter(!(category_type == "category_1" & attribute == "F")) %>%
  filter(!(category_type == "category_2" & attribute == "M")) %>%
  select(-attribute, -gender) %>%
  group_by(language_code) %>%
  spread(category_type, mean_swab) %>%
  summarize_all(mean, na.rm = T) %>%
  mutate(sYXab_num = category_1 - category_2,
         sYXab = sYXab_num/sd)  %>%
  mutate(test = pluck(word_list_career, "test_name"), 
        bias_type = pluck(word_list_career, "bias_type")) %>%
  select(language_code, test, bias_type, sYXab)

write_csv(es_career_swabs_career, OUTFILE_CAREER)

### Genius ###
OUTFILE_GENIUS <- "es_genius_separate_no_pronouns.csv"

word_list_genius <- list(test_name = "genius_gender", 
                         bias_type = "genius_gender",
                         #category_1 = c("male", "man", "he", "him", "his"),
                         #category_2 = c("female", "woman",  "she", "her", "hers"),
                         category_1 = c("male", "man"),
                         category_2 = c("female", "woman"),
                         attribute_1 = c("genius", "brilliant", "super smart"),
                         attribute_2 = c("creative", "artistic", "super imaginative"))

LANGS2 <- c("fa", "de", "nl",  "zh", "es", "id", "da", "it", "ko", "ru", "tr", "he", "ro")


swabs_genius <- map_df(LANGS2, get_wiki_es_separate_gender, 
                       word_list_genius)

es_genius_swabs_genius <- swabs_genius %>%
  filter(!(category_type == "category_1" & attribute == "F")) %>%
  filter(!(category_type == "category_2" & attribute == "M")) %>%
  select(-attribute, -gender) %>%
  group_by(language_code) %>%
  spread(category_type, mean_swab) %>%
  summarize_all(mean, na.rm = T) %>%
  mutate(sYXab_num = category_1 - category_2,
         sYXab = sYXab_num/sd)  %>%
  mutate(test = pluck(word_list_genius, "test_name"), 
         bias_type = pluck(word_list_genius, "bias_type")) %>%
  select(language_code, test, bias_type, sYXab)

write_csv(es_genius_swabs_genius, OUTFILE_GENIUS)
