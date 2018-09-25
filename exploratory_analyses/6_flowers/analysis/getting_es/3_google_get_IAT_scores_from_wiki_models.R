## Get effect size measure from wiki embeddings for names ## 
# (career_effect_sizes_hand_translations.csv/career_effect_sizes_google_translations.csv) 
## lots of words, google translate
# load packages
library(tidyverse)
library(data.table)

source("IAT_utils.R")

################ set parameters ############
MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/wiki."

# google translations
TRANSLATION_PATH <- "../../data/tidy_google_translations_pleasantness.csv"
RAW_VECTOR_PATH <- "../../data/raw_vectors/google_pleasentness_" 
CALCULATED_VECTOR_PATH <- "../../data/calculated_vectors/" 
ES_OUTPUT_PATH <- "../../data/weapons_effect_sizes_google.csv"
IAT_NAME <- "pleasentness"

FLOWER_WORD_LIST <- list(test_name = "WEAT_1",
           bias_type = "flowers-insects-attitude",
           category_1 = c("aster", "clover", "hyacinth", "marigold", "poppy", 
                          "azalea", "crocus", "iris", "orchid", "rose", "bluebell",
                          "daffodil", "lilac", "pansy", "tulip", "buttercup", "daisy",
                          "lily", "peony", "violet", "carnation", "gladiola", "magnolia",
                          "petunia", "zinnia"),
           category_2 = c("ant", "caterpillar", "flea", "locust", "spider", "bedbug",
                          "centipede", "fly","maggot", "tarantula", "bee", "cockroach",
                          "gnat", "mosquito", "termite", "beetle", "cricket", "hornet",
                          "moth", "wasp", "blackfly", "dragonfly", "horsefly", "roach",
                          "weevil"),
           attribute_1 = c("caress", "freedom", "health", "love", "peace", "cheer",
                           "friend","heaven", "loyal", "pleasure", "diamond", "gentle", 
                           "honest", "lucky", "rainbow","diploma", "gift", "honor",
                           "miracle", "sunrise", "family", "happy", "laughter","paradise", 
                           "vacation"),
           attribute_2 = c("abuse" , "crash" , "filth" , "murder" , "sickness" , "accident" ,
                           "death" , "grief" , "poison" , "stink" , "assault" , "disaster" ,
                           "hatred" , "pollute" , "tragedy" , "divorce" , "jail" , "poverty" ,
                           "ugly" , "cancer" , "kill" , "rotten" , "vomit" , "agony" , "prison"))

#Greenwald, 1998: "Measuring Individual Differences in Implicit Cognition: The Implicit Association Test"
#universally accepted attitude towards instruments and weapons
WEAPONS_WORD_LIST <- list(test_name = "WEAT_2",
           bias_type = "instruments-weapons-attitude",
           category_1 = c("bagpipe","cello", "guitar", "lute", "trombone", "banjo", 
                          "clarinet", "harmonica", "mandolin", "trumpet", "bassoon", 
                          "drum", "harp", "oboe", "tuba", "bell", "fiddle", "harpsichord",
                          "piano", "viola", "bongo", "flute", "horn", "saxophone", "violin" ),
           category_2 = c("arrow", "club", "gun", "missile", "spear", "axe", "dagger", 
                          "harpoon", "pistol", "sword", "blade", "dynamite", "hatchet", 
                          "rifle", "tank", "bomb", "firearm", "knife", "shotgun", "teargas",
                          "cannon", "grenade", "mace", "slingshot", "whip" ),
           attribute_1 = c("caress", "freedom", "health", "love", "peace", "cheer", "friend",
                           "heaven", "loyal", "pleasure", "diamond", "gentle", "honest", 
                           "lucky", "rainbow","diploma", "gift", "honor", "miracle", "sunrise",
                           "family", "happy", "laughter","paradise", "vacation"),
           attribute_2 = c("abuse" , "crash" , "filth" , "murder" , "sickness" , "accident" , 
                           "death" , "grief" , "poison" , "stink" , "assault" , "disaster" , 
                           "hatred" , "pollute" , "tragedy" , "divorce" , "jail" , "poverty" , 
                           "ugly" , "cancer" , "kill" , "rotten" , "vomit" , "agony" , "prison"))

######################## read in language and word data ####################
countries_to_langs <- read_csv("../../data/language_names_to_wiki_codes.csv")$wiki_language_code
wiki_langs <- countries_to_langs[!is.na(countries_to_langs)]

tidy_translations <- read_csv(TRANSLATION_PATH)

################## loop over languages and get word vectors #################
save_subsetted_model <- function(current_lang, tidy_translations, model_prefix, raw_prefix, calculated_prefix){
  
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
  translated_word_list <- tidy_translations %>%
    filter(wiki_language_code == current_lang) %>%
    data.table()
  
  relevant_vectors <- translated_word_list %>%
    merge(model  %>% rename(translation = V1),  # get vectors for relevant words only
          by = "translation", all.x = TRUE)
  
  # write raw vectors
  write_csv(relevant_vectors, paste0(raw_prefix, "wiki.", current_lang, "_raw_", IAT_NAME, ".csv"))
  
  calculated_vectors <- relevant_vectors %>%
    select(-translation) %>%
    group_by(wiki_language_code, target_word, translation_id) %>% # sum across word ids
    summarise_at(vars(V2:V301), sum, na.rm = TRUE) %>%
    group_by(wiki_language_code, target_word) %>% 
    summarize_at(vars(V2:V301), mean, na.rm = TRUE) # mean across words
  
  # write calculated vectors
  write_csv(calculated_vectors, paste0(calculated_prefix, "wiki.", 
                                       current_lang, "_calculated_", IAT_NAME, ".csv"))
}

# # do the thing: get all subsetted models
# (full models from: https://github.com/facebookresearch/fastText/blob/master/pretrained-vectors.md)
walk(wiki_langs, save_subsetted_model, tidy_translations, MODEL_PREFIX, RAW_VECTOR_PATH, CALCULATED_VECTOR_PATH)

######################## loop over langs and get effect sizes ########################
get_wiki_es <- function(current_lang, calculated_prefix, output_path, word_list){
  
  # read back in subsetted file
  file_name <- paste0(calculated_prefix, "google_", IAT_NAME, "_wiki.", current_lang, "_calculated.csv")
  subsetted_model <- read_csv(file_name)%>%
    select(-wiki_language_code)
  
  if (nrow(subsetted_model) > 10){ # does the subsetted model exist?
  # calculate ES
    ES <- get_ES(word_list, subsetted_model) %>%
      mutate(language_code = current_lang) %>%
      select(language_code, everything())
  } else {
    ES <- data.frame(language_code = current_lang)
  }
  
  write_csv(ES, append = TRUE, path = output_path)
}

# do the thing
walk(wiki_langs, get_wiki_es, CALCULATED_VECTOR_PATH, ES_OUTPUT_PATH, WEAPONS_WORD_LIST)


######################## count missing  ########################
count_missingwf <- function(current_lang, calculated_prefix, output_path){
  
  # read back in subsetted file
  file_name <- paste0(calculated_prefix, "google_", IAT_NAME, "_wiki.", current_lang, "_calculated.csv")

  subsetted_model <- read_csv(file_name) %>%
    select(-wiki_language_code) %>%
    mutate(word_type = ifelse((target_word %in% flowers_words) & (target_word %in% weapon_words), "b",
                              ifelse(target_word %in% flowers_words, "f", "w"))) %>%
    mutate(missing = ifelse(V2 == 0, T, F)) %>%
    group_by(word_type) %>%
    summarize(prop_missing = length(which(missing))/n()) 
  
    prop_missing_flowers = mean(filter(subsetted_model, word_type %in% c("b", "f")) %>%
                                  select(prop_missing) %>% unlist())
    
    prop_missing_weapons = mean(filter(subsetted_model, word_type %in% c("w", "f")) %>%
                                  select(prop_missing) %>% unlist())
  data.frame(language_code = current_lang,
             prop_missing_flowers = prop_missing_flowers,
             prop_missing_weapons = prop_missing_weapons)
}

# do the thing

flowers_words <- c(FLOWER_WORD_LIST$category_1, FLOWER_WORD_LIST$category_2, 
                  FLOWER_WORD_LIST$attribute_1, FLOWER_WORD_LIST$attribute_2)


weapon_words <- c(WEAPONS_WORD_LIST$category_1, WEAPONS_WORD_LIST$category_2, 
                  WEAPONS_WORD_LIST$attribute_1, WEAPONS_WORD_LIST$attribute_2)


setwd("~/Documents/research/Projects/IATLANG/analyses/6_flowers/analysis/getting_es/")
CALCULATED_VECTOR_PATH <- "../../data/calculated_vectors/" 
missing_pleasentness <-  map_df(wiki_langs, count_missingwf, CALCULATED_VECTOR_PATH) 

count_missingwf <- function(current_lang, calculated_prefix, output_path){
  
  # read back in subsetted file
  file_name <- paste0(calculated_prefix, 
                      "wiki.", current_lang, "_calculated_career.csv")
  subsetted_model <- read_csv(file_name) %>%
    select(-wiki_language_code) %>%
    filter(V2 == 0)
  
  data.frame(language_code = current_lang,
             num_missing = nrow(subsetted_model))
  
}


setwd("/Users/mollylewis/Documents/research/Projects/IATLANG/writeup/cogsci2018/analysis/study2b/google_translate_and_names_analysis/")
CALCULATED_VECTOR_PATH <- "data/wiki_language_embeddings_career/calculated/google_"
missing_career <- map_df(wiki_langs, count_missing, CALCULATED_VECTOR_PATH) %>%
  mutate(prop_missing_career = num_missing/32) %>%
  arrange(-prop_missing_career)%>%
  select(-num_missing)


all_prop_missing <- left_join(missing_career, missing_pleasentness)  

write_csv(all_prop_missing, "prop_google_translate_missing.csv")





