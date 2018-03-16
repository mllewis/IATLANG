# load packages
library(tidyverse)
library(data.table)


MODEL_PATH <- "wiki.en.vec"
OUTPUT_FILE <- "gender_score_output.csv"
TARGET_WORDS <- list(
  female_words = c("girl", "woman", "female", "she", "her", "hers"),
  male_words = c("boy", "man", "male", "he", "him", "his"))

#### Get list of target words from childrens books #####
montag_words <- read_csv("montag_words.csv") %>%
  bind_rows(data.frame(word = unlist(TARGET_WORDS, use.names = F))) %>%
  rename(target_word = "word")

####### Get gender es for english #########
model <- fread( 
  MODEL_PATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("target_word", 
                unlist(lapply(2:301, function(x) paste0("V", x)))))


model_filtered <- model[target_word %in% montag_words$target_word]

# about 300 words missing (mostly contractions/non-words)
#missing <- merge(model, montag_words, all.y=TRUE) %>%
#  filter(is.na(V2)) %>% select(target_word)


### FUNCTIONS TO GET DISTANCEES
# gets cosine distance 
get_word_distance_cos = function(w1, w2, model){
  w1_vec <- filter(model, target_word == tolower(w1)) %>% select(-1) %>% as.matrix()
  w2_vec <- filter(model, target_word == tolower(w2)) %>% select(-1) %>% as.matrix()
  if (dim(w2_vec)[1] > 0){
    lsa::cosine(w1_vec[1,], w2_vec[1,])
  } else{
    NA
  }
}

# get mean dist to m/f words
get_mean_dist_to_targets <- function(target_words, current_word, modelf){
  # get female score
  female_score <- map(target_words$female_words, get_word_distance_cos, 
                         current_word, modelf) %>%
                    unlist() %>%
                    mean()
  # get male score
  male_score <- map(target_words$male_words, get_word_distance_cos, 
                        current_word, modelf) %>%
                      unlist() %>%
                      mean()
  
  data.frame(word = current_word,
             female_score = female_score,
             male_score = male_score,
             gender_score = male_score - female_score)
}

# wrapper function
get_gender_score <- function(current_word, modelf, output_file, target_words){
  print(current_word)
  dists <- get_mean_dist_to_targets(target_words, current_word, modelf)
  write_csv(dists, append = TRUE, path = output_file)
}


walk(montag_words$target_word, get_gender_score, model_filtered, 
     OUTPUT_FILE, TARGET_WORDS) 


