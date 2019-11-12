# IAT utility functions
require(lsa)

# effect size function on Caliskan pg 2 (top right)
get_sYXab <- function(df){
  sYXab_denom <- sd(df$swab, na.rm = TRUE)
  
  df %>%
    group_by(category_type) %>%
    summarize(mean_swab = mean(swab, na.rm = TRUE)) %>%
    spread(category_type, mean_swab) %>%
    summarize(sYXab_num = category_1 - category_2) %>%
    transmute(sYXab = sYXab_num/sYXab_denom) %>%
    unlist(use.names = FALSE)
}

# gets cosine distance 
get_word_distance_cos = function(model, w1 , w2){
  w1_vec <- filter(model, target_word == w1) %>% select(-1) %>% as.matrix()
  w2_vec <- filter(model, target_word == w2) %>% select(-1) %>% as.matrix()
  cosine(w1_vec[1,], w2_vec[1,]) # this comes from lsa package
}


# function on the top right of Caliskan pg 2
get_swabs <- function(df, model){
  df %>%
    rowwise() %>%
    mutate(cosine_sim = get_word_distance_cos(model, category_value, attribute_value)) %>%
    ungroup() %>% # gets rid of rowwise error message
    group_by(category_type, category_value, attribute_type) %>%
    summarize(word_attribute_mean = mean(cosine_sim, na.rm = TRUE)) %>%
    spread(attribute_type, word_attribute_mean) %>%
    mutate(swab = attribute_1 - attribute_2)
}


# gets df with each unique pairing of category and attribute values
prep_word_list <- function(word_list) {
  if (length(word_list) > 4) {
    word_list <- word_list[-1:-2]
  }
  cross_df(word_list) %>%
    mutate_all(tolower) %>%
    gather(key = "category_type", value = "category_value", category_1:category_2) %>%
    gather(key = "attribute_type", value = "attribute_value", attribute_1:attribute_2) %>%
    distinct(category_value, attribute_value, .keep_all = TRUE) 
}

get_swabs_only <- function(df, model, att_type) {
  
  df <- prep_word_list(df[-1:-2]) %>% 
    get_swabs(., model)
  
  sYXab_denom <- sd(df$swab, na.rm = TRUE)
  
  mean_swabs <-  df %>%
    group_by(category_type) %>%
    summarize(mean_swab = mean(swab, na.rm = TRUE)) 
  
  mean_swabs %>%
      mutate(attribute = att_type, 
             sd = sYXab_denom)
             
}

# function to loop over languages and get es for gendered languages
get_lang_es_separate_gender <- function(current_lang, 
                                        model_path, 
                                        word_list){
  
  print(paste0("====", current_lang, "===="))
  
  # read back in subsetted file
  file_path <- paste0(model_path, current_lang, "_calculated.csv")
  
  targ_words <- c(pluck(word_list$attribute_1), 
                    pluck(word_list$attribute_2),
                    pluck(word_list$category_1),
                    pluck(word_list$category_2))
  
  subsetted_model <- read_csv(file_path) %>%
    select(-language_code) %>%
    mutate(word = str_replace_all(word, "-", " ")) %>%
    rename(target_word = word) %>%
    filter(target_word %in% targ_words) # filter to only career words
  
  # check if it's a gendered langauge
  gendered_words <- c(pluck(word_list$attribute_1), pluck(word_list$attribute_2))
  multi_gender <- subsetted_model %>%
    filter(target_word %in% gendered_words) %>%
    distinct(gender) %>%
    nrow() > 1
  
  current_model <- ifelse(grepl("wiki.cc", model_path), "wiki_cc",
                          ifelse(grepl("europarl", model_path), "europarl",
                          ifelse(grepl("wiki", model_path), "wiki",
                          ifelse(grepl("sub", model_path), "sub", NA))))
  
  if(multi_gender){
    subsetted_model_f <- subsetted_model %>%
      filter(gender %in% c("F", "N")) %>%
      select(-gender)
    
    ES_F <- get_swabs_only(word_list, subsetted_model_f, "F") %>%
      mutate(gender = "F")
    
    subsetted_model_m <- subsetted_model %>%
      filter(gender %in% c("M", "N")) %>%
      select(-gender)
    
    ES_M <- get_swabs_only(word_list, subsetted_model_m, "M") %>%
      mutate(gender = "M")
    
   bind_rows(ES_F, ES_M) %>%
      mutate(model_source = current_model,
             language = current_lang) %>%
      select(language, model_source,  everything()) 
    
  } else {
    ES_N <- get_swabs_only(word_list, subsetted_model %>%
                             select(-gender), "N") %>%
      mutate(model_source = current_model,
             language = current_lang,
             gender = "N") %>%
      select(language, model_source, everything()) 
    ES_N
  }
}

# this is used for gender-free languages. This function is used in the Caliskan replication.
get_ES <- function(df, model) {
  
  print(pluck(df, "test_name"))
  
  es <- prep_word_list(df[-1:-2]) %>%
    get_swabs(., model) %>%
    get_sYXab()
  
  data.frame(test = pluck(df, "test_name"), 
             bias_type = pluck(df, "bias_type"),
             effect_size = es)
}