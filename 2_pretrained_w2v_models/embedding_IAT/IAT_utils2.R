# IAT utility functions


# gets df with each unique pairing of category and attribute
prep_word_list <- function(word_list) {
  if (length(word_list) > 4) {
    word_list <- word_list[-1:-2]
  }
  cross_df(word_list) %>%
    gather(key = "category_type", value = "category_value", category_1:category_2) %>%
    gather(key = "attribute_type", value = "attribute_value", attribute_1:attribute_2) %>%
    distinct(category_value, attribute_value, .keep_all = TRUE) 
}

# gets cosine distance 
get_word_distance_cos = function(model, w1 , w2){
  w1_vec <- filter(model, target_word == w1) %>% select(-1) %>% as.matrix()
  w2_vec <- filter(model, target_word == w2) %>% select(-1) %>% as.matrix()
  #(crossprod(w1_vec[1,], w2_vec[1,])/sqrt(crossprod(w1_vec[1,]) * crossprod(w2_vec[1,])))[1]
  lsa::cosine(w1_vec[1,], w2_vec[1,])
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

# wrapper for ES function
get_ES <- function(df, model) {

    es <- prep_word_list(df[-1:-2]) %>%
    get_swabs(., model) %>%
    get_sYXab()
  
  data.frame(test = pluck(df, "test_name"), 
             bias_type = pluck(df, "bias_type"),
             effect_size = es)
}
