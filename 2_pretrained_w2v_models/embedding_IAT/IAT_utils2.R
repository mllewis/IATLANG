# IAT utility functions

write_subsetted_models <- function(word_list, model, lang_code){
  
  relevant_vectors <- word_list %>%
      merge(model  %>% rename(translation = V1),  # get vectors for relevant words only
                by = "translation", all.x = TRUE)
  
  # write raw vectors
  relevant_raw_vector_path <- "../../data/models/wikipedia/subsetted_career_models/raw/"
  write_csv(relevant_vectors, paste0(relevant_raw_vector_path, "wiki.", lang_code, "_raw_career.csv"))
  
  calculated_vectors <- relevant_vectors %>%
    select(-translation) %>%
    group_by(language_code, target_word, translation_id) %>%
    summarise_at(vars(V2:V301), sum) %>%
    group_by(language_code, target_word) %>%
    summarize_at(vars(V2:V301), mean)
  
  # write calculated vectors
  relevant_calculated_vector_path <- "../../data/models/wikipedia/subsetted_career_models/calculated/"
  write_csv(calculated_vectors, paste0(relevant_calculated_vector_path, "wiki.", 
                                       lang_code, "_calculated_career.csv"))
}

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
    summarize(word_attribute_mean = mean(cosine_sim)) %>%
    spread(attribute_type, word_attribute_mean) %>%
    mutate(swab = attribute_1 - attribute_2)
}


# effect size function on Caliskan pg 2 (top right)
get_sYXab <- function(df){
  sYXab_denom <- sd(df$swab)
  
  df %>%
    group_by(category_type) %>%
    summarize(mean_swab = mean(swab)) %>%
    spread(category_type, mean_swab) %>%
    summarize(sYXab_num = category_1 - category_2) %>%
    transmute(sYXab = sYXab_num/sYXab_denom) %>%
    unlist(use.names = FALSE)
}

# wrapper for ES function
get_ES <- function(df, model) {
  print(pluck(df, "test_name"))
  es <- prep_word_list(df[-1:-2]) %>%
    get_swabs(., model) %>%
    get_sYXab()
  
  data.frame(test = pluck(df, "test_name"), 
             bias_type = pluck(df, "bias_type"),
             effect_size = es)
}
