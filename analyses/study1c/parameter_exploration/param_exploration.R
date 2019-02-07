

train_model_get_iat <- function(these_params, this_corpus, these_targ_words, these_stims,
                                temp_path, es_path, model_name){
  # parameters
  this_word_count <- these_params$wc[1]
  this_vector_size <- these_params$vs[1]
  this_ngrams <- these_params$ng[1]
  this_window_size <- these_params$ws[1]
  
  # train and save model to google drives
  this_model <- train_the_model(this_corpus, this_word_count, this_vector_size, this_ngrams, this_window_size)
  write_csv(this_model, temp_path)
  full_out_path <- paste("trained_models/", model_name, "model", this_word_count, this_vector_size, 
                          this_ngrams, this_window_size, sep = "_")
  drive_upload(temp_path, full_out_path)
  
  # calculate and save effect sizes
  these_effect_sizes <- get_effect_sizes_for_given_model(this_model, these_targ_words, these_stims)
  
  tidy_es <- these_effect_sizes %>%
    mutate(model = model_name,
           word_count = this_word_count, 
           vector_size = this_vector_size, 
           ngrams = this_ngrams, 
           this_window_size)

  write_csv(tidy_es, es_path, append = T)
}

train_the_model <- function(corpus, this_wc, this_vs, this_ng, this_ws){
  
  min_gram <- ifelse(!this_ng, 1, 3)
  max_gram <- ifelse(!this_ng, 1, 5)
  
  tmp_file_txt <- tempfile()
  tmp_file_model <- tempfile()
  writeLines(text = corpus, con = tmp_file_txt)
  execute(commands = c("skipgram",
                       #set the input and output files
                       "-input", tmp_file_txt,
                       "-output", tmp_file_model,
                       #set the window size if needed, default is 5
                       "-ws", this_ws,
                       #min length of char ngram, default is 3
                       "-minn", min_gram,
                       #max length of char ngram, default is 6, minn and maxn both as 1 if don’t want to use ngrams
                       "-maxn", max_gram,
                       #minimal number of word occurrences, default is 5, can set to 100 instead
                       "-minCount", this_wc,
                       #max length of word ngram, default is 1
                       #”-wordNgrams",1,
                       #number of epochs, default is 5
                       #"-epoch",5,
                       #set the number of dimensions, default is 100
                       "-dim", this_vs,
                       "-verbose", 1))
  
  model <- load_model(tmp_file_model)
  vectors <- get_word_vectors(model) %>%
    as.data.frame() %>%
    rownames_to_column("target_word")
  
  vectors
}

get_effect_sizes_for_given_model <- function(current_model, word_df, stim_sets){
  
  # check that all words in model
  print(paste0("Num words missing from model: ", setdiff(word_df$stim_name, current_model$target_word)))
  
  # get es
  effect_sizes <- map_df(stim_sets, get_ES, current_model)  
  
  effect_sizes
}
