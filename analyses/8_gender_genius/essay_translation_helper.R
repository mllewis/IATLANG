## helper functions for essay translations

### get_essay_words_translation
### get_essay_vector_in_lang
### get_one_essay_location (called by get_essay_vector_in_lang)
### get_lang_mod_centroids
### get_dist_from_prompt

# translate prompt using google translate
get_essay_words_translation <- function(targ_lang, word, output_path){

  if(targ_lang == "ar"){print(as.character(word))}
  
  if (targ_lang == "en"){
    translated_text <- word
  } else {
    translated_text <- gl_translate(as.character(word), 
                                    target = targ_lang, 
                                    source = "en")$translatedText
  }
  
  new_row <- data.frame(lang = targ_lang,
             target_word = word,
             translated_word = translated_text)
  
  write_csv(new_row, append = TRUE, path = output_path)
}

# get location of essay in the vector space of a particular language (called by get_essay_vector_in_lang)
get_one_essay_location <- function(this_essay_id, 
                                   lang_word_vecs, 
                                   essays, 
                                   model_lang){
  
  # merge current essay with corresponding word vecs
  merged_vectors <- essays[essay_id == this_essay_id] %>%
    merge(lang_word_vecs, all.x = TRUE, by = "translated_word")
  
  essay_lang <- merged_vectors$essayL1_google_lang_name[1]
  
  # get pieces to essay vector computation
  word_vectors <- merged_vectors %>%  # get word vectors only
    select(V2:V301) %>%
    as.matrix()
  counts <- merged_vectors$count # get counts
  L1_weights <- merged_vectors$L1_freq_weight # get L1 freq weights
  
  # weighted by number of times word appears in essay
  count_weighted_vecs <- word_vectors * counts # weight by number of times word appears
  essay_vector_count_weighted <- t(colSums(count_weighted_vecs, na.rm = TRUE)) # sum across words
  
  # also weighted by frequency of word in L1 essays
  count_and_L1freq_weighted_vecs <- count_weighted_vecs * L1_weights
  essay_vector_count_and_L1freq_weighted <- t(colSums(count_and_L1freq_weighted_vecs, na.rm = TRUE)) # sum across words
  
  # return essay vector
  count_weighting_vec <- data.frame(model_lang = model_lang,
                                    essay_lang = essay_lang,
                                    essay_id = this_essay_id,
                                    word_weighting = "unweighted", # only by num words in essay
                                    essay_vector_count_weighted)
  
  L1_and_count_weighting_vec <- data.frame(model_lang = model_lang, 
                                           essay_lang = essay_lang,
                                           essay_id = this_essay_id,
                                           word_weighting = "L1_weighted", # num words in essay and frequency
                                           essay_vector_count_and_L1freq_weighted)
  
  bind_rows(count_weighting_vec, L1_and_count_weighting_vec)
}


# get vectors from fasttext model for target words 
# reads in fasttext model, returns 2 vectors for each essay in a model space (count weighted)
# and freq + count weighted)
get_essay_vector_in_lang <- function(model_lang, translated_words, essays){
  print(paste0("getting word vectors for ", model_lang, " language"))
  
  # read in wiki model
  MODEL_PREFIX <- "/Volumes/wilbur_the_great/fasttext_models/wiki."
  
  file_name <- paste0(MODEL_PREFIX, model_lang, ".vec")
  
  lang_word_vecs <- fread(
    file_name,
    header = FALSE,
    skip = 1,
    quote = "",
    key = "translated_word",
    encoding = "UTF-8",
    data.table = TRUE,
    col.names = c("translated_word", 
                  unlist(lapply(2:301, function(x) paste0("V",x)))))
  
  setkey(lang_word_vecs, translated_word)
  
  # get translation equivalents for each word in each essay
  lang_translated_words <- translated_words[google_lang_name == model_lang]
  lang_translated_words[,google_lang_name:=NULL]
  essays_with_lang_translation <- merge(essays, 
                                        lang_translated_words, 
                                        by = "target_word",
                                        all.x = TRUE)
  
  setkey(essays_with_lang_translation, translated_word)
  
  # loop over essays to get vectors
  all_essay_ids <- unique(essays_with_lang_translation$essay_id)
  
  essay_vectors <- map_df(all_essay_ids, 
         get_one_essay_location, 
         lang_word_vecs, 
         essays_with_lang_translation,
         model_lang)

  rm(lang_word_vecs)
  
  output_filename <- paste0("../../../data/processed/translated_essay_vectors/", 
                            model_lang, "_essay_vectors.feather")
  write_feather(essay_vectors, output_filename)
}

# for all essays from one L1 in one model lang for one prompt, 
# get centroid of all essays 
get_lang_mod_centroids <- function(this_prompt_id, 
                                   this_model_lang, 
                                   this_essay_lang, 
                                   essays){
  
  print(paste(this_prompt_id, this_model_lang, this_essay_lang, sep = "_"))
  
  vecs <- essays[(model_lang == this_model_lang) & 
                 (essay_lang == this_essay_lang) &
                 (prompt_id == this_prompt_id)]
  
  group_centroid <- colMeans(vecs[,V2:V301])
  
  data.frame(model_lang = this_model_lang,
              essay_lang = this_essay_lang,
              prompt_id = this_prompt_id,
             t(group_centroid))
}

# for all essays from one L1 in one model lang for one prompt
# get cosine dist to relevant prompt
get_dist_from_prompt <- function(this_prompt_id, 
                                 this_model_lang, 
                                 this_essay_lang, 
                                 essay_centroids,
                                 prompt_centroids){
  
  print(paste(this_prompt_id, this_model_lang, this_essay_lang, sep = "_"))
  
  
  this_prompt_vector <- prompt_centroids %>% 
                              filter(prompt_id == this_prompt_id,
                                     lang == this_model_lang) %>%
    select(-lang, -prompt_id) 
  
  this_essay_centroid_vector <- essay_centroids[(model_lang == this_model_lang) & 
                                                (essay_lang == this_essay_lang) &
                                                (prompt_id == this_prompt_id)]  %>%
    select(-model_lang, -essay_lang, -prompt_id) 
  
  # get distances
   dist_from_prompt <- lsa::cosine(as.vector(unlist(this_prompt_vector)),
              as.vector(unlist(this_essay_centroid_vector)))[1,1]
  
  #dist_from_prompt <- dist(as.matrix(rbind(unlist(this_prompt_vector),
  #                                   unlist(this_essay_centroid_vector))))[1]
  
  data.frame(model_lang = this_model_lang,
             essay_lang = this_essay_lang,
             prompt_id = this_prompt_id,
             cosine_dist = dist_from_prompt)
}

