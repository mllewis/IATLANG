# get gender ratings from models
library(tidyverse)
library(here)
library(data.table)


MALE_WORDS <- c("son", "his","him","he", "brother","boy", "man") 
FEMALE_WORDS <- c("daughter", "hers", "her", "she",  "sister", "girl", "woman")
FILE_OUTPATH <- here("exploratory_studies/13_europarl/gender_ratings/")
FILE_INPATH <- "/Users/mollylewis/Downloads/europarl_models/"
GENDER_NORMS <- here("data/study1a/raw/GlasgowNorms.csv")

glasgow_norms <- read_csv(GENDER_NORMS) %>%
  select(word) %>%
  rowwise() %>%
  mutate(word =  str_split(word, " ", simplify = T)[1],
         word = tolower(word)) %>%
  distinct() %>%
  ungroup()

get_gender_score <- function(this_word, all_words_df){
  print(this_word)
  mat <- all_words_df %>%
    filter((word == this_word & type == "target") | type == "anchor")
  
  word_word_dists <- coop::cosine(t(as.matrix(mat[,c(-1, -2, -3)])))
  
  wide_word_word_dists <- word_word_dists %>%
    as.data.frame()  %>%
    mutate(word1 =  mat$word,
           gender_type = mat$gender_type) %>%
    select(word1,gender_type, everything())
  
  names(wide_word_word_dists)  = c("word1", "gender_type", mat$word)
  
  long_word_word_dists <- gather(wide_word_word_dists, "word2", "cos_dist", -word1, -gender_type) %>%
    select(word1, word2, gender_type, everything()) %>%
    filter(word2 == this_word & word1 != this_word) 
  
  try({
    long_word_word_dists %>%
      group_by(gender_type) %>%
      summarize(mean_cos_dist = mean(cos_dist)) %>%
      spread(gender_type, mean_cos_dist)  %>%
      mutate(male_score = male - female,
             word = this_word) %>%
      rename(female_target = female,
             male_target = male) %>%
      select(word, everything())
  })
}


get_gender_scores_from_model <- function(infile_path, targ_words, outfile_path, mw, fw){
  
  current_model <- fread(
    infile_path,
    header = FALSE,
    skip = 1,
    quote = "",
    encoding = "UTF-8",
    data.table = TRUE,
    col.names = c("word",
                  unlist(lapply(2:301, function(x) paste0("V", x)))))
  
  target_word_coordinates <-  targ_words %>%
    left_join(current_model)  %>%
    mutate(type = "target", 
           gender_type = NA) %>%
    filter(!is.na(V2)) %>%
    filter(!(word %in% c(mw, fw)))
  
  anchor_word_coordinates <- current_model %>%
    filter(word %in% c(mw, fw)) %>%
    mutate(type = "anchor",
           gender_type = ifelse(word %in% mw, "male", "female"))
  
  all_words <- bind_rows(target_word_coordinates, anchor_word_coordinates)  %>%
    select(word, type, gender_type, everything())
  
  crit_dists <- map(target_word_coordinates$word, 
                    get_gender_score, 
                    all_words)
  
  crit_dists_df <- keep(crit_dists, ~length(.) > 1) %>%
    bind_rows() %>%
    mutate(model = basename(infile_path))
  
  outfile_full_path <- paste0(outfile_path, basename(infile_path), "_word_gender.csv")
  write_csv(crit_dists_df, outfile_full_path)
}

# do the thing
list.files(FILE_INPATH, full.names = T) %>%
  walk(get_gender_scores_from_model, 
              glasgow_norms, 
              FILE_OUTPATH,
              MALE_WORDS, 
              FEMALE_WORDS)



