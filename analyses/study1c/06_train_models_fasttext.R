# train separate fasttext model for coca and bnc corpora 
library(tidyverse)
library(data.table)
library(fastrtext)
library(here)

### repeat 5X per model
CORPUS_PATH <-   here("data/study1c/raw/BNCspokenFormatted.txt") #  
OUTFILE <- here("data/study1c/processed/trained_bnc_fasttext_400_10.csv")  #_bnc_model_5_400_FALSE_10_
#CORPUS_PATH <- here("data/study1c/raw/COCAshort_words.txt")
#OUTFILE <- here("data/study1c/processed/trained_coca_fasttext_400_10.csv") #_coca_model_5_400_FALSE_10_

MIN_WORD_COUNT <- 5
VECTOR_SIZE <- 400
WINDOW_SIZE <- 10

# get corpus
corpus <- read_lines(CORPUS_PATH)   %>%
  str_split(" ")  %>%
  unlist()

tmp_file_txt <- tempfile()
tmp_file_model <- tempfile()
writeLines(text = corpus, con = tmp_file_txt)
execute(commands = c("skipgram",
                     #set the input and output files
                     "-input", tmp_file_txt,
                     "-output", tmp_file_model,
                     #set the window size if needed, default is 5
                     "-ws", WINDOW_SIZE,
                     #min length of char ngram, default is 3
                     "-minn", 1,
                     #max length of char ngram, default is 6, minn and maxn both as 1 if don’t want to use ngrams
                     "-maxn", 1,
                     #minimal number of word occurrences, default is 5, can set to 100 instead
                     "-minCount", MIN_WORD_COUNT,
                     #max length of word ngram, default is 1
                     #”-wordNgrams",1,
                     #number of epochs, default is 5
                     #"-epoch",5,
                     #set the number of dimensions, default is 100
                     "-dim", VECTOR_SIZE,
                     "-verbose", 1))

model <- load_model(tmp_file_model)
vectors <- get_word_vectors(model) %>%
  as.data.frame() %>%
  rownames_to_column("target_word")

write_csv(vectors, OUTFILE)