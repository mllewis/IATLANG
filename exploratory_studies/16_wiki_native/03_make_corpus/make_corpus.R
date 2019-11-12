# tidy raw native language wiki articles to corpus for embedding training
library(here)
library(tidyverse)

LANGS <- rev(c("ar", "da", "de", "en", "es", "fa", "fi", "fr", "he", "hi", "hr", "id", "it", "ja",
           "ko", "ms", "nl", "no", "pl", "pt", "ro", "sv", "tl", "tr", "zh"))

LANGS <-  "sr"

RAW_PATH1 <- "exploratory_studies/16_wiki_native/data/02_text/"
RAW_PATH2 <- "_nativelang_article_text.csv"
OUTPATH1 <-  "exploratory_studies/16_wiki_native/data/03_native_wiki_corpora/"
OUTPATH2 <- "_native_wiki_corpus.txt"

raw_path1 = RAW_PATH1
raw_path2 = RAW_PATH2
current_language = "en"
process_one_lang_corpus <- function(current_language, raw_path1, raw_path2, outpath1, outpath2){
  print(current_language)
  raw_path <- here(paste0(raw_path1, current_language, raw_path2))
  raw_csv <- read_csv(raw_path, col_names = c("article_id", "title", "text"))

  tidy_csv <- raw_csv %>%
    mutate(clength= nchar(text)) %>%
    filter(clength >= 50)  %>%
    data.table::data.table()

  tidy_csv$text2 <- gsub('HTML|\n|=|\"', '',tidy_csv$text)


  tidy_csv2 <- tidy_csv %>%
    mutate(text = str_replace_all(text, "HTML", ""),
           text = str_replace_all(text, "\n", ""),
           text = str_replace_all(text, "=", ""),
           text = str_replace_all(text, "\"", ""))
          # text = tolower(text), @# manual lowercase dd if=/Users/mollylewis/Documents/research/Projects/1_in_progress/IATLANG/exploratory_studies/16_wiki_native/data/03_native_wiki_corpora/pt_native_wiki_corpus.txt of=/Users/mollylewis/Documents/research/Projects/1_in_progress/IATLANG/exploratory_studies/16_wiki_native/data/03_native_wiki_corpora/pt_native_wiki_corpusl.txt conv=lcase
          # text = gsub('[[:punct:] ]+',' ',text),
           #text = str_trim(text, side = "both"))

  tidy_csv$text2 <- gsub('[[:punct:] ]+',' ',tidy_csv2$text)

  outpath <- here(paste0(outpath1, current_language, outpath2))
  write_lines(tidy_csv$text2, outpath)

}

walk(LANGS, process_one_lang_corpus,
     RAW_PATH1,
     RAW_PATH2,
     OUTPATH1,
     OUTPATH2)
