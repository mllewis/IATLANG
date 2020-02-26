# tidy raw native language wiki articles to corpus for embedding training (english and portuguese)
# run perl script afterward to deal with line processing
library(here)
library(tidyverse)


LANGS <-  c("en", "pt")

RAW_PATH1 <- "exploratory_studies/16_wiki_native/data/02_text/"
RAW_PATH2 <- "_nativelang_article_text.csv"
OUTPATH1 <-  "exploratory_studies/16_wiki_native/data/03_native_wiki_corpora/"
OUTPATH2 <- "_native_wiki_corpus.txt"


process_one_lang_corpus <- function(current_language, raw_path1, raw_path2, outpath1, outpath2){
  print(current_language)
  raw_path <- here(paste0(raw_path1, current_language, raw_path2))
  raw_csv <- read_csv(raw_path, col_names = c("article_id", "title", "text"))

  tidy_csv <- raw_csv %>%
    mutate(clength= nchar(text)) %>%
    filter(clength >= 50)  %>%
    data.table::data.table()

  outpath <- here(paste0(outpath1, current_language, outpath2))
  write_lines(tidy_csv$text, outpath)

}

walk(LANGS[2], process_one_lang_corpus,
     RAW_PATH1,
     RAW_PATH2,
     OUTPATH1,
     OUTPATH2)


# PERL SCIRPT:
#perl -pi -w -e 's/HTML//g' en_native_wiki_corpus.txt
#perl -pi -w -e 's/\\\n//g' en_native_wiki_corpus.txt
#perl -pi -w -e 's/=//g' en_native_wiki_corpus.txt
#perl -pi -w -e 's/\\\"//g' en_native_wiki_corpus.txt
#perl -pi -w -e 's/[[:punct:]]/ /g' en_native_wiki_corpus.txt
#perl -pi -w -e 's/^\s+|\s+$//' en_native_wiki_corpus.txt # remove leading and trailing white space
#perl -pi -w -e '$_=lc' en_native_wiki_corpus.txt # lower case

