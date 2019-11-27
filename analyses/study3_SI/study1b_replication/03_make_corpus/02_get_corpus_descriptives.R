# get number of articles in each language
library(here)
library(tidyverse)
library(R.utils)
LANGS <- rev(c("ar", "da", "de", "en", "es", "fa", "fi", "fr", "he", "hi", "hr", "id", "it", "ja",
           "ko", "ms", "nl", "no", "pl", "pt", "ro", "sv", "tl", "tr", "zh", "sr"))

INPATH1 <-  "exploratory_studies/16_wiki_native/data/03_native_wiki_corpora/"
INPATH2 <- "_native_wiki_corpus.txt"
OUTPATH <- here("data/study3_SI/study1b_replication/03_corpus/native_wiki_corpus_article_counts.csv")

get_n_articles_in_one_corpus <- function(current_language, inpath1, inpath2){
  print(current_language)
  inpath <- here(paste0(inpath1, current_language, inpath2))
  total_articles <-  as.numeric(countLines(inpath))

  data.frame(language = current_language,
             n_articles = total_articles)

}

article_counts <- map_df(LANGS,
                         get_n_articles_in_one_corpus,
                         INPATH1,
                         INPATH2)

# sum across sr/hr
article_counts_tidy <- article_counts %>%
  mutate(language = case_when(language %in% c("sr", "hr") ~ "sr",
                              TRUE ~ language)) %>%
  group_by(language) %>%
  summarize(n_articles = mean(n_articles))

write_csv(article_counts_tidy, OUTPATH)
