
library(tidyverse)
PATH <- "/Users/mollylewis/Downloads/Ndebele.Pelling.1971.txt"


df <- read_tsv(PATH) %>%
  janitor::clean_names()


df_tidy <- df %>%
  filter(pos == "n.") %>%
  select(prefix, stem, lexeme, gloss)


df_tidy %>%
  filter(str_detect(gloss, "man"))

df_tidy %>%
  filter(str_detect(gloss, "cousin"))