
library(tidyverse)
PATH <- "/Users/mollylewis/Downloads/Ndebele.Pelling.1971.txt"


df <- read_tsv(PATH) %>%
  janitor::clean_names()


df_tidy <- df %>%
  filter(pos == "n.") %>%
  mutate(full_lexeme = paste0(prefix,stem)) %>%
  select(full_lexeme, gloss) 
  

df_tidy %>%
  filter(str_detect(gloss, "creative"))
