# Get continous gender measures based on translations

library(tidyverse)
library(here)
INFILE <- here("data/study1b/iat_translations_tidy_wiki.csv")
OUTFILE <- here("data/study2/empirical_gender_by_lang.csv")

translations <- read_csv(INFILE) %>%
  filter(!(word %in% c("super-smart", "genius", "super-imaginative", "brilliant", "artistic", "creative")))

gender_langs <- translations %>%
   count(language, gender) %>% 
   spread("gender", "n") %>% 
   mutate(gendered = ifelse(is.na(F) & is.na(M), "not_gendered", "gendered")) %>%
   replace_na(list(F = 0,M = 0)) %>%
   filter(gendered == "gendered") %>%
   pull(language)

 
tidy_gendered_translations <- translations %>%
   filter(language %in% gender_langs,
          gender != "N") %>%
   mutate(translation = trimws(translation), # trim leading/trailing white space
          translation = tolower(translation),
          translation = str_replace_all(translation, "\b+", ""),
          translation = str_replace_all(translation, "  *", " "), # get rid of multiple spaces
          translation = str_replace_all(translation, " -", "-"), # get rid of spaces around -
          translation = str_replace_all(translation, " - ", "-"),
          translation = str_replace_all(translation, "- ", "-"),
          translation = str_replace_all(translation, "-", " "), # replace dash with space to indicate diff word
          translation = str_replace_all(translation, "/ ", "/"),
          translation = str_replace_all(translation, " /", "/")) %>%
   spread("gender", "translation") %>%
   arrange(language)

gendered_df <- tidy_gendered_translations %>%
  group_by(language, word, translation_id) %>%
  summarize(F_paste = paste(`F`, collapse =" "),
            M_paste = paste(M, collapse =" ")) %>%
  filter(F_paste != "NA",  M_paste != "NA") %>%
  mutate(diff_trans = M_paste != F_paste) %>%
  group_by(language, word) %>%
  summarize(diff_trans = sum(diff_trans)/n()) %>%
  group_by(language) %>%
  summarize(empirical_grammatical_gender_score = sum(diff_trans)/n())

non_gendered_df <- translations %>%
  filter(!(language %in% gender_langs)) %>%
  distinct(language) %>%
  mutate(empirical_grammatical_gender_score = 0)

full_df <- bind_rows(gendered_df, non_gendered_df)

write_csv(full_df, OUTFILE)

