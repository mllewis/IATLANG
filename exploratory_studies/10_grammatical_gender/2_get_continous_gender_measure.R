# Get continous gender measures based on translations

library(tidyverse)
OUTFILE <- "continuous_gender.csv"

translations <- read_csv("/Users/mollylewis/Documents/research/Projects/1_in_progress/IATLANG/exploratory_analyses/8_gender_genius/2_language_wikipedia/data/tidy_iat_translations.csv") %>%
  filter(!(word %in% c("super-smart", "genius", "super-imaginative", "brilliant", "artistic", "creative")))

translations %>%
  filter(gender != "N")

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
  mutate(diff_trans = `M` == `F`) %>%
  group_by(language) %>%
  summarize(num_dif_trans = 16 -sum(diff_trans)) 

non_gendered_df <- translations %>%
  filter(!(language %in% gender_langs)) %>%
  distinct(language) %>%
  mutate(num_dif_trans = 0)

full_df <- bind_rows(gendered_df, non_gendered_df)

write_csv(full_df, OUTFILE)

