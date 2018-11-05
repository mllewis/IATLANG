# calculate swabs from embedding model (function on the top right of Caliskan pg 2)
# dealing with gendered langs appropriately (loops in parallel for speed)

# load packages
library(tidyverse)
library(parallel)
library(here)
source(here("analyses/study1b/0_IAT_utils.R"))

MODEL_PATH_WIKI <- here("data/study1b/wiki_subsetted_models/calculated/")
MODEL_PATH_SUB <- here("data/study1b/subt_subsetted_models/calculated/")
OUTFILE <- here("data/study1b/iat_swabs.csv")
NCLUSTERS <- 4
CAREER_WORD_LIST <- list(test_name = "WEAT_6", # not identical to caliskan (caliskan used proper names)
                         bias_type = "gender-bias-career-family",
                         category_1 = c("male", "man", "boy", "brother", "he", "him", "his", "son"),
                         category_2 = c("female", "woman", "girl", "sister", "she", "her", "hers", "daughter"),
                         attribute_1 = c("executive", "management", "professional", "corporation", "salary", 
                                         "office", "business", "career"),
                         attribute_2 = c("home", "parents", "children", "family", "cousins", "marriage", 
                                         "wedding", "relatives"))
 
BAD_LANGS <- c("ta", "cs", "ml", "vi") # these languages we don't have translations for or the models are too small

### prep lists to loop over
# get model-language pairs to loop over
wiki_langs <- list.files(MODEL_PATH_WIKI) %>%
  str_split("\\.|_") %>%
  map_chr(~.[2]) %>%
  data.frame() %>%
  rename(lang = ".") %>%
  mutate(path = paste0(MODEL_PATH_WIKI, "wiki."))

subt_langs <- list.files(MODEL_PATH_SUB) %>%
  str_split("\\.|_") %>%
  map_chr(~.[3]) %>%
  data.frame() %>%
  rename(lang = ".") %>%
  mutate(path = paste0(MODEL_PATH_SUB, "sub.multiword."))

all_langs <- bind_rows(wiki_langs, subt_langs) %>%
  filter(!(lang %in% BAD_LANGS)) %>%
  mutate(id = 1:n()) %>%
  nest(-id)


####

complete <- read_csv(OUTFILE, col_names = c("lang", "model", "category", "x", "y", "z", "w")) %>%
  distinct(lang, model) %>%
  mutate(status = "complete")

all_combos <- bind_rows(wiki_langs %>% mutate(model = "wiki"),
                        subt_langs %>% mutate(model = "sub")) %>%
  select(-path) %>%
  mutate(status = "all")

missing_combos = bind_rows(all_combos, complete) %>%
  count(lang, model) %>%
  filter(n == 1)

all_langs <- bind_rows(wiki_langs %>% mutate(model = "wiki"),
                       subt_langs %>% mutate(model = "sub")) %>%
  right_join(missing_combos) %>%
  select(-model, -n) %>%
  filter(!(lang %in% BAD_LANGS)) %>%
  mutate(id = 1:n()) %>%
  nest(-id) 
  

 ####
 
 
### DO THE THING (IN PARALLEL)
# initial cluster
cluster <- makeCluster(NCLUSTERS, type = "FORK")

# wrapper function
parallel_wrapper <- function(id, this_df, outfile, word_list){
  lang <- pluck(this_df$data[[id]], "lang")
  path <- pluck(this_df$data[[id]], "path")
  es_out <- get_lang_es_separate_gender(lang,
                                        path,
                                        word_list)
  write_csv(es_out, outfile, append = T)
}

parLapply(cluster,
          1:nrow(all_langs), 
          parallel_wrapper, 
          all_langs, 
          OUTFILE, 
          CAREER_WORD_LIST)
