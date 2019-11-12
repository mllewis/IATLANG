# get by-language weights based on model sizes and n participants in behavraiol data

library(tidyverse)
library(here)

print("get weights")


LANGS_NUM_PARICIPANTS <- here("data/study0/processed/by_language_df.csv") 
LANGS <- here("data/study0/processed/top_lang_by_country_ethnologue.csv") 
MODEL_PREFIX_SUB <- "/Volumes/wilbur_the_great/subtitle_models/sub."
MODEL_PREFIX_WIKI <- "/Volumes/wilbur_the_great/fasttext_models/wiki."
MODEL_PREFIX_WIKI_CC <- "/Volumes/wilbur_the_great/wiki_cc_models/wiki_cc."

FILEOUT <- here("data/study1b/language_weights.csv") 

langs <- read_csv(LANGS) %>%
  distinct(wiki_language_code) %>%
  pull(wiki_language_code)

num_participants <- read_csv(LANGS_NUM_PARICIPANTS) %>%
  select(wiki_language_code, n_participants )

target_langs <- read_csv(LANGS) %>%
  distinct(wiki_language_code) %>%
  pull(wiki_language_code)

wiki_models <- map_chr(langs, ~paste0(MODEL_PREFIX_SUB, ., ".vec"))
sub_models <- map_chr(langs, ~paste0(MODEL_PREFIX_WIKI, ., ".vec"))
wiki_cc_models <- map_chr(langs, ~paste0(MODEL_PREFIX_WIKI_CC, ., ".vec"))


all_models <- c(wiki_models, sub_models, wiki_cc_models)

get_size <- function(this_file){
  
  if (file.exists(this_file)) {
    this_size = file.size(this_file)
  } else  {
    this_size = NA
  }

  this_lang = str_split(this_file, "\\.") %>%
    map_chr(~.[2]) 
  
  this_source = str_split(this_file, "models/") %>%
    map_chr(~.[2])  %>%
    str_split("\\.")  %>%
    map_chr(~.[1]) 
  
  data.frame(wiki_language_code = this_lang,
             file_size = this_size,
             source = this_source)
}

model_sizes <- map_df(all_models, get_size) %>%
  filter(wiki_language_code %in% target_langs,
         !is.na(file_size))


all_sizes <- full_join(model_sizes, num_participants) %>%
  group_by(source) %>%
  mutate(file_size_scaled = scale(file_size, center = F),
        n_participants_scaled = scale(n_participants, center = F)) %>%
  rowwise()%>%
  mutate(mean_weight = mean(c(file_size_scaled, n_participants_scaled)))
        
write_csv(all_sizes, FILEOUT)


