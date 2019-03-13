# In order to deploy the shiny app to shinyapp.io all of the data has to be stored locally,
# in the SI shiny app folder. This means there two copies of the data in the repo, which means
# that when changes are made to processed data in the main analysis scripts these don't get
# automatically updated in the SI app. This script copies the relevant files and writes them
# to the SI directory. Run this script before deploying the app to ensure all data is up to do date.

library(here)
library(tidyverse)

data_for_SI <- 
  list(
    here("data/study0/processed/top_lang_by_country_ethnologue.csv"),
    here("data/study0/processed/by_country_df.csv"),
    here("data/SI/caliskan_wiki_es.csv"),
    here("data/SI/caliskan_paper_es.csv"),
    here("data/SI/caliskan_sub_es.csv"),
    here("data/SI/corrs_by_exclusions.csv"),
    here("data/study2/general_gender_by_lang.csv"),
    here("data/study2/occupation_translations_tidy.csv")
)

copy_to_si_dir <- function(old_filepath) {
  current_file <- read_csv(old_filepath)
  new_file_path <- paste0("writeup/journal/SI/data/", tail(str_split(old_filepath, "/")[[1]],1))
  write_csv(current_file, here(new_file_path))
}

walk(data_for_SI, copy_to_si_dir)



