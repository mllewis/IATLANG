# make europarl corpora
library(tidyverse)
library(data.table)

TIDY_CSVS <- "/Volumes/wilbur_the_great/europarl/sessions/all_target_turns.csv"
OUTFILE <- "/Volumes/wilbur_the_great/europarl/sessions/corpora/"
all_target_data <- fread(TIDY_CSVS)

all_target_data_tidy <- all_target_data %>%
  mutate_at(vars(session, speaker_language, speaker_name, 
                 speaker_country, turn_language), as.factor)

# untranslated corpora
write_untranslated_corpus <- function(current_language, turn_df, outfile){
  print(current_language)
  es_data <- turn_df %>%
    filter(speaker_language == current_language,
           turn_language == current_language) 
  
  concatenated_data <- paste(es_data$turn_text, collapse = ' ')
  outfile_path <- paste0(outfile, "untrans_europarl_", current_language, ".txt")
  
  writeLines(concatenated_data, outfile_path)
}


walk(unique(all_target_data_tidy$speaker_language),
     write_untranslated_corpus,
     all_target_data_tidy,
     OUTFILE)

# translated corpora
write_translated_corpus <- function(current_language, turn_df, outfile){
  print(current_language)
  es_data <- turn_df %>%
    filter(#speaker_language != current_language,
           turn_language == current_language) 
  
  concatenated_data <- paste(es_data$turn_text, collapse = ' ')
  outfile_path <- paste0(outfile, "trans_f_europarl_", current_language, ".txt")
  
  writeLines(concatenated_data, outfile_path)
}

walk(unique(all_target_data_tidy$speaker_language),
     write_translated_corpus,
     all_target_data_tidy,
     OUTFILE)


