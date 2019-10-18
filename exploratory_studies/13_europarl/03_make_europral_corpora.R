# make europarl corpora
library(tidyverse)
library(data.table)

TIDY_CSVS <- "/Volumes/wilbur_the_great/europarl/tidy_csvs/all_target_turns.csv"
OUTFILE <- "/Volumes/wilbur_the_great/europarl/corpora/"
all_target_data <- fread(TIDY_CSVS)

all_target_data_tidy <- all_target_data %>%
  mutate_at(vars(session, speaker_language, speaker_name, 
                 speaker_country, turn_language, chapter), as.factor)

# untranslated corpora
write_untranslated_corpus <- function(current_language, turn_df, outfile){
  print(current_language)
  es_data <- turn_df %>%
    filter(speaker_language == current_language,
           turn_language == current_language)  %>%
    mutate(turn_text = tolower(turn_text),
           turn_text = str_remove_all(turn_text, "[’—().,?/!;:\"\'\\/?/-]")) 
  concatenated_data <- paste(es_data$turn_text, sep = "\n")
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
    filter(speaker_language != current_language,
           turn_language == current_language)  %>%
    mutate(turn_text = tolower(turn_text),
           turn_text = str_remove_all(turn_text, "[’—().,?/!;:\"\'\\/?/-]")) 
  concatenated_data <- paste(es_data$turn_text, sep = "\n")
  
  concatenated_data <- paste(es_data$turn_text, sep = "\n")
  outfile_path <- paste0(outfile, "trans_f_europarl_", current_language, ".txt")
  
  writeLines(concatenated_data, outfile_path)
}

walk(unique(all_target_data_tidy$speaker_language),
     write_translated_corpus,
     all_target_data_tidy,
     OUTFILE)

sum(c(3, 35, 46, 17, 5, 32, 15, 17, 4, 13, 2, 8))

