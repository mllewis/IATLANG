# bind together tidy csvs
library(tidyverse)
library(data.table)

target_langs <- c("ar", "fa", "es", "de", "en", "nl", "pt", "zh", "da", "fi", "fr", "hr", "id",
                  "he", "hi", "it", "ja", "ko", "ms", "no", "tl", "pl", "ro", "sv", "tr")

OVERLAPPING_LANGS <- c("en", "it", "nl", "fr", "pt", "es", "da", "sv", "de", "fi", "pl", "ro")
INFILE <- "/Volumes/wilbur_the_great/europarl/tidy_csvs/"
OUTFILE <- "/Volumes/wilbur_the_great/europarl/tidy_csvs/all_target_turns.csv"

files_df <- list.files(INFILE, full.names = T) %>%
  map(fread) %>%
  bind_rows()

all_files_tidy <- files_df %>%
  mutate_at(vars(session, speaker_language, speaker_name,
                 speaker_country, turn_language), as.factor) %>%
  filter(!is.na(speaker_language)) %>%
  filter(speaker_language %in% OVERLAPPING_LANGS,
         turn_language %in% OVERLAPPING_LANGS)

write_csv(all_files_tidy, OUTFILE)

