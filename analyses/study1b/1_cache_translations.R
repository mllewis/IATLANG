# cache translations to csv from online doc

library(googlesheets)
library(tidyverse)
library(here)

print("cache translations from google docs")

OUTFILE <- here('data/study1b/iat_translations_raw.csv')

googlesheet_df <- gs_title("IATLANG STUDY 1 TRANSLATIONS - V2 - clean")
translations <- gs_read(googlesheet_df)

write_csv(translations, OUTFILE)

