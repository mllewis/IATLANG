# cache translations to csv from online doc

library(googlesheets)
library(tidyverse)

OUTFILE <- '../../data/study2b/iat_translations_raw.csv'

googlesheet_df <- gs_title("IATLANG STUDY 1 TRANSLATIONS - V2 - clean")
translations <- gs_read(googlesheet_df)

write_csv(translations, OUTFILE)