# cache translations to csv from online doc

library(googlesheets)
library(tidyverse)

OUTFILE <- 'iat_translations.csv'

googlesheet_df <- gs_title("IATLANG STUDY 1 TRANSLATIONS - V2 - clean")
translations <- gs_read(googlesheet_df)

write_csv(translations, OUTFILE)