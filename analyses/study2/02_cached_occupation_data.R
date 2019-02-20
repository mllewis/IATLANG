# cache occupation translations to csv from online doc

library(googlesheets)
library(tidyverse)
library(here)

print("cache occupation translations from google docs")

OUTFILE <- here('data/study2/occupation_translations_raw.csv')

googlesheet_df <- gs_title("occupations for translation")
translations <- map_df(3:29, function(x){Sys.sleep(10);gs_read(googlesheet_df, x)})
# sleep time necessary, or else get too many requests error

write_csv(translations, OUTFILE)
