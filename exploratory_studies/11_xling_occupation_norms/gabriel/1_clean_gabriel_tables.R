# clean up gabriel tables

library(tidyverse)

# translations
TPATH <- "raw_data/GabrielAPP-A(2008).txt"
TPATHOUT <- "gabriel_translations_clean.csv"

trans <- read.table(TPATH, 
                    sep  = "\t", 
                    fileEncoding = "MAC",
                    fill = T) %>%
  select(V2, V4, V6) %>%
  slice(-1:-4) %>%
  rename(en = V2,
         fr = V4,
         de = V6) %>%
  mutate_all(tolower) %>%
  mutate(fr = replace(fr, fr =="proches (famille)","proches"))

trans_long <- trans %>%
  gather("wiki_lang_code", "translation", -1) %>%
  rename(target_word = "en") %>%
  bind_rows(data.table(target_word = trans$en,
                       wiki_lang_code = "en", 
                       translation = trans$en))

write_csv(trans_long, TPATHOUT)
         

# norms S1
NPATH <- "raw_data/GabrielAPP-B(2008).txt"
NPATHOUT <- "gabriel_norms_clean_S1.csv"

clean_norms <- read_delim(NPATH, delim = "\t") %>%
  select(1,2,4,6) %>%
  slice(-1:-4) %>%
  rename(occupation = `Appendix B`,
         en = X2, 
         fr = X4, 
         de = X6) %>%
  mutate(occupation = tolower(occupation)) %>%
  gather("wiki_lang_code", "human_mean_rating", -1)

write_csv(clean_norms, NPATHOUT)

# norms S1g
NPATH <- "raw_data/GabrielAPP-C(2008).txt"
NPATHOUT <- "gabriel_norms_clean_S1g.csv"

clean_norms <- read_delim(NPATH, delim = "\t") %>%
  select(1,2,4,6, 8, 10, 12) %>%
  slice(-1:-6) %>%
  rename(occupation = `Appendix C`,
         en_m = X2, 
         en_f = X4, 
         fr_m = X6,
         fr_f = X8,
         de_m = X10,
         de_f = X12) %>%
  mutate(occupation = tolower(occupation)) %>%
  gather("wiki_lang_code", "human_mean_rating", -1) %>%
  separate(wiki_lang_code, "_", into = c("wiki_lang_code" ,"gender"))

write_csv(clean_norms, NPATHOUT)

# norms S2
NPATH <- "raw_data/GabrielAPP-D(2008).txt"
NPATHOUT <- "gabriel_norms_clean_S2.csv"


clean_norms <- read_delim(NPATH, delim = "\t") %>%
  select(1,2,4,6) %>%
  slice(-1:-4) %>%
  rename(occupation = `Appendix D`,
         en = X2, 
         fr = X4, 
         de = X6) %>%
  mutate(occupation = tolower(occupation)) %>%
  gather("wiki_lang_code", "human_mean_rating", -1)

write_csv(clean_norms, NPATHOUT)

# norms S2g
NPATH <- "raw_data/GabrielAPP-E(2008).txt"
NPATHOUT <- "gabriel_norms_clean_S2g.csv"


clean_norms <- read_delim(NPATH, delim = "\t") %>%
  select(1,2,4,6, 8, 10, 12) %>%
  slice(-1:-6) %>%
  rename(occupation = `Appendix E`,
         en_m = X2, 
         en_f = X4, 
         fr_m = X6,
         fr_f = X8,
         de_m = X10,
         de_f = X12) %>%
  mutate(occupation = tolower(occupation)) %>%
  gather("wiki_lang_code", "human_mean_rating", -1) %>%
  separate(wiki_lang_code, "_", into = c("wiki_lang_code" ,"gender"))

write_csv(clean_norms, NPATHOUT)
