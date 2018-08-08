# clean up miseresky tables from pdf table

library(tidyverse)

# translations
TPATH <- "miseresky_translations_tidy.csv"
TPATHOUT <- "miseresky_translations_clean.csv"

trans <- read_csv(TPATH)

clean_translations <- trans %>%
  mutate(English = str_replace_all(English, "[0-9]", "")) %>%
  mutate_all(trimws) %>%
  mutate_all(tolower) %>%
  mutate_all(funs(ifelse(. ==  "n/a", NA, .)))

write_csv(clean_translations, TPATHOUT)

# norms
NPATH <- "mirsesky_norms_tidy.csv"
NPATHOUT <- "mirsesky_norms_clean.csv"

norms <- read_csv(NPATH)

clean_norms <-  norms %>%
  rename(occupation = "X1") %>%
  rowwise() %>%
  mutate_at(2:8, funs(str_split(., " ")[[1]][1])) %>%
  mutate_all(tolower) %>%
  mutate_all(funs(ifelse(. ==  "n/a", NA, .))) %>%
  mutate_at(2:8, as.numeric) 

# Czech
czech_missing <- clean_norms %>%
  select(occupation, Czech) %>%
  filter(is.na(Czech)) %>%
  mutate(Czech = replace(Czech, occupation =="accountants", 
                         unlist(clean_norms[clean_norms$occupation == "bookkeepers","Czech"])),
         Czech = replace(Czech, occupation =="administrative workers", 
                         unlist(clean_norms[clean_norms$occupation == "office workers","Czech"])),
         Czech = replace(Czech, occupation =="babysitters", 
                         unlist(clean_norms[clean_norms$occupation == "child-minders","Czech"])),
         Czech = replace(Czech, occupation =="bank clerks", 
                         unlist(clean_norms[clean_norms$occupation == "bankers","Czech"])),
         Czech = replace(Czech, occupation =="bikers", 
                         unlist(clean_norms[clean_norms$occupation == "cyclists","Czech"])),
         Czech = replace(Czech, occupation =="cake decorators", 
                         unlist(clean_norms[clean_norms$occupation == "pastry chefs","Czech"])),
         Czech = replace(Czech, occupation =="customers", 
                         unlist(clean_norms[clean_norms$occupation == "shoppers","Czech"])),
         Czech = replace(Czech, occupation =="caregivers", 
                         unlist(clean_norms[clean_norms$occupation == "health visitors","Czech"])),
         Czech = replace(Czech, occupation =="climbers", 
                         unlist(clean_norms[clean_norms$occupation == "mountain climbers","Czech"])),
         Czech = replace(Czech, occupation =="doctors of philosophy", 
                         unlist(clean_norms[clean_norms$occupation == "physicians","Czech"])),
         Czech = replace(Czech, occupation =="dressmakers", 
                         unlist(clean_norms[clean_norms$occupation == "tailors","Czech"])),
         Czech = replace(Czech, occupation =="fans", 
                         unlist(clean_norms[clean_norms$occupation == "groupies","Czech"])),
         Czech = replace(Czech, occupation =="gardeners", 
                         unlist(clean_norms[clean_norms$occupation == "groundkeepers","Czech"])),
         Czech = replace(Czech, occupation =="infant teachers", 
                         unlist(clean_norms[clean_norms$occupation == "nursery teachers","Czech"])),
         Czech = replace(Czech, occupation =="joggers", 
                         unlist(clean_norms[clean_norms$occupation == "runners","Czech"])),
         Czech = replace(Czech, occupation =="killers", 
                         unlist(clean_norms[clean_norms$occupation == "murderers","Czech"])),
         Czech = replace(Czech, occupation =="kindergarten teachers", 
                         unlist(clean_norms[clean_norms$occupation == "nursery teachers","Czech"])),
         Czech = replace(Czech, occupation =="leaders", 
                         unlist(clean_norms[clean_norms$occupation == "supervisors","Czech"])),
         Czech = replace(Czech, occupation =="sales assistants", 
                         unlist(clean_norms[clean_norms$occupation == "salespersons","Czech"])),
         Czech = replace(Czech, occupation =="shoplifters", 
                         unlist(clean_norms[clean_norms$occupation == "thieves","Czech"])))
czech_all <- clean_norms %>%
  select(occupation, Czech) %>%
  filter(!is.na(Czech)) %>%
  bind_rows(czech_missing) %>%
  mutate(language = "czech") %>%
  rename(mean_gender_rating = Czech)

# French missing
french_missing <- clean_norms %>%
  select(occupation, French) %>%
  filter(is.na(French)) %>%
  mutate(French = replace(French, occupation =="bookkeepers", 
                         unlist(clean_norms[clean_norms$occupation == "accountants","French"])),
         French = replace(French, occupation =="car mechanics", 
                         unlist(clean_norms[clean_norms$occupation == "mechanics","French"])),
         French = replace(French, occupation =="caretakers", 
                          unlist(clean_norms[clean_norms$occupation == "janitors","French"])),
         French = replace(French, occupation =="children", 
                          unlist(clean_norms[clean_norms$occupation == "kids","French"])),
         French = replace(French, occupation =="clients", 
                          unlist(clean_norms[clean_norms$occupation == "customers","French"])),
         French = replace(French, occupation =="company directors", 
                          unlist(clean_norms[clean_norms$occupation == "general managers","French"])),
         French = replace(French, occupation =="conservationists", 
                          unlist(clean_norms[clean_norms$occupation == "curators","French"])),
         French = replace(French, occupation =="doctors of philosophy", 
                          unlist(clean_norms[clean_norms$occupation == "medical doctors","French"])),
         French = replace(French, occupation =="editors", 
                          unlist(clean_norms[clean_norms$occupation == "publishers","French"])),
         French = replace(French, occupation =="journalists", 
                          unlist(clean_norms[clean_norms$occupation == "reporters","French"])))
         
french_all <- clean_norms %>%
  select(occupation, French) %>%
  filter(!is.na(French)) %>%
  bind_rows(french_missing)%>%
  mutate(language = "french") %>%
  rename(mean_gender_rating = French)

# German missing

german_missing <- clean_norms %>%
  select(occupation, German) %>%
  filter(is.na(German)) %>%
  mutate(German = replace(German, occupation =="accountants", 
                           unlist(clean_norms[clean_norms$occupation == "bookkeepers","German"])),
         German = replace(German, occupation =="children", 
                           unlist(clean_norms[clean_norms$occupation == "kids","German"])),
         German = replace(German, occupation =="child educators", 
                           unlist(clean_norms[clean_norms$occupation == "nursery teachers","German"])))
german_all <- clean_norms %>%
  select(occupation, German) %>%
  filter(!is.na(German)) %>%
  bind_rows(german_missing)%>%
  mutate(language = "german") %>%
  rename(mean_gender_rating = German)

# Italian missing
italian_missing <- clean_norms %>%
  select(occupation, Italian) %>%
  filter(is.na(Italian)) %>%
  mutate(Italian = replace(Italian, occupation =="ballet dancers", 
                          unlist(clean_norms[clean_norms$occupation == "dancers","Italian"])),
         Italian = replace(Italian, occupation =="car mechanic", 
                          unlist(clean_norms[clean_norms$occupation == "mechanics","Italian"])),
         Italian = replace(Italian, occupation =="carpenters", 
                           unlist(clean_norms[clean_norms$occupation == "wood workers","Italian"])),
         Italian = replace(Italian, occupation =="dress makers", 
                           unlist(clean_norms[clean_norms$occupation == "tailors","Italian"])),
         Italian = replace(Italian, occupation =="fashion models", 
                           unlist(clean_norms[clean_norms$occupation == "models","Italian"])),
         Italian = replace(Italian, occupation =="health visitors", 
                           unlist(clean_norms[clean_norms$occupation == "social workers","Italian"])),
         Italian = replace(Italian, occupation =="infant teachers", 
                           unlist(clean_norms[clean_norms$occupation == "nursery teachers","Italian"])),
         Italian = replace(Italian, occupation =="technicians", 
                           unlist(clean_norms[clean_norms$occupation == "technologists","Italian"])))

italian_all <- clean_norms %>%
  select(occupation, Italian) %>%
  filter(!is.na(Italian)) %>%
  bind_rows(italian_missing) %>%
  mutate(language = "italian") %>%
  rename(mean_gender_rating = Italian)

# Norwegian missing
norwegian_missing <- clean_norms %>%
  select(occupation, Norwegian) %>%
  filter(is.na(Norwegian)) %>%
  mutate(Norwegian = replace(Norwegian, occupation =="medical doctors", 
                           unlist(clean_norms[clean_norms$occupation == "physicians","Norwegian"])),
         Norwegian = replace(Norwegian, occupation =="members of the armed forces", 
                             unlist(clean_norms[clean_norms$occupation == "soldiers","Norwegian"])),
         Norwegian = replace(Norwegian, occupation =="funeral directors", 
                             unlist(clean_norms[clean_norms$occupation == "undertakers","Norwegian"])),
         Norwegian = replace(Norwegian, occupation =="fortune tellers", 
                             unlist(clean_norms[clean_norms$occupation == "tarot card readers","Norwegian"])),
         Norwegian = replace(Norwegian, occupation =="dressmakers", 
                             unlist(clean_norms[clean_norms$occupation == "tailors","Norwegian"])),
         Norwegian = replace(Norwegian, occupation =="company directors", 
                             unlist(clean_norms[clean_norms$occupation == "executives","Norwegian"])),
         Norwegian = replace(Norwegian, occupation =="caretakers", 
                             unlist(clean_norms[clean_norms$occupation == "supervisors","Norwegian"])),
         Norwegian = replace(Norwegian, occupation =="babysitters", 
                             unlist(clean_norms[clean_norms$occupation == "child-minders","Norwegian"])),
         Norwegian = replace(Norwegian, occupation =="administrative workers", 
                             unlist(clean_norms[clean_norms$occupation == "secretaries","Norwegian"])))

norwegian_all <- clean_norms %>%
  select(occupation, Norwegian) %>%
  filter(!is.na(Norwegian)) %>%
  bind_rows(norwegian_missing) %>%
  mutate(language = "norwegian")  %>%
  rename(mean_gender_rating = Norwegian)

# Slovak missing
slovak_missing <- clean_norms %>%
  select(occupation, Slovak) %>%
  filter(is.na(Slovak)) %>%
  mutate(Slovak = replace(Slovak, occupation =="accountants", 
                             unlist(clean_norms[clean_norms$occupation == "bookkeepers","Slovak"])),
         Slovak = replace(Slovak, occupation =="adminstrative workers", 
                             unlist(clean_norms[clean_norms$occupation == "office workers","Slovak"])),
         Slovak = replace(Slovak, occupation =="bikers", 
                             unlist(clean_norms[clean_norms$occupation == "cyclists","Slovak"])),
         Slovak = replace(Slovak, occupation =="cake decorators", 
                          unlist(clean_norms[clean_norms$occupation == "pastry chefs","Slovak"])),
         Slovak = replace(Slovak, occupation =="climbers", 
                          unlist(clean_norms[clean_norms$occupation == "mountain climbers","Slovak"])),
         Slovak = replace(Slovak, occupation =="customers", 
                          unlist(clean_norms[clean_norms$occupation == "shoppers","Slovak"])),
         Slovak = replace(Slovak, occupation =="dressmakers", 
                          unlist(clean_norms[clean_norms$occupation == "tailors","Slovak"])),
         Slovak = replace(Slovak, occupation =="doctors of philosophy", 
                          unlist(clean_norms[clean_norms$occupation == "physicians","Slovak"])),
         Slovak = replace(Slovak, occupation =="fans", 
                          unlist(clean_norms[clean_norms$occupation == "groupies","Slovak"])),
         Slovak = replace(Slovak, occupation =="gardeners", 
                          unlist(clean_norms[clean_norms$occupation == "groundkeepers","Slovak"])),
         Slovak = replace(Slovak, occupation =="joggers", 
                          unlist(clean_norms[clean_norms$occupation == "runners","Slovak"])),
         Slovak = replace(Slovak, occupation =="killers", 
                          unlist(clean_norms[clean_norms$occupation == "murderers","Slovak"])),
         Slovak = replace(Slovak, occupation =="kindergarten teachers", 
                          unlist(clean_norms[clean_norms$occupation == "nursery teachers","Slovak"])),
         Slovak = replace(Slovak, occupation =="leaders", 
                          unlist(clean_norms[clean_norms$occupation == "supervisors","Slovak"])),
         Slovak = replace(Slovak, occupation =="shoplifters", 
                          unlist(clean_norms[clean_norms$occupation == "thieves","Slovak"])))

slovak_all <- clean_norms %>%
  select(occupation, Slovak) %>%
  filter(!is.na(Slovak)) %>%
  bind_rows(slovak_missing) %>%
  mutate(language = "slovak") %>%
  rename(mean_gender_rating = Slovak)


all_norms_clean <- clean_norms %>%
  select(occupation, English) %>%
  mutate(language = "english") %>% 
  rename(mean_gender_rating = English) %>%
  bind_rows(list(french_all, german_all, czech_all, slovak_all, norwegian_all, italian_all))

write_csv(all_norms_clean, NPATHOUT)

             
  

