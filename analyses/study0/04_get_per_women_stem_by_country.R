# get percentage women stem from raw unesco data (same measure as Stoet and Geary, 2018)

library(tidyverse)
library(here)
library(janitor)

print("Get prop women STEM by country")


RAW_PATH <- here("data/study0/raw/EDULIT_DS_08112018110754920.csv") 
# from: http://data.uis.unesco.org labeled “Distribution of tertiary graduates by field of study” (same as Stoet & Geary 2018 paper)

OUTFILE <-  here("data/study0/processed/per_women_stem_by_country.csv") 

# tidy dataframe
raw_data <- read_csv(RAW_PATH) %>%
  clean_names() %>%
  mutate_if(is.character,as.factor) %>%
  rename(year = time) %>%
  mutate(indicator_clean = str_split(indicator, "graduating from|\\(%\\)|graduates from programmes|graduates from Science,"))  %>%
  mutate(long_program = unlist(map(indicator_clean, ~.[[2]])),
         full_program = str_split(long_program, "education,|programmes,|fields,"),
         program = tolower(str_trim(unlist(map(full_program, ~.[[1]])))),
         program = str_remove(program, " in tertiary"),
         sex = str_trim(unlist(map(full_program, ~pluck(., length(.))))),
         value = case_when(str_detect(flag_codes, "n") ~ 0, TRUE ~ value)) %>%# replace "nil" flag with 0
  filter(!(program %in% c("technology, engineering and mathematics programmes", "other than science, technology, engineering and mathematics"))) %>%
  select(-indicator_clean, -long_program, -full_program, -time_2, -location, -edulit_ind)

# 3 levels from Stoet & Geary (2018) paper: 
# natural sciences/mathematics/statistics, 
# information and communication technologies, 
# engineering, manufacturing, and construction
   
# get female, STEM only
tidy_data <- raw_data %>%
  filter(sex == "female") %>%
  mutate(STEM_field  = 
           case_when(program %in% c( "information and communication technologies" , "natural sciences, mathematics and statistics"  ,"engineering, manufacturing and construction", "information and communication technologies") ~ "STEM",
                     TRUE ~ "non-STEM")) %>%
  filter(STEM_field == "STEM") %>%
  select(-indicator)

# take the mean across time points
percentage_in_stem_by_country <- tidy_data %>%
  group_by(year, country) %>%
  summarize(sum_percentage_female_STEM = sum(value, na.rm = T)) %>% # sum across stem field
  group_by(country) %>%
  summarize(per_women_stem_2012_2017 = mean(sum_percentage_female_STEM, na.rm = T)) %>%  # mean across years
  mutate(country_code = countrycode::countrycode(country, 'country.name', 'iso2c'))   %>%
  select(country_code, per_women_stem_2012_2017)

write_csv(percentage_in_stem_by_country, OUTFILE)
  

