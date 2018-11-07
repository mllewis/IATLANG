# get percentage women stem

library(tidyverse)
library(here)
library(janitor)


RAW_PATH <- here("data/study0/raw/EDULIT_DS_06112018192722395.csv")

raw_data <- read_csv(RAW_PATH) %>%
  janitor::clean_names() %>%
  mutate_if(is.character,as.factor) %>%
  select(-time_2, -location, -edulit_ind) %>%
  rename(year = time) %>%
  mutate(indicator_clean = str_split(indicator, "graduating from|\\(%\\)|graduates from programmes|graduates from Science,"))  %>%
  mutate(long_program = unlist(map(indicator_clean, ~.[[2]])),
         full_program = str_split(long_program, "education,|programmes,|fields,"),
         program = tolower(str_trim(unlist(map(full_program, ~.[[1]])))),
         program = str_remove(program, " in tertiary"),
         sex = str_trim(unlist(map(full_program, ~pluck(., length(.)))))) %>%
  #filter(!(program %in% c("technology, engineering and mathematics programmes", "other than science, technology, engineering and mathematics"))) %>%
  select(-indicator_clean, -long_program, -full_program)

# Three levels
#natural sciences/mathematics/statistics, 
#information and communication technologies, 
#engineering, manufacturing, and construction
    
tidy_data <- raw_data %>%
  filter(sex %in% c("female", "male")) %>%
  mutate(STEM_field  = 
           case_when(program %in% sample(unique(raw_data$program),3) ) ~ "STEM",
                     TRUE ~ "non-STEM") %>%
  #mutate(STEM_field  = 
  #         case_when(program %in% c( "business, administration and law", "natural sciences, mathematics and statistics"  ,"engineering, manufacturing and construction", "information and communication technologies") ~ "STEM",
  #                   TRUE ~ "non-STEM")) %>%
  filter(STEM_field == "STEM", sex == "female") %>%
  select(-indicator)

# take the mean across time points
percentage_in_stem_by_country <- tidy_data %>%
  filter(year %in% 2012:2015) %>%
  group_by(year, country) %>%
  summarize(sum_percentage_female_STEM = sum(value, na.rm = T)) %>% # sum across stem field
  group_by(country) %>%
  summarize(mean_percentage_female_STEM = mean(sum_percentage_female_STEM, na.rm = T)) # mean across years
filter(percentage_in_stem_by_country, country == "Algeria")
filter(percentage_in_stem_by_country, country == "Macao")

40.7% in Algeria
