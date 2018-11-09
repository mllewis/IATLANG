
library(here)

PATH <- here("analyses/study0/")

PATH %>%
  list.files(full.names = T) %>%
  keep( ~!str_detect(., "wrapper")) %>%
  walk(source)