
library(here)

PATH <- here("analyses/study1b/")

PATH %>%
  list.files(full.names = T) %>%
  keep( ~!str_detect(., "wrapper|utils|.sh")) %>%
  walk(source)