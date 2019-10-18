# convert list of compressed *wiki-latest-langlinks.sql.gz (language links) files to csvs
library(tidyverse)
library(reticulate)
library(here)

use_python("/Users/mollylewis/anaconda2/envs/py2/bin/python/") # python 2

PARSING_PYTHON_SCRIPT <- here("exploratory_studies/16_wiki_native/01_get_links/process_interlanguage_links.py")
COMPRESSED_LINK_FILES <- here("exploratory_studies/16_wiki_native/data/01_raw_links/")

source_python(PARSING_PYTHON_SCRIPT)

# DO THE THING
all_files <- list.files(COMPRESSED_LINK_FILES, full.names = T)
walk(all_files, parse)
