# process wiki dump into target (native language)  files
# download wiki dump, select files we want based on language link csv, write new file with only text of relevant files
# finally, delete wiki dump
library(reticulate)
library(tidyverse)
library(here)
library(curl)
library(R.utils)
use_python("/Users/mollylewis/anaconda2/envs/py3/bin/python/") # python 3

PYTHON_SCRIPT <- here("exploratory_studies/16_wiki_native/02_get_wiki_text/get_wiki_text_from_id.py") # modified version fo Gary's scripts (wd must be current file to read modified wiki_dump_reader)
RAW_XML_PATH <- "https://dumps.wikimedia.org/"
RAW_XML_PATH_ZIPPED <- here("exploratory_studies/16_wiki_native/data/02_text/temp_xml.xml.bz2")
LANGLINKS_CSV_PATH <- here("exploratory_studies/16_wiki_native/data/01_processed_links/")
TEXT_OUTFILE <- here("exploratory_studies/16_wiki_native/data/02_text/")

LANGS <- c("ar", "da", "de", "en", "es", "fa", "fi", "fr", "he", "hi", "hr", "id", "it", "ja",
           "ko", "ms", "nl", "no", "pl", "pt", "ro", "sv", "tl", "tr", "zh")


LANGS <- c("da", "de", "en", "es", "fa", "fi", "fr", "he", "hi", "hr", "id", "it", "ja",
           "ko", "ms", "nl", "no", "pl", "pt", "ro", "sv", "tl", "tr", "zh")

source_python(PYTHON_SCRIPT)

process_target_articles_one_lang <- function(current_lang,
                                             raw_xml_path,
                                             raw_xml_path_zipped,
                                             langlinks_csv_path) {

  print(paste("%%%%%%%%%%%%%", current_lang, "%%%%%%%%%%%%%"))

  # download file
  print("====downloading raw xml====")
  current_raw_xml_path <- paste0(raw_xml_path, current_lang, "wiki/latest/", current_lang, "wiki-latest-pages-articles.xml.bz2")
  curl_download(current_raw_xml_path, raw_xml_path_zipped, quiet = FALSE)

  # unzip file
  print("====unzipping raw xml====")
  bunzip2(raw_xml_path_zipped)

  # get target texts based on link file and raw xml
  print("====processing/writing target articles====")
  link_path <- paste0(langlinks_csv_path, current_lang, "wiki-latest-langlink-parsed.csv")
  current_raw_xml_path_unzipped <- str_remove(raw_xml_path_zipped, ".bz2")
  get_native_wiki_text(link_path, current_raw_xml_path_unzipped, current_lang)

  # remove large raw xml files
  file.remove(raw_xml_path_zipped)
  file.remove(current_raw_xml_path_unzipped)

}

walk(LANGS,
     process_target_articles_one_lang,
     RAW_XML_PATH,
     RAW_XML_PATH_ZIPPED,
     LANGLINKS_CSV_PATH)
