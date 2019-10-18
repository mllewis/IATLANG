# convert all costep europarl files into tidy csvs (https://pub.cl.uzh.ch/wiki/public/costep/start)
library(tidyverse)
library(xml2)
library(parallel)

FILE_PATH <- "/Volumes/wilbur_the_great/europarl/sessions/"
OUTFILE <- "/Volumes/wilbur_the_great/europarl/tidy_csvs/"

turn_type_processing <- function(this_turn){
  turn_type <- pluck(this_turn$p, attr_getter("type")) # deals with empty ps
  ifelse(is.null(turn_type), NA, turn_type)
}

process_one_turn_type <- function(single_turn){
  map(single_turn, turn_type_processing) %>%
   unname() %>%
   unlist()
}

turn_language_processing <- function(this_turn){
  turn_language <- pluck(this_turn, attr_getter("language"))
  ifelse(is.null(turn_language), NA, turn_language)
}

process_one_turn_language <-  function(single_turn){
  map(single_turn, turn_language_processing)  %>%
    unname() %>%
    unlist()
}

turn_text_processing <- function(this_turn){ # this deals with the fact that there can be multiple paragraph tags per turn/language
  map(this_turn, ~.[[1]][1]) %>%
    unlist(use.names = F) %>%
    paste(collapse = " ")
}

process_one_turn_text <- function(single_turn){
  map(single_turn, turn_text_processing) %>%
    unname() %>%
    unlist()
}

process_one_chapter <- function(one_chapter, chapter_id, this_session_name){
  this_chapter <- one_chapter %>%
    xml_find_all(".//turn")  #.// looks *beneath* current node
  print(chapter_id)
   if (length(this_chapter) > 0){
   
    chapter_turns <- tibble(
      session = this_session_name,
      chapter = chapter_id,
      turn = as_list(this_chapter %>% xml_children()),
      turn_id = this_chapter %>% xml_attr(attr = "id"),
      speaker_language = this_chapter %>% xml_children() %>% xml_attr(attr = "language"),
      speaker_name = this_chapter %>% xml_children() %>% xml_attr(attr = "name"),
      speaker_country = this_chapter %>% xml_children() %>% xml_attr(attr = "country") 
    )
    
   tidy_chapter <- chapter_turns %>%
      mutate(turn_text = map(turn, possibly(process_one_turn_text, NA)),
             turn_language = map(turn, process_one_turn_language),
             turn_type = map(turn, possibly(process_one_turn_type, NA)),
             length_text = map_dbl(turn_text, length),
             length_language = map_dbl(turn_language, length))  %>%
     filter(length_text == length_language)

      tidy_chapter  %>%
        select(-turn, -length_text, -length_language) %>%
        unnest() %>%
        filter(turn_type == "speech") %>% # exclude transcriber comments/empty ps
        select(-turn_type)

 }
}

process_one_session <- function(path_file_name, out_file_name){
  print(path_file_name)
  
  session_name <- path_file_name %>%
    basename() %>%
    str_split(".xml") %>%
    pluck(1,1)
  
  session_data <- read_xml(path_file_name)
  
  all_chapters <- list(chapters = xml_find_all(session_data, "//chapter"),
                       chapter_id = xml_find_all(session_data, "//chapter") %>% xml_attr( attr="id"))
  
  processed_session_data <- map2(all_chapters$chapters,
                                 all_chapters$chapter_id, 
                                 possibly(process_one_chapter, NULL), 
                                 session_name)
  
  tidy_session <- processed_session_data %>%
    bind_rows() %>%
    mutate_at(vars(session, speaker_language, speaker_name, speaker_country, chapter), as.factor) %>%
    mutate_at(vars(turn_id), as.numeric)
  
  out_file_name <- paste0(out_file_name, session_name, "_tidy.csv")
  write_csv(tidy_session, out_file_name)
}

# INITIATE CLUSTER
cluster <- makeCluster(6, type = "FORK")

# DO THE THING (IN PARALLEL)
parLapply(cluster, 
          list.files(FILE_PATH, full.names = T)[1:796], 
          possibly(process_one_session, NA),
          OUTFILE) 

########################################################################
### DO THE THING
list.files(FILE_PATH, full.names = T)[1:796] %>%
  list.files(FILE_PATH, full.names = T)[1] %>%
  rev() %>%
  unlist() %>%
  walk(process_one_session, OUTFILE)

setdiff(total, complete) %>%
  map(~paste0(FILE_PATH, ., ".xml")) %>%
  unlist() %>%
  walk(possibly(process_one_session, NA), OUTFILE)

complete <- list.files("/Volumes/wilbur_the_great/europarl/tidy_csvs/", full.names = T)  %>%
  basename() %>%
  map(~str_split(., "_tidy.csv")[[1]][1]) %>%
  unlist()

total <- list.files(FILE_PATH, full.names = T) %>%
  basename() %>%
  map(~str_split(., ".xml")[[1]][1]) %>%
  unlist()

