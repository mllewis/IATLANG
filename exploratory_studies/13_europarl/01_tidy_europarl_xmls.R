# convert all costep europarl files into tidy csvs (https://pub.cl.uzh.ch/wiki/public/costep/start)
library(tidyverse)
library(xml2)

FILE_PATH <- "/Volumes/wilbur_the_great/europarl/sessions/"
OUTFILE <- "/Volumes/wilbur_the_great/europarl/tidy_csvs/"

process_one_turn_type <- function(single_turn){
  map(single_turn, ~pluck(.$p, attr_getter("type"))) %>%
   unname() %>%
   unlist()
}

process_one_turn_language <-  function(single_turn){
  map(single_turn, ~pluck(., attr_getter("language")))  %>%
    unname() %>%
    unlist()
}

process_one_turn_text <- function(single_turn){
  map(single_turn, ~.$p[[1]]) %>%
    unname() %>%
    unlist()
}

process_one_chapter <- function(one_chapter, chapter_id, this_session_name){
  this_chapter <- one_chapter %>%
    xml_find_all(".//turn")  #.// looks *beneath* current node
  
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
    
    chapter_turns %>%
      mutate(turn_text = map(turn, possibly(process_one_turn_text, NA)),
             turn_language = map(turn, process_one_turn_language),
             turn_type = map(turn, possibly(process_one_turn_type, NA)),
             length_text = map_dbl(turn_text, length),
             length_language = map_dbl(turn_language, length)) %>% 
      filter(length_text == length_language) %>%  # text is missing for a few (but lang present)
      select(-turn, -length_text, -length_language) %>%
      unnest() %>%
      filter(turn_type == "speech") %>% # exclude transcriber comments
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
                                 process_one_chapter, 
                                 session_name)
  
  tidy_session <- bind_rows(processed_session_data) %>%
    mutate_at(vars(session, speaker_language, speaker_name, speaker_country), as.factor) %>%
    mutate_at(vars(chapter, turn_id), as.numeric)
  
  out_file_name <- paste0(out_file_name, session_name, "_tidy.csv")
  write_csv(tidy_session, out_file_name)
}

### DO THE THING
#list.files(FILE_PATH, full.names = T) %>%
setdiff(total, complete) %>%
  map(~paste0(FILE_PATH, ., ".xml")) %>%
  unlist() %>%
  walk(possibly(process_one_session, NA), OUTFILE)


##################
complete <- list.files("/Volumes/wilbur_the_great/europarl/tidy_csvs/", full.names = T)  %>%
  basename() %>%
  map(~str_split(., "_tidy.csv")[[1]][1]) %>%
  unlist()

total <- list.files(FILE_PATH, full.names = T) %>%
  basename() %>%
  map(~str_split(., ".xml")[[1]][1]) %>%
  unlist()

