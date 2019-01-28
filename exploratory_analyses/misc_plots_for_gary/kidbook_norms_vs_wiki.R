# get gender bias score for each word using same method used in Caliskan
# in english, wiki compared to normes we collected for kdibook project

library(tidyverse)
library(data.table)
library(here)

MALE_WORDS <- c("son", "his","him","he", "brother","boy", "man", "male") 
FEMALE_WORDS <- c("daughter", "hers", "her", "she",  "sister", "girl", "woman", "female")
TARGET_NORMS <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/KIDBOOK_GENDER/analysis/9_collecting_gender_norms/mean_gender_ratings.csv" 
MODEL_SOURCE <- "wiki" # sub or wiki

if (MODEL_SOURCE == "sub"){
  MODEL_PATH <- "/Volumes/wilbur_the_great/subtitle_models/sub.en.vec"
  OUTFILE  <- here("data/study1a/processed/gender_bias_by_word_english_sub.csv")
} else if (MODEL_SOURCE == "wiki"){
  MODEL_PATH <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/VOCAB_SEEDS/analyses/0_exploration/wiki.en.vec" 
  OUTFILE  <- "gender_bias_by_word_english_wiki.csv"
}

model <- fread(
  MODEL_PATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("word",
                unlist(lapply(2:301, function(x) paste0("V", x)))))

kid_norms <- read_csv(TARGET_NORMS) %>%
  select(word) %>%
  rowwise() %>%
  mutate(word =  str_split(word, " ", simplify = T)[1],
         word = tolower(word)) %>%
  distinct()

kid_word_coordinates <-  kid_norms %>%
  left_join(model)  %>%
  mutate(type = "our", 
         gender = NA)

target_word_coordinates <- model %>%
  filter(word %in% c(MALE_WORDS, FEMALE_WORDS)) %>%
  mutate(type = "target",
         gender = ifelse(word %in% MALE_WORDS, "male", "female"))

all_words <- bind_rows(kid_word_coordinates, target_word_coordinates)  %>%
  select(word, type, gender, everything())

get_gender_score <- function(this_word, all_words_df){
  print(this_word)
  mat <- all_words_df %>%
    filter((word == this_word & type == "our")| type == "target")
  
  word_word_dists <- coop::cosine(t(as.matrix(mat[,c(-1, -2, -3)])))
  
  wide_word_word_dists <- word_word_dists %>%
    as.data.frame()  %>%
    mutate(word1 =  mat$word,
           gender = mat$gender) %>%
    select(word1,gender, everything())
  
  names(wide_word_word_dists)  = c("word1", "gender", mat$word)
  
  long_word_word_dists <- gather(wide_word_word_dists, "word2", "cos_dist", -word1, -gender) %>%
    select(word1, word2, gender, everything()) %>%
    filter(word2 == this_word & word1 != this_word) 
  
  try({
    long_word_word_dists %>%
      group_by(gender)%>%
      summarize(mean_cos_dist = mean(cos_dist)) %>%
      spread(gender, mean_cos_dist)  %>%
      mutate(male_score = male - female,
             word = this_word) %>%
      rename(female_target = female,
             male_target = male) %>%
      select(word, everything())
  })
}

# this is slow....
crit_dists <- map(kid_norms$word, 
                     get_gender_score, 
                     all_words)

crit_dists_df <- keep(crit_dists, ~length(.) > 1) %>%
  bind_rows()

# write_csv(crit_dists_df, OUTFILE)


kid_norms <- read_csv(TARGET_NORMS) %>%
  select(word, mean_gender_rating)

all_data <- crit_dists_df %>%
  left_join(kid_norms)

cor.test(all_data$male_score, all_data$mean_gender_rating)

pdf("wiki_vs_our_gender_norms.pdf", width = 5, height = 5)
ggplot(all_data, aes(x = -male_score, y= mean_gender_rating)) +
  xlab("Embedding Gender Score (male)") +
  ylab("Human Gender Rating (male)") +
  annotate("text", label = "r = .61", x = .15, y = 2, color = "red", size = 7) +
  geom_point(alpha = .2) +
  geom_smooth(method = "lm", size = 1.3) +
  theme_classic() +
  theme(legend.position = "none" ,
        axis.line = element_line(size = 1.2),
        axis.ticks = element_line(size = 1),
        strip.background = element_rect(colour="white", fill="white"),
        text = element_text(size = 16))
dev.off()
  
