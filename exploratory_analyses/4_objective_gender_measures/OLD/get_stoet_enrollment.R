# extract enrollment data from Stoet  (2018) Supplmentary materials (S1)

library("tabulizer")
library("tidyverse")

pdf_path  <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/IATLANG/resources/language_social/Misersky2014_Article_NormsOnTheGenderPerceptionOfRo.pdf"
OUTFILE <- "stoet_enrollment.csv"

t = extract_areas(pdf_path, 8)

t_df <- map_df(t, ~as.data.frame(.) %>% select(1:3))

names(t_df)  = c("country", "ggi", "f_enrollment")

tidy_enrollment_df <- t_df %>% 
  separate(f_enrollment, sep = " ", into = "f_enrollment") %>%
  select(country, f_enrollment)

write_csv(tidy_enrollment_df, OUTFILE) # note! still needs manual tiying for countries with two rows for name


  

