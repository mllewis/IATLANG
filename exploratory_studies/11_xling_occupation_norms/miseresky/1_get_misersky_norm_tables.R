# extract tables from miseresky

library("tabulizer")
library("tidyverse")

pdf_path  <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/IATLANG/resources/language_social/Misersky2014_Article_NormsOnTheGenderPerceptionOfRo.pdf"

# translations
OUTFILE1 <- "miseresky_translations_raw.csv"

t = extract_tables(pdf_path, 8:21)

dft <- as.data.frame(do.call(rbind,t))

write_csv(dft, OUTFILE1)

# norms
OUTFILE2 <- "miseresky_norms_raw.csv"

n = extract_tables(pdf_path, 22:31)
walk(1:10, ~write.csv(data.frame(n[[.]]), paste0("p", ., "_raw.csv")))

write_csv(dfn, OUTFILE2)




  

