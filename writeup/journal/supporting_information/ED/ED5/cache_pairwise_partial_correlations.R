# cache pairwise correlations between vars for SI table that include partial correlations
library(tidyverse)
library(here)
library(numform)

TIDY_MEASURES_DF <- here("writeup/journal/SI/data/tidy_measures.csv")
CACHE_TABLE_PATH <-  here("writeup/journal/supporting_information/ED/ED5/cached_partial_corr_table.csv")

tidy_measures <- read_csv(TIDY_MEASURES_DF) %>%
  select(median_country_age, es_iat_sex_age_order_explicit_resid,
         es_iat_sex_age_order_implicit_resid, per_women_stem_2012_2017,
         lang_es_sub, lang_es_wiki,
         lang_es_wiki_native,
         mean_prop_distinct_occs,
         subt_gender_diff_score_fm_abs,
         wiki_gender_diff_score_fm_abs,
         wiki_native_gender_diff_score_fm_abs)  %>%
  rename(`Implicit Male-Career Assoc. (IAT)` = "es_iat_sex_age_order_implicit_resid",
         `Explicit Male-Career Assoc.` = "es_iat_sex_age_order_explicit_resid",
         `Male-Career Assoc. (Subt.)` = "lang_es_sub",
         `Male-Career Assoc. (Wiki.)` = "lang_es_wiki",
         `Male-Career Assoc.\n(Wikipedia, untranslated)` = "lang_es_wiki_native",
         `Lang. Occup. Genderness (Subt.)` = "subt_gender_diff_score_fm_abs",
         `Lang. Occup. Genderness (Wiki.)` = "wiki_gender_diff_score_fm_abs",
         `Lang. Occup. Genderness\n(Wiki., untranslated)` = "wiki_native_gender_diff_score_fm_abs",
         `Prop. Gendered Occup. Terms` = "mean_prop_distinct_occs",
         `Percent Women in STEM` = "per_women_stem_2012_2017",
         `Median Country Age` = "median_country_age")

simple_corr <- psych::corr.test(tidy_measures, adjust = "none")$r %>%
  as_tibble(rownames = "rowname") %>%
  gather("var2", "simple_r", -rowname)

simple_corr_p <- psych::corr.test(tidy_measures, adjust = "none")$ci %>%
  as_tibble(rownames = "rowname") %>%
  gather("var2", "simple_p", -rowname)

partial_psych_obj <- psych::partial.r(data = tidy_measures,
                                      x = 2:11, y = "Median Country Age")

partial_corr <- psych::corr.p(partial_psych_obj, n = nrow(tidy_measures) - 1, # 1 indicates number of variables partialed out
                              adjust = "none")$ci %>%
  rownames_to_column() %>%
  bind_cols(simple_corr %>% filter(rowname != "Median Country Age", var2 != "Median Country Age")) %>%
  select(rowname1, var2, r, lower, upper, p)


print_tidy_corrs <- partial_corr %>%
  filter(rowname1 != var2) %>%
  mutate_at(vars(lower, r, upper), ~ format(round(., 2), nsmall = 2) %>%  f_num(., digits = 2)) %>%
  mutate(p = case_when(p < .001 ~ "<.001",
                       TRUE ~ as.character(round(p, 2))),
         r_print = paste0("$", r,"$", " $[", lower, ",", upper, "]$", ", ", p)) %>%
  select(rowname1, var2, r_print)

tidy_corrs_to_print_partial<- print_tidy_corrs %>%
  spread(var2, r_print)  %>%
  mutate_all(funs(replace_na(., ""))) %>%
  select("rowname1",
         contains("Explicit"), contains("Implicit"),  contains("STEM"), "Male-Career Assoc. (Subt.)",
         "Male-Career Assoc. (Wiki.)",
         contains("Occup. Terms"),  "Lang. Occup. Genderness (Subt.)",
         "Lang. Occup. Genderness (Wiki.)",
         contains("Age")) %>%
  rename(" " = "rowname1")

tidy_corrs_to_print_reordered_partial <- tidy_corrs_to_print_partial[c(1,2,9,7,8,10,4,5),]

write_csv(tidy_corrs_to_print_reordered_partial, CACHE_TABLE_PATH)



