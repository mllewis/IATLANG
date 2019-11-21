# cache pairwise correlations between vars for SI table that include partial correlations
library(tidyverse)
library(here)
library(numform)
source(here("writeup/journal/helpers/psych_to_mat.R"))

TIDY_MEASURES_DF <- here("writeup/journal/SI/data/tidy_measures.csv")
CACHE_TABLE_PATH <- here("writeup/journal/SI/data/cached_corr_table.csv")

tidy_measures <- read_csv(TIDY_MEASURES_DF) %>%
  select(median_country_age, es_iat_sex_age_order_explicit_resid,
         es_iat_sex_age_order_implicit_resid, per_women_stem_2012_2017,
         lang_es_sub, lang_es_wiki,
         lang_es_wiki_native,
         mean_prop_distinct_occs,
         subt_occu_semantics_fm,
         wiki_occu_semantics_fm,
         wiki_native_occu_semantics_fm)  %>%
  rename(`Residualized Implicit Bias (IAT)` = "es_iat_sex_age_order_implicit_resid",
         `Residualized Explicit Bias` = "es_iat_sex_age_order_explicit_resid",
         `Language IAT (Subtitle)` = "lang_es_sub",
         `Language IAT (Wikipedia)` = "lang_es_wiki",
         `Language IAT\n(Wikipedia, untranslated)` = "lang_es_wiki_native",
         `Occupation Bias (Subtitle)` = "subt_occu_semantics_fm",
         `Occupation Bias (Wikipedia)` = "wiki_occu_semantics_fm",
         `Occupation Bias (Wikipedia, untranslated)` = "wiki_native_occu_semantics_fm",
         `Prop. Gender-Distinct Labels` = "mean_prop_distinct_occs",
         `Percent Women in STEM` = "per_women_stem_2012_2017",
         `Median Country Age` = "median_country_age")

simple_corr <- psych::corr.test(tidy_measures, adjust = "none")$r %>%
  as_tibble(rownames = "rowname") %>%
  gather("var2", "simple_r", -rowname)

simple_corr_p <- psych::corr.test(tidy_measures, adjust = "none")$p %>%
  as_tibble(rownames = "rowname") %>%
  gather("var2", "simple_p", -rowname)

partial_psych_obj <- psych::partial.r(data = tidy_measures,
                                      x = 2:11, y = "Median Country Age" )

partial_corr <- psych::corr.p(partial_psych_obj, n = nrow(tidy_measures) - 1,
                              adjust = "none")$r %>%
  psych_to_mat() %>%
  as_tibble(rownames = "rowname") %>%
  gather("var2", "partial_r", -rowname)

partial_corr_p <- psych::corr.p(partial_psych_obj, n = nrow(tidy_measures) - 1,
                                adjust = "none")$p %>%
  psych_to_mat() %>%
  as_tibble(rownames = "rowname") %>%
  gather("var2", "partial_p", -rowname)

tidy_corrs <- simple_corr %>%
  left_join(simple_corr_p) %>%
  left_join(partial_corr) %>%
  left_join(partial_corr_p)

print_tidy_corrs <- tidy_corrs %>%
  filter(rowname != var2) %>%
  mutate_at(vars(simple_r, partial_r), ~ format(round(., 2), nsmall = 2) %>%  f_num(., digits = 2)) %>%
  mutate_at(vars(simple_r, partial_r), ~
              case_when(str_detect(.,"^-") ~ ., TRUE ~ paste0("\\ ", .))) %>% # add leading space so decimals align
  mutate_at(vars(simple_p, partial_p), ~ case_when(
    . < .01 ~ "**", . < .05 ~ "*",  . < .1 ~ "+", TRUE ~ "")) %>%
  mutate(r_partial_print = case_when(
    !is.na(partial_r) ~ paste0(partial_r, partial_p),TRUE ~ ""),
    r_simple_print = paste0(simple_r, simple_p)) %>%
  select(rowname, var2, r_simple_print, r_partial_print)

tidy_corrs_to_print_simple <- print_tidy_corrs %>%
  select(-r_partial_print) %>%
  spread(var2, r_simple_print)  %>%
  mutate_all(funs(replace_na(., ""))) %>%
  select("rowname",
         contains("Residualized"), contains("STEM"), "Language IAT (Subtitle)",
         "Language IAT (Wikipedia)",
         "Language IAT\n(Wikipedia, untranslated)",
         contains("Occupation Labels"), "Occupation Bias (Subtitle)",
         "Occupation Bias (Wikipedia)",
         "Occupation Bias (Wikipedia, untranslated)",
         contains("Age")) %>%
  rename(" " = "rowname")

tidy_corrs_to_print_partial <- print_tidy_corrs %>%
  select(-r_simple_print) %>%
  spread(var2, r_partial_print)  %>%
  mutate_all(funs(replace_na(., ""))) %>%
  select("rowname",
         contains("Residualized"), contains("STEM"), "Language IAT (Subtitle)",
"Language IAT (Wikipedia)",
"Language IAT\n(Wikipedia, untranslated)",
         contains("Occupation Labels"),  "Occupation Bias (Subtitle)",
"Occupation Bias (Wikipedia)",
"Occupation Bias (Wikipedia, untranslated)") %>%
  rename(" " = "rowname") %>%
  mutate("Median Country Age" = " ")

tidy_corrs_to_print_reordered_simple <- tidy_corrs_to_print_simple[c(10,11,8,2,3,1,9,5,7,6,4),]
tidy_corrs_to_print_reordered_partial <- tidy_corrs_to_print_partial[c(10,11,8,2,3,1,9,5,7,6,4),]

tidy_corrs_to_print_reordered <- bind_rows(tidy_corrs_to_print_reordered_simple, tidy_corrs_to_print_reordered_partial) %>%
  slice(1:(n()-1))

write_csv(tidy_corrs_to_print_reordered, CACHE_TABLE_PATH)



