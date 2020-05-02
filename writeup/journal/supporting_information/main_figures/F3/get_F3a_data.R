# get F3 data
library(tidyverse)
library(here)
library(broom)
library(ggrepel)

DATA_OUTFILE <- here("writeup/journal/supporting_information/main_figures/F3/F3.csv")


OCCUPATION_OVERLAP_PATH <- here('data/study2/occupation_gender_scores.csv')
by_lang_scores_tidy <- read_csv(OCCUPATION_OVERLAP_PATH)

# Occupation semantics by language
BY_LANGUAGE_OCCUPATION_PATH  <- here("data/study2/occupation_gender_score_by_language.csv")
occupation_semantics <- read_csv(BY_LANGUAGE_OCCUPATION_PATH)

# Behavioral IAT by languages measure
BEHAVIORAL_IAT_PATH <- here("data/study0/processed/by_language_df.csv")
iat_behavioral_es <- read_csv(BEHAVIORAL_IAT_PATH) %>%
  rename(language_code = "wiki_language_code") %>%
  select(language_code, median_country_age,
         prop_male,log_age, es_iat_sex_age_order_explicit_resid,
         es_iat_sex_age_order_implicit_resid, per_women_stem_2012_2017, n_participants)

LANGUAGE_NAME_PATH <- here("data/study0/processed/lang_name_to_wiki_iso.csv")
language_names <- read_csv(LANGUAGE_NAME_PATH) %>%
  rename(language_code = wiki_language_code) %>%
  distinct(language_code, .keep_all = TRUE)

all_es_tidy2 <- full_join(by_lang_scores_tidy, iat_behavioral_es) %>%
  left_join(occupation_semantics)  %>%
  left_join(language_names) %>%
  filter(language_code != "zu")  %>% # exclude zulu as in study 1b
  select(language_code, language_name, es_iat_sex_age_order_implicit_resid, wiki_gender_diff_score_fm_abs,
         subt_gender_diff_score_fm_abs, mean_prop_distinct_occs, n_participants)

write_csv(all_es_tidy2, DATA_OUTFILE)


#distinct_behavior_corr <- cor(all_es_tidy2$mean_prop_distinct_occs,
#                              all_es_tidy2$es_iat_sex_age_order_implicit_resid)

#semantic_cor <- cor.test(all_es_tidy2$wiki_gender_diff_score_fm_abs,
   #                           all_es_tidy2$es_iat_sex_age_order_implicit_resid, na.rm = T)
#
# # implicit behavioral ~ prop distinct
# ggplot(all_es_tidy2, aes(x = mean_prop_distinct_occs,
#                                          y = es_iat_sex_age_order_implicit_resid)) +
#   geom_smooth(method = "lm", alpha = .1) +
#   geom_text_repel(aes(label = language_name), size = 1.8, segment.size = .3) +
#   scale_x_continuous(breaks = c(0,.25,.5, .75, 1),
#                      label = c("0.00\n", "0.25", "0.50", ".75", "1.00\n"),
#                      limits = c(-.01, 1.01)) +
#   scale_y_continuous(breaks = c(-.075, -.05, -.025, 0, .025, .05),
#                      label = c("-.075\n(weaker)", "-.05", "-.025", "0", ".025", ".05\n(stronger)") ,
#                      limits = c(-.075, .05) ) +
#   geom_point(alpha = .2, aes(size = n_participants)) +
#   scale_size(trans = "log10",
#              labels = scales::comma, name = "N participants") +
#   ggtitle("Implicit Male-Career Association\nand Gendered Occupation Terms") +
#   ylab("Implicit Male-Career Association\n(residualized effect size)") +
#   xlab("Prop. Gender-Specific Occupation Terms") +
#
#   theme_classic(base_size = 7)   +
#   theme(legend.position = "none",
#         axis.line = element_line(size = .8))
#
# all_es_tidy2 %>%
#   select(language_name, subt_gender_diff_score_fm_abs,
#          wiki_gender_diff_score_fm_abs,
#          es_iat_sex_age_order_implicit_resid, n_participants) %>%
#   ggplot(aes(x = wiki_gender_diff_score_fm_abs, y = es_iat_sex_age_order_implicit_resid,
#              size = n_participants)) +
#   geom_smooth(method = "lm", alpha = .1, size = .9) +
#   geom_text_repel(aes(label = language_name),
#                   size = 1.8, box.padding = 0.1) +
#   geom_point(alpha = .2) +
#
#   scale_x_continuous(breaks = c( 0, .025, .05, .075, .1),
#                      label = c("0\n(netural)", ".025", ".05", ".075", ".1\n(gendered)"),
#                      limits = c(0,.1)) +
#   scale_y_continuous(breaks = c(-.075, -.05, -.025, 0, .025, .05),
#                      label = c("-.075\n(weaker)", "-.05", "-.025", "0", ".025", ".05\n(stronger)"),
#                      limits = c(-.075, .05) ) +
#   scale_size(trans = "log10", labels = scales::comma,
#              name = "N participants") +
#   ggtitle("Implicit Male-Career Association\nand Gender Associations for Occupation Terms") +
#   ylab("Implicit Male-Career Association\n(residualized effect size)") +
#   xlab("Genderness of Occupation Terms") +
#   theme_classic(base_size = 7)   +
#   theme(legend.position = "right",
#         axis.line = element_line(size = .8))
#
#
