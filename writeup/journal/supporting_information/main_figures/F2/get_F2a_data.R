# get F2a data
library(tidyverse)
library(here)
library(broom)
library(langcog)

DATA_OUTFILE <- here("writeup/journal/supporting_information/main_figures/F2/f2a_csv")

BEHAVIORAL_IAT_PATH <- here("data/study0/processed/by_language_df.csv")
iat_behavioral_es <- read_csv(BEHAVIORAL_IAT_PATH) %>%
  rename(language_code = "wiki_language_code") %>%
  select(language_code, median_country_age,
         es_iat_sex_age_order_explicit_resid,
         es_iat_sex_age_order_implicit_resid,
         per_women_stem_2012_2017,
         n_participants)

# study 1b
LANG_IAT_PATH <- here("data/study1b/iat_es_lang.csv")
iat_lang_es <- read_csv(LANG_IAT_PATH)

LANGUAGE_NAME_PATH <- here("data/study0/processed/lang_name_to_wiki_iso.csv")
language_names <- read_csv(LANGUAGE_NAME_PATH) %>%
  rename(language_code = wiki_language_code) %>%
  distinct(language_code, .keep_all = TRUE)

# combine lang and behavioral and family info
all_es <- left_join(iat_behavioral_es, iat_lang_es, by = "language_code") %>%
  left_join(language_names)

# remove exclusions and fix croatian to be mean of hr and sr (only in wiki)
EXCLUSIONS_PATH <- here("data/study1b/language_exclusions.csv")
exclusions <- read_csv(EXCLUSIONS_PATH)

hr_new_wiki <- mean(c(filter(iat_lang_es, language_code == "hr") %>%  pull(lang_es_wiki),
                      filter(iat_lang_es, language_code == "sr") %>%  pull(lang_es_wiki)))

all_es_tidy <- all_es %>%
  left_join(exclusions) %>%
  mutate(lang_es_wiki = case_when(exclude_wiki == TRUE ~ NA_real_,
                                  TRUE ~ lang_es_wiki),
         lang_es_sub = case_when(exclude_sub == TRUE ~ NA_real_,
                                 TRUE ~ lang_es_sub)) %>%
  select(-exclude_wiki, -exclude_sub) %>%
  mutate(lang_es_wiki = case_when(language_code == "hr" ~ hr_new_wiki,
                                  TRUE ~ lang_es_wiki),
         lang_es_sub = case_when(language_code == "hr" ~ NA_real_, # sr is missing from sub
                                 TRUE ~ lang_es_sub))  %>%
  filter(language_code != "zu")  # exclude proportion overlap measure (study 2) in zulu

# note: sample size discrepancy between study 0 (657335)  and study 1 (656636) is due to the exclusion fo zulu
all_es_tidy2 <- all_es_tidy %>%
  select(language_code, language_name, es_iat_sex_age_order_implicit_resid,
         es_iat_sex_age_order_explicit_resid, lang_es_wiki, lang_es_sub, n_participants) %>%
  rename(implicit_behavioral_es = es_iat_sex_age_order_implicit_resid,
         explicit_behavioral_es = es_iat_sex_age_order_explicit_resid)

write_csv(all_es_tidy2, DATA_OUTFILE)

#  all_es_tidy2 %>%
#   ggplot(aes(x = lang_es_wiki, y = implicit_behavioral_es, size = n_participants)) +
#   #facet_wrap( . ~ model) +
#   geom_smooth(method = "lm", alpha = .1, size = .9) +
#   geom_point(alpha = .2) +
#   ggrepel::geom_text_repel(aes(label = language_name),
#                            size = 1.8, box.padding = 0.1) +
#   scale_x_continuous(breaks = c(-.25, -0,.25, .5,.75, 1),
#                      label = c("-.25\n(weaker)", "0", ".25", ".5", ".75", "1\n(stronger)"),
#                      limits = c(-.25, 1)) +
#   scale_y_continuous(breaks = c(-.075, -.05, -.025, 0, .025, .05),
#                      label = c("-.075\n(weaker)", "-.05", "-.025", "0", ".025", ".05\n(stronger)"),
#                      limits = c(-.075, .05) ) +
#   scale_size(trans = "log10", labels = scales::comma, name = "N participants") +
#   ggtitle("Implicit and Linguistic\nMale-Career Association") +
#   ylab("Implicit Male-Career Association\n(residualized effect size)") +
#   xlab("Language Male-Career Association\n(effect size)") +
#   theme_classic(base_size = 7)   +
#   theme(legend.position = "right",
#         axis.line = element_line(size = .8))
#
# cor.test(all_es_tidy2$implicit_behavioral_es,
#          all_es_tidy2$lang_es_wiki)
#
# cor.test(all_es_tidy2$implicit_behavioral_es,
#          all_es_tidy2$lang_es_sub)
#
# cor.test(all_es_tidy2$explicit_behavioral_es,
#          all_es_tidy2$lang_es_sub)
#
# cor.test(all_es_tidy2$explicit_behavioral_es,
#          all_es_tidy2$lang_es_wiki)


