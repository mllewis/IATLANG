# get F2b data
library(tidyverse)
library(here)
library(broom)

DATA_OUTFILE <- here("writeup/journal/supporting_information/main_figures/F2/F2b.csv")


## Method

STUDY1C_DATAPATH <- here("data/study1c/processed/long_form_confirmatory_behavior_and_language.csv")

full_1c_df <- read_csv(STUDY1C_DATAPATH)

tidy_lang_data <- full_1c_df %>%
  distinct(residence, domain, run, .keep_all = T) %>%
  select(residence, domain, run, lang_diff) %>%
  group_by(domain)%>%
  summarize(lang_diff = mean(lang_diff))

es_iat_tidy <- full_1c_df %>%
  group_by(residence, domain) %>%
  summarize(resid = mean(behavioral_effect_resid)) %>%
  spread(residence, resid) %>%
  mutate(behavioral_resid_es_diff = uk - us) %>%
  left_join(tidy_lang_data) %>%
  select(-uk, -us) %>%
  mutate(domain2 = str_replace_all(domain, " People", ""),
         domain2 = str_replace_all(domain2, " - ", "-"),
         domain2 = fct_recode(domain2, `Protein-Carbs.` = "Protein-Carbohydrates",
                              "Defense-Education" = "National Defense-Education",
                              "Labor-Management" = "Organized Labor-Management")) %>%
  select(-domain) %>%
  rename(domain = domain2,
         lang_es_diff = lang_diff) %>%
  select(domain, everything())

write_csv(es_iat_tidy, DATA_OUTFILE)

# corr1c <- cor.test(es_iat_tidy$behavioral_resid_es_diff,
#                    es_iat_tidy$lang_es_diff)$estimate
#
# ggplot(es_iat_tidy, aes(x = lang_es_diff, y = behavioral_resid_es_diff)) +
#   geom_smooth(method = "lm", alpha = .1) +
#   ggrepel::geom_text_repel(aes(label = domain), size = 1.2,
#                            segment.size = 0.2, segment.color = "grey", box.padding= 0.12) +
#   geom_point(size = .6) +
#   ylab("Implicit Association Difference\n(residualized effect size)") +
#   xlab("Language Association Difference\n(effect size)") +
#   #annotate("text", y = -.15, x = .9, label =
#   #           paste0(" italic(r) == ", f_num(corr1c, 2)),
#   #         color = "red", size = 2.6, parse = T) +
#   scale_x_continuous(breaks = c(-1.5, -1, -.5, 0, .5, 1),
#                      label = c("-1.5\n(US Greater)", "-1", "-.5" ,"0", ".5", "1\n(UK Greater)") , limits = c(-1.6, 1.2)) +
#   scale_y_continuous(breaks = c( -.2,-.1, 0, .1, .2, .3),
#                      label = c("-.2\n(US\nGreater)", "-.1", "0", ".1", ".2", ".3\n(UK\nGreater)"), limits = c(-.2, .32) ) +
#   ggtitle("Implicit and Linguistic Associations\nin British and American Participants") +
#   theme_classic(base_size = 7) +
#   theme(axis.line = element_line(size = .8))

