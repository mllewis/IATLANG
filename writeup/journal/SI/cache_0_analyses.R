# Mixed-effect model Study country age analyses, cached for SM
library(tidyverse)
library(here)
library(lme4)
library(sjPlot)

BY_PARTICIPANT_DF <-  here("data/study0/processed/by_participant_df.csv")
BY_COUNTRY_DF <- here("data/study0/processed/by_country_df.csv")
MODEL_OUTPATH_0a <- here("writeup/journal/SI/data/study_0a_model_print.txt")
MODEL_OUTPATH_0b <- here("writeup/journal/SI/data/study_0b_model_print.txt")


country_iat <- read_csv(BY_COUNTRY_DF) %>%
  select(country_code, median_country_age, per_women_stem_2012_2017)


participant_iat <- read_csv(BY_PARTICIPANT_DF,
                            col_types = list(
                              country_code = "c",
                              country_name = "c",
                              sex = "i",
                              log_age = "d",
                              education = col_skip(),
                              log_age = "d",
                              overall_iat_D_score = "d",
                              order = "i",
                              explicit_dif = "d",
                              es_iat_sex_age_order_explicit_resid = "d",
                              es_iat_sex_age_order_implicit_resid = "d"
                            )
                  ) %>%
  left_join(country_iat)


# median country age predicting iat
study_0a_model <- participant_iat %>%
  lmer(overall_iat_D_score ~ sex + log_age + order + median_country_age + (1|country_code),
       data = .)

tab_model(study_0a_model,
                   show.p = F,
                   show.stat = T,
                   show.se = T,
                   string.se = "SE",
                   show.ci = F,
                   show.r2 = T,
                   file = MODEL_OUTPATH_0a,
                   pred.labels = c("(intercept)","median country age", "sex (M)",  "task order",
                                   "log age"))

# median country age AND stem predicting iat

study_0b_model <- participant_iat %>%
  lmer(overall_iat_D_score ~ sex + log_age + order +
         median_country_age + per_women_stem_2012_2017 + (1|country_code),
       data = .)


tab_model(study_0b_model,
                   show.p = F,
                   show.stat = T,
                   show.se = T,
                   string.se = "SE",
                   show.ci = F,
                   show.r2 = T,
                   file = MODEL_OUTPATH_0b,
                   pred.labels = c("(intercept)", "median country age","% women in STEM",
                                   "task order", "sex (M)",
                                   "log age"))






