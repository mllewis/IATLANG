
```{r, parity_with_implicit}
subj_means_career_parity <- read_csv("data/study1/WEF_GGGR_Dataset_2016.csv", skip = 2) %>%
  select(-contains("rank")) %>%
  purrr::set_names(c("country_name", "overall_parity", "economic_parity", "education_parity", "health_parity", "political_parity"))  %>%
  mutate(country_name = replace(country_name, 
                                country_name == "United Kingdom", "UK"),
         country_name = replace(country_name, 
                                country_name == "United States", "United States of America"),
         country_name = replace(country_name, 
                                country_name == "Korea, Rep.", "Republic of Korea")) %>%
  add_row(country_name = "Hong Kong", overall_parity = 0.676) %>%
  add_row(country_name = "Taiwan", overall_parity = 0.676) %>%
  add_row(country_name = "Afghanistan", overall_parity = 0.556)

country_means_career_implicit <- iat_behavioral_tidy %>%
  group_by(country_name) %>%
  summarize(mean_iat = mean(overall_iat_D_score)) 

# by country correlations
country_means_career_parity <- subj_means_career_parity  %>%
  group_by(country_name) %>%
  summarize(overall_parity = mean(overall_parity, na.rm = T),
            economic_parity = mean(economic_parity, na.rm = T),
            education_parity = mean(education_parity, na.rm = T),
            health_parity = mean(health_parity, na.rm = T),
            political_parity = mean(political_parity, na.rm = T)) %>%
  right_join(country_means_career_implicit, by = "country_name") %>%
  right_join(country_means_career_explicit %>% select(-mean_iat), by = "country_name") 

cor.test(country_means_career_parity$overall_parity, country_means_career_parity$mean_iat) # ns
cor.test(country_means_career_parity$overall_parity, country_means_career_parity$mean_diff) # ns


```

```{r, parity_with_explicit}
## career
cor.test(country_means_career_parity$overall_parity, country_means_career_parity$mean_career) # primarily driven by norway

## family
cor.test(country_means_career_parity$overall_parity, country_means_career_parity$mean_family) # Marginal but in the wrong direction

## diff
cor.test(country_means_career_parity$overall_parity, country_means_career_parity$mean_diff) # no effect


ggplot(country_means_career_parity , aes(x = overall_parity, y = mean_career)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(country_means_career_parity , aes(x = overall_parity, y = mean_family)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(filter(country_means_career_parity, country_name != "Norway") , aes(x = overall_parity, y = mean_career)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(filter(country_means_career_parity, country_name != "Norway") , aes(x = overall_parity, y = mean_family)) +
  geom_point() +
  geom_smooth(method = "lm")

no_norway <- filter(country_means_career_parity, country_name != "Norway") 
cor.test(no_norway$overall_parity, no_norway$mean_career) 
cor.test(no_norway$overall_parity, no_norway$mean_family)
```


```{r, parity_with_implicit_by_gender}
country_means_career_implicit_gender <- iat_behavioral_tidy %>%
  group_by(country_name, sex) %>%
  summarize(mean_iat = mean(overall_iat_D_score)) 

# by country correlations
country_means_career_parity_gender <- subj_means_career_parity  %>%
  group_by(country_name) %>%
  summarize(overall_parity = mean(overall_parity, na.rm = T),
            economic_parity = mean(economic_parity, na.rm = T),
            education_parity = mean(education_parity, na.rm = T),
            health_parity = mean(health_parity, na.rm = T),
            political_parity = mean(political_parity, na.rm = T)) %>%
  right_join(country_means_career_implicit_gender, by = "country_name") 

country_means_career_parity_gender %>%
  group_by(sex) %>%
  do(tidy(cor.test(.$mean_iat, .$overall_parity)))
```

```{r, parity_with_explicit_by_gender}
country_means_career_explicit_gender <- subj_means_career_explicit %>%
  group_by(country_name, sex) %>%
  summarize(mean_diff = mean(explicit_dif, na.rm = T),
            mean_career = mean(assocareer, na.rm = T),
            mean_family = mean(assofamily, na.rm = T),
            mean_iat = mean(overall_iat_D_score, na.rm = T))

# by country correlations
country_means_career_parity_gender_explicit <- subj_means_career_parity  %>%
  group_by(country_name) %>%
  summarize(overall_parity = mean(overall_parity, na.rm = T),
            economic_parity = mean(economic_parity, na.rm = T),
            education_parity = mean(education_parity, na.rm = T),
            health_parity = mean(health_parity, na.rm = T),
            political_parity = mean(political_parity, na.rm = T)) %>%
  right_join(country_means_career_explicit_gender, by = "country_name") 

country_means_career_parity_gender_explicit %>%
  group_by(sex) %>%
  do(tidy(cor.test(.$overall_parity, .$mean_family)))

country_means_career_parity_gender_explicit %>%
  group_by(sex) %>%
  do(tidy(cor.test(.$overall_parity, .$mean_career)))
```

parity and implicit: C: no
parity and explicit: C: maybe
