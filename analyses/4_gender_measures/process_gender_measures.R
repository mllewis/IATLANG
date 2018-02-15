# pre-process gender measures
library(tidyverse)
library(wbstats)
library(countrycode)
# README: https://docs.google.com/document/d/1dQUGOP-7t_7BvdwQIN5BlgoSfH6h-X8rIBdxsVt8FBM/edit

### READ IN ALL MEASURES
gdi <- read_csv("data/gender_measures/Gender\ Development\ Index (GDI).csv", skip = 1) %>%
  select(Country, `2015`) %>%
  rename(country_gdi = Country,
         gdi = `2015`) %>%
  mutate(country_code = countrycode(country_gdi, "country.name", "iso2c"))
  

gii <- read_csv("data/gender_measures/Gender\ Inequality\ Index (GII).csv", skip = 1) %>%
  select(Country, `2015`) %>%
  rename(country_gii = Country,
         gii = `2015`) %>%
  mutate(country_code = countrycode(country_gii, "country.name", "iso2c"))

ggi <- read_csv("data/gender_measures/WEF_GGGR_Dataset_2016_cleaned.csv", skip = 1) %>%
  select(-contains("Rank")) %>%
  rename(country_ggi = Country,
         ggi = overall_score,
         ggi_econonomic_participation_score = econonomic_participation_score,
         ggi_educational_attainment_score_ggi = educational_attainment_score,
         ggi_health_survival_score= health_survival_score,
         ggi_political_empowerment_score = political_empowerment_score) %>%
  mutate(country_code = countrycode(country_ggi, "country.name", "iso2c"))

sigi <- read_csv("data/gender_measures/SIGI.csv") %>%
  select(2,4:7) %>%
  set_names("country_sigi", "sigi", "sigi_fam", "sigi_physical", "sigi_son") %>%
  mutate(country_code = countrycode(country_sigi, "country.name", "iso2c"))

wps <- read_csv("data/gender_measures/WPS_index.csv") %>%
  rename(country_wps = country_name,
         wps = wps_index) %>%
  mutate(country_code = countrycode(country_wps, "country.name", "iso2c"))

# use worldbank package to get GPI
#gender_indicators <- wbsearch(pattern = "gender", 
#                              field = "indicator") %>%
  
#wb_gender <- wb(indicator = gender_indicators$indicatorID, 
#     return_wide = TRUE,
#     mrv = 1,
#     gapfill = TRUE)
#write_csv(wb_gender, "gender_measures/GPI_from_wbstats.csv")
  
gpi <- read_csv("data/gender_measures/GPI_from_wbstats.csv") %>%
  select(c(-1:-3, -5)) %>%
  replace(is.na(.), 0) %>%
  mutate(blank_row = rowSums(.[2:9])) %>% 
  filter(blank_row  > 0) %>%
  select(-blank_row) %>%
  rename(country_gpi = country,
         #wb_education = `5.51.01.07.gender`,
         wb_cpia = IQ.CPA.GNDR.XQ,
         gpi_schooling_gap_15 = PRJ.MYS.15UP.GPI, #Projection: Mean Years of Schooling. Age 15+. Gender Gap
         gpi_schooling_gap_25 = PRJ.MYS.25UP.GPI,
         gpi_literacy = SE.ADT.1524.LT.FM.ZS, # Literacy rate, youth (ages 15-24), gender parity index (GPI)
         gpi_prim_school = SE.ENR.PRIM.FM.ZS, #chool enrollment, primary (gross), gender parity index (GPI)
         gpi_prim_sec_school = SE.ENR.PRSC.FM.ZS,
         gpi_sec_school = SE.ENR.SECO.FM.ZS,
         gpi_tert_school = SE.ENR.TERT.FM.ZS)  %>%
  mutate(country_code = countrycode(country_gpi, "country.name", "iso2c")) %>%
  select(-gpi_schooling_gap_15, -gpi_schooling_gap_25)

#### MERGE TOGETHER

country_to_langs <- read_csv("/Users/mollylewis/Documents/research/Projects/IATLANG/data/other/country_langiso_langwiki_key.csv") %>%
  select(countryres, country_name) %>%
  rename(country_code = countryres)

all <- list(country_to_langs, gdi, gii, ggi, sigi, wps) %>%
        reduce(left_join, by = "country_code")  %>%
  rename(countryname = country_name,
         countrycode = country_code) %>%
  select(-contains("country_"))  %>%
  mutate_at(3:14, as.numeric) %>%
  mutate_at(1:2, as.factor) %>%
  rename(country_name = countryname,
         country_code = countrycode)

write_csv(all, "data/gender_measures/all_gender_measures2.csv")



