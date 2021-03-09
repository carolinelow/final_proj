library(shiny)
library(tidyverse)
library(plotly)
library(countrycode)
library(knitr)
library(stringr)
library(rsconnect)
library(maps)
library(dplyr)
mental_health <- read.csv("nmhss-puf-2018.csv")
state_populations <- read.csv("state.csv")
mental_health_facilities <- full_join(mental_health, state_populations, by = "LST")
state_shapes <- map_data("state") %>%
  mutate(name = toupper(region))
abbreviations <- data.frame(name = state.name, abb= state.abb) %>%
  mutate(name = toupper(name))
state_map <- left_join(state_shapes, abbreviations, by = "name") %>%
  mutate(LST = abb)
psych <- mental_health_facilities %>%
  select(LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 1)
agg_psych <- aggregate(psych['FACILITYTYPE'], by=psych['LST'], sum)
state_psych <- left_join(state_map, agg_psych, by = "LST") %>%
  mutate(number = FACILITYTYPE)
total <- mental_health_facilities %>%
  select(LST, MHINTAKE) %>%
  filter(MHINTAKE == 1)
agg_total <- aggregate(total['MHINTAKE'], by=total['LST'], sum)
state_total <- left_join(state_map, agg_total, by = "LST") %>%
  mutate(number = MHINTAKE)
separate <- mental_health_facilities %>%
  select(LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 2)
agg_separate <- aggregate(separate['FACILITYTYPE'], by=separate['LST'], sum)
state_separate <- left_join(state_map, agg_separate, by = "LST") %>%
  mutate(number = FACILITYTYPE)
res_child <- mental_health_facilities %>%
  select(LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 3)
agg_res_child <- aggregate(res_child['FACILITYTYPE'], by=res_child['LST'], sum)
state_res_child <- left_join(state_map, agg_res_child, by = "LST") %>%
  mutate(number = FACILITYTYPE)
res_adult <- mental_health_facilities %>%
  select(LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 4)
agg_res_adult <- aggregate(res_adult['FACILITYTYPE'], by=res_adult['LST'], sum)
state_res_adult <- left_join(state_map, agg_res_adult, by = "LST") %>%
  mutate(number = FACILITYTYPE)
other_res <- mental_health_facilities %>%
  select(LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 5)
agg_other_res <- aggregate(other_res['FACILITYTYPE'], by=other_res['LST'], sum)
state_other_res <- left_join(state_map, agg_other_res, by = "LST") %>%
  mutate(number = FACILITYTYPE)
vet <- mental_health_facilities %>%
  select(LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 6)
agg_vet <- aggregate(vet['FACILITYTYPE'], by=vet['LST'], sum)
state_vet <- left_join(state_map, agg_vet, by = "LST") %>%
  mutate(number = FACILITYTYPE)
com <- mental_health_facilities %>%
  select(LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 7)
agg_com <- aggregate(com['FACILITYTYPE'], by=com['LST'], sum)
state_com <- left_join(state_map, agg_com, by = "LST") %>%
  mutate(number = FACILITYTYPE)
partial <- mental_health_facilities %>%
  select(LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 8)
agg_partial <- aggregate(partial['FACILITYTYPE'], by=partial['LST'], sum)
state_partial <- left_join(state_map, agg_partial, by = "LST") %>%
  mutate(number = FACILITYTYPE)
outpatient <- mental_health_facilities %>%
  select(LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 9)
agg_outpatient <- aggregate(outpatient['FACILITYTYPE'], by=outpatient['LST'], sum)
state_outpatient <- left_join(state_map, agg_outpatient, by = "LST") %>%
  mutate(number = FACILITYTYPE)
multi <- mental_health_facilities %>%
  select(LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 10)
agg_multi <- aggregate(multi['FACILITYTYPE'], by=multi['LST'], sum)
state_multi <- left_join(state_map, agg_multi, by = "LST") %>%
  mutate(number = FACILITYTYPE)
other <- mental_health_facilities %>%
  select(LST, FACILITYTYPE) %>%
  filter(FACILITYTYPE == 11)
agg_other <- aggregate(other['FACILITYTYPE'], by=other['LST'], sum)
state_other <- left_join(state_map, agg_other, by = "LST") %>%
  mutate(number = FACILITYTYPE)
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )
#states
al <- mental_health_facilities %>%
  filter(LST == "AL") %>%
  select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
  gather() %>%
  filter(value == 1)
ak <- mental_health_facilities %>%
  filter(LST == "AK") %>%
  select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
  gather() %>%
  filter(value == 1)
az <- mental_health_facilities %>%
  filter(LST == "AZ") %>%
  select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
  gather() %>%
  filter(value == 1)
ar <- mental_health_facilities %>%
  filter(LST == "AR") %>%
  select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
  gather() %>%
  filter(value == 1)
ca <- mental_health_facilities %>%
  filter(LST == "CA") %>%
  select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
  gather() %>%
  filter(value == 1)
co <- mental_health_facilities %>%
  filter(LST == "CO") %>%
  select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
  gather() %>%
  filter(value == 1)
