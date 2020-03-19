
source("R/austraits.R")
library(dplyr)
library(readr)

raw.firn <-read_csv("data/Firn_2019/raw/Firn_australian_sites_NutNet_foliar_data.csv") %>%
  filter(trt.x =="Control")

firn <- read_csv("data/Firn_2019/raw/Firn_australian_sites_NutNet_foliar_data.csv") %>%
  select( species_name = `Taxon`, block, functional_group, trt.x, leaf_pct_N, leaf_pct_C,	leaf_pct_P,	leaf_pct_K,	SLA,	site_name) %>%
  filter(trt.x =="Control") %>% write_csv("data/Firn_2019/data.csv", na = "")
         
#site_code
#site_name, MAT, TEMP_VAR, MAP, MAP_VAR  only used these site variables
#use this if you want to get site variables for each block (contextual field)
data_sites <- 
  raw.firn %>%
  select(site_name, MAT, TEMP_VAR, MAP, MAP_VAR, pct_N,	ppm_P, ppm_K) %>%
  group_by(site_name) %>%
  summarise_all(.funs = mean)


read_csv("data/Firn_2019/raw/Firn_australian_sites_NutNet_foliar_data_updated_SLA.csv") %>%
  mutate(Taxon = str_to_sentence(Taxon)) %>%
  write_csv("data/Firn_2019/data.csv", na = "")


