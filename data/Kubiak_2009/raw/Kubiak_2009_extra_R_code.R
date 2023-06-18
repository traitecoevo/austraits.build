
library(tidyverse)

wide_data <- read_csv("data/Kubiak_2009/raw/wide_data.csv")

# Find values with 'y' to add substitutions
#unique(wide_data$first_flowering_resp)[wide_data$first_flowering_resp %>% unique() %>% str_detect("y")]

wide_data %>% 
  mutate(
    seedlings_first_observed = str_extract(seedlings_first_observed, "[0-9\\-]+(?=w)"),
    first_flowering_resp = if_else(
      str_detect(first_flowering_resp, "y"),
      str_extract(first_flowering_resp, "[0-9][0-9\\-\\.]*(?=y)"),
      str_extract(first_flowering_resp, "[0-9\\-]+(?=w)")    
    ),
    first_flowering_seed = if_else(
      str_detect(first_flowering_seed, "y"),
      str_extract(first_flowering_seed, "[0-9][0-9\\-\\.]*(?=y)"),
      str_extract(first_flowering_seed, "[0-9\\-]+(?=w)")    
    ),
    first_flowering_unknown = if_else(
      str_detect(first_flowering_unknown, "y"),
      str_extract(first_flowering_unknown, "[0-9][0-9\\-\\.]*(?=y)"),
      str_extract(first_flowering_unknown, "[0-9\\-]+(?=w)")    
    ),
    first_fruiting_resp = if_else(
      str_detect(first_fruiting_resp, "y"),
      str_extract(first_fruiting_resp, "[0-9][0-9\\-\\.]*(?=y)"),
      str_extract(first_fruiting_resp, "[0-9\\-]+(?=w)")    
    ),
    first_fruiting_seed = if_else(
      str_detect(first_fruiting_seed, "y"),
      str_extract(first_fruiting_seed, "[0-9][0-9\\-\\.]*(?=y)"),
      str_extract(first_fruiting_seed, "[0-9\\-]+(?=w)")    
    ),
    first_fruiting_unknown = if_else(
      str_detect(first_fruiting_unknown, "y"),
      str_extract(first_fruiting_unknown, "[0-9][0-9\\-\\.]*(?=y)"),
      str_extract(first_fruiting_unknown, "[0-9\\-]+(?=w)")    
    ),
    primary_juvenile_period = if_else(
      str_detect(primary_juvenile_period, "y"),
      str_extract(primary_juvenile_period, "[0-9][0-9\\-\\.]*(?=y)"),
      str_extract(primary_juvenile_period, "[0-9\\-]+(?=w)")    
    ),
    secondary_juvenile_period = case_when(
      str_detect(secondary_juvenile_period, "\\+") ~ str_extract(secondary_juvenile_period, "[0-9\\-]+(?=\\+)"),
      str_detect(secondary_juvenile_period, "y") ~ str_extract(secondary_juvenile_period, "[0-9\\-][0-9\\-\\.]*(?=y)"),
      TRUE ~ str_extract(secondary_juvenile_period, "[0-9\\-]+(?=w)")
    ),
    peak_flowering_resp = if_else(
      str_detect(peak_flowering_resp, "\\+"),
      str_extract(peak_flowering_resp, "[0-9\\-]+(?=\\+)"),
      str_extract(peak_flowering_resp, "[0-9][0-9\\-\\.]*(?=y)")    
    ),
    peak_flowering_seed = if_else(
      str_detect(peak_flowering_seed, "\\+"),
      str_extract(peak_flowering_seed, "[0-9\\-]+(?=\\+)"),
      str_extract(peak_flowering_seed, "[0-9][0-9\\-\\.]*(?=y)")    
    ),
    peak_flowering_unknown = if_else(
      str_detect(peak_flowering_unknown, "\\+"),
      str_extract(peak_flowering_unknown, "[0-9\\-]+(?=\\+)"),
      str_extract(peak_flowering_unknown, "[0-9][0-9\\-\\.]*(?=y)")    
    ),
    peak_fruiting_resp = if_else(
      str_detect(peak_fruiting_resp, "\\+"),
      str_extract(peak_fruiting_resp, "[0-9\\-]+(?=\\+)"),
      str_extract(peak_fruiting_resp, "[0-9][0-9\\-\\.]*(?=y)")    
    ),
    peak_fruiting_seed = if_else(
      str_detect(peak_fruiting_seed, "\\+"),
      str_extract(peak_fruiting_seed, "[0-9\\-]+(?=\\+)"),
      str_extract(peak_fruiting_seed, "[0-9][0-9\\-\\.]*(?=y)")    
    ),
    peak_fruiting_unknown = if_else(
      str_detect(peak_fruiting_unknown, "\\+"),
      str_extract(peak_fruiting_unknown, "[0-9\\-]+(?=\\+)"),
      str_extract(peak_fruiting_unknown, "[0-9][0-9\\-\\.]*(?=y)")    
    )
  ) -> wide_data

wide_data %>% 
  pivot_longer(
    c(fire_response_LCR, fire_response_NL, seedlings_first_observed, first_flowering_resp,
      first_flowering_seed, first_flowering_unknown, first_fruiting_resp, first_fruiting_seed, 
      first_fruiting_unknown, primary_juvenile_period, secondary_juvenile_period, 
      peak_flowering_resp, peak_flowering_seed, peak_flowering_unknown, peak_fruiting_resp, 
      peak_fruiting_seed, peak_fruiting_unknown),
    names_to = "trait_name",
    values_to = "value"
  ) %>% 
  drop_na(value) -> long_data

long_data %>% write_csv("data/Kubiak_2009/data.csv")
