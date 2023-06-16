
library(tidyverse)

wide_data <- read_csv("data/Kubiak_2009/raw/wide_data.csv")

wide_data %>% 
  pivot_longer(
    c(fire_response_LCR, fire_response_NL, seedlings_first_observed, first_flowering_resp,
      first_flowering_seed, first_fruiting_resp, first_fruiting_seed, primary_juvenile_period,
      secondary_juvenile_period, peak_flowering, peak_fruiting),
    names_to = "trait_name",
    values_to = "value"
  ) -> long_data

long_data %>% write_csv("data/Kubiak_2009/data.csv")
