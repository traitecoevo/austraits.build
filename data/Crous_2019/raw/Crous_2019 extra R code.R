read_csv("data/Crous_2019/raw/EucFACE_Austraits_NPleaf_2012-2018-L3.csv") %>%
  mutate(context = paste(Age,"_leaves_measured_on_plants_in_",CO2.treat,"_CO2_treatment"),
         site_name = "EucFACE",
         species_name = "Eucalyptus tereticornis") %>%
  write_csv("data/Crous_2019/data.csv") %>%
  distinct(context,.keep_all = TRUE) %>%
  select(context, Age, CO2.treat) %>%
  write_csv("data/Crous_2019/raw/context.csv")
  
read_csv("data/Crous_2019/raw/context.csv") -> context_table


