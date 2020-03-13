read_csv("data/Vesk_2004/raw/Vesk_2004_sites.csv") -> Vesk_sites

read_csv("data/Vesk_2004/raw/Vesk_2004_invidiual_level_data.csv") -> individual_data 
read_csv("data/Vesk_2004/raw/Vesk_2004_species_level_data.csv") -> species_data 
read_csv("data/Vesk_2004/data_old.csv") %>%
  select(`name_original`,`sprouting_w/i/s`) %>%
  rename(`species`=`name_original`,`fire_response_detailed` = `sprouting_w/i/s`) %>%
  full_join(species_data,by="species") -> species_data

individual_data %>%
  bind_rows(species_data) %>%
  write_csv("data/Vesk_2004/data.csv")