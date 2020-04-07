read_csv("data/Nicholson_2017/raw/data_raw.csv") %>% 
  mutate(site_name = "Clarke Island") %>% 
  mutate(dead = if_else(is.na(dead),0,dead)) %>% 
  mutate(resprouting = if_else(is.na(resprouting),0,resprouting)) %>% 
  mutate(fire_response = if_else(dead==(resprouting+dead),"fire_killed","resprouts")) %>% 
  mutate(resprouting_proportion = resprouting/(resprouting+dead)) %>% 
  write_csv("data/Nicholson_2017/data.csv")

#data.csv file then manually manipulated to add in data from the file 
#"Species by severity_Nicholson et al 2017.xls" in the raw data folder.