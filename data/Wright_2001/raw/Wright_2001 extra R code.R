#text to rejoin two parts of Ian's unpublished seed mass and seed reserve mass data
#doing this, because the data in tables in the Westoby_2003 citation are not the data 
#that were in AusTraits under Westoby_2003

read_csv("data/Westoby_2003/data.csv") -> old_Westoby_2003_data

read_csv("data/Wright_2001/data.csv") %>%
  bind_rows(old_Westoby_2003_data) %>% 
  write_csv("data/Wright_2001/data.csv") 

#but in the end I started from scratch with Ian Wright's file, to regain the seed provenance
#site and sample size information
