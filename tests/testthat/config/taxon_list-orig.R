
# to generate taxon_list-orig.csv
v1 <- read_csv("config/taxon_list.csv") 
v2 <- read_csv("tests//testthat/config/taxon_list-orig.csv") 
v1 %>% filter(taxon_id %in% v2$taxon_id, taxonomic_status=="accepted") %>% write_csv("tests/testthat/config/taxon_list-orig.csv") 
