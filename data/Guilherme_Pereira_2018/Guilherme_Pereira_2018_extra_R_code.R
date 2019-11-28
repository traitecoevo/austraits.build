read_csv("data/Guilherme_Pereira_2018/raw/Leaf Trait Data (Guilherme Pereira et al 2018).csv") %>%
  mutate(Species = gsub("A. littorea","Anthocercis littorea",Species),
         Species = gsub("A. rostellifera","Acacia rostellifera",Species),
         Species = gsub("L. cassioides","Labichea cassioides",Species),
         Species = gsub("M. insulare","Myoporum insulare",Species),
         Species = gsub("M. systena","Melaleuca systena",Species),
         Species = gsub("O. axillaris","Olearia axillaris",Species),
         Species = gsub("S. globulosum","Spyridium globulosum",Species),
         Species = gsub("T. retusa","Templetonia retusa",Species)) %>%
  write_csv("data/Guilherme_Pereira_2018/data.csv") -> data_tmp

read_csv("data/Guilherme_Pereira_2018/site_data.csv") -> site_data
