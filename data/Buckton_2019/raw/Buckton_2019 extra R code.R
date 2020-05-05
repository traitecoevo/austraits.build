my_mean <- function(x) {mean(x, na.rm=TRUE)}
summarise_all(my_mean)


read_csv("data/Buckton_2019/raw/LICOR_ForR.csv") %>%
  select(Species,Tree.code,Ci,WUE) %>%
  group_by(Species,Tree.code) %>%
  summarise_all(my_mean) %>%
  ungroup() %>%
  rename(Tree = Tree.code) -> extra_gas_exchange

read_csv("data/Buckton_2019/raw/LeafData.csv") %>%
  select(-Time,-ID,-Leaf,-LeafCode,-Lam1,-Lam2,-Lam3,-Spec.Code.old,-LMA,-Individual,-Growth.Form) %>%
  group_by(Species,Tree,Date) %>%
  summarise_all(my_mean) %>%
  ungroup() %>%
  rename(date_leaf = Date) -> leaf_data

read_csv("data/Buckton_2019/raw/StemData.csv") %>%
  select(-Growth.Form,-Species.Code.OLD,-Individual,-Length,-Diameter,-Fresh.Weight,-Vol.measured,-Vol.waterDisp, -Dry.Weight,-Species.Old) %>%
  group_by(Species, BranchCode) %>%
  summarise_all(my_mean) %>%
  ungroup() %>%
  rename(Tree = BranchCode) -> stem_data

read_csv("data/Buckton_2019/raw/Stem_water_18O.csv") %>%
  select(-Identifier,-Day,-Growth.Form) %>%
  rename(Tree = BranchCode, Date_d18O = Date) -> stem_water

read_csv("data/Buckton_2019/raw/TreeSampleList.csv") %>%
  rename(Species_name = Species,Species = `Species Code`) %>%
  select(-X9, -`Growth Form`,-Species_name) %>%
  mutate(Tree = paste0(Species,"_",Individual)) -> tree_names

read_csv("data/Buckton_2019/raw/StemData.csv") %>% 
  rename(Species_name = Species.Old) %>%
  select(Species,Species_name, Growth.Form) %>%
  distinct(Species, .keep_all = TRUE) %>%
  add_row(Species = "ACM",Species_name= "Acmena graveolens",Growth.Form ="T") %>%
  add_row(Species = "SYZ",Species_name= "Acmena graveolens",Growth.Form ="T") -> species_names

read_csv("data/Buckton_2019/raw/Photo_Combined.csv") %>%
  select(-LMA,-LDMC,-LS,-Lamina,`Leaf N`,-Growth.Form) %>%
  left_join(extra_gas_exchange,by=c("Species","Tree")) %>% 
  full_join(leaf_data,by=c("Species","Tree")) %>%
  full_join(stem_data,by=c("Species","Tree")) %>%
  full_join(stem_water,by=c("Species","Tree")) %>%
  full_join(tree_names,by=c("Species","Tree")) %>%
  left_join(species_names,by=c("Species")) %>%
  group_by(Species) %>%
  mutate_at(vars(`Growth.Form`),funs(replace(.,duplicated(.),NA))) %>%
  ungroup() %>%
  write_csv("data/Buckton_2019/data.csv") %>%
View()

