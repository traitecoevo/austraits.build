Lim_1 <- read_csv("data/Lim_2017/raw/Lim_Bogong_leafTraits_LeafArea_Dryweight.csv")
data <- read_csv("data/Lim_2017/raw/data_old.csv")

Lim_1 %>% 
  group_by(Spp,Site,Position,Branchno) %>% 
  tally() -> Lim_2
Lim_1 %>% 
  group_by(Spp,Site,Position,Branchno) %>% 
  summarise(
    Area_mean = mean(Area)) -> Lim_3

Lim_4 <- read_csv("data/Lim_2017/raw/leaf_area.csv")

Lim_merge <- dplyr::left_join(Lim_4, Lim_2, by = c("Spp","Site","Position","Branchno"))
Lim_merge <- dplyr::left_join(Lim_merge, Lim_3, by = c("Spp","Site","Position","Branchno"))
Lim_merge %>% mutate(
  Dry_weight = DryweightGroup/n) -> Lim_merge
data <- dplyr::left_join(data, Lim_merge, by = c("Spp","Site","Position","Branchno"))
write_csv(data,"data/Lim_2017/data.csv")
