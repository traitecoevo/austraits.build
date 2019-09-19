read_csv("data/Lim_2017/raw/Lim_Bogong_leafTraits_LeafArea_Dryweight.csv") %>%
  mutate(Site = Site %>% str_replace("FBPN","BPN")) %>%
  group_by(Date, Site, Position, Spp, Branchno, `Date dryweight`) %>%
  summarise(Area_sum = sum(Area),leaf_count = max(IndividualLeafnumber)) -> leaf_area_sum

read_csv("data/Lim_2017/raw/Lim_Bogong_leafTraits_LeafArea_Dryweight.csv") %>%
  filter(!is.na(DryweightGroup)) %>%
  select(Site,Position,Spp,Branchno,DryweightGroup) %>%
  left_join(leaf_area_sum,by = (c("Site", "Position", "Spp", "Branchno"))) %>%
  mutate(Dry_weight = DryweightGroup/leaf_count) %>%
  mutate(Area_mean = Area_sum/leaf_count) -> leaf_area_sum2

write_csv(leaf_area_sum2,"data/Lim_2017/raw/test.csv")
library("rqdatatable")
install.packages("rqdatatable")
Lim_check <- 
read_csv("data/Lim_2017/data.csv") %>%
  mutate(Spp = ifelse(Spp == "Kunzea muelleri","KuMue",Spp),
         Site = ifelse(Site == "F04North","F04N",Site),
         Site = ifelse(Site == "BPN","BPN",Site)) %>%
  natural_join(leaf_area_sum2, by = (c("Site", "Position", "Spp", "Branchno")), jointype = "FULL") %>%
  mutate(Species = ifelse(Spp == "KuMue","Kunzea muelleri",Species),
         Species = ifelse(Spp == "BaGun","Baeckea gunniana",Species),
         Species = ifelse(Spp == "OlFro","Olearia frostii",Species)) -> data_tmp

write_csv(data_tmp,"data/Lim_2017/data.csv")

austraits <- remake::make("austraits")
build_study_report("Lim_2017",overwrite = TRUE)

