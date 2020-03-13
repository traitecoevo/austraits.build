read_csv("data/Vesk_2019/raw/sprouts-traits-data.csv") -> traits_data

read_csv("data/Vesk_2019/raw/sprouts-traits-data.csv") %>%
  select(CODE,SPECIES,Family) -> name_match


read_csv("data/Vesk_2019/raw/compiled-sprout-depths.csv") %>%
  gather(observation, bud_depth_mm, -spp, -plant, -coll, -yr, -treat) %>%
  rename(CODE = spp) %>%
  drop_na(bud_depth_mm) %>%
  mutate(observation = as.numeric(gsub("depths_mm_","",observation))) %>%
  left_join(name_match,by="CODE") %>%
  bind_rows(traits_data) %>%
  write_csv("data/Vesk_2019/data.csv")

=min(data_tmp$bud_depth_mm)