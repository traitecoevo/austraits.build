read_csv("data/Geange_2020/raw/Geange_2020_ice.csv") %>%
  select(-Ordered_Tag,-Ibutton_ID) %>%
  rename(Tag_Num = Tag_ID) %>%
  mutate(SubPlot = paste0(Block,Plot,Treatment)) -> ice_nucleation

read_csv("data/Geange_2020/raw/Geange_2020_traits.csv") %>%
  full_join(ice_nucleation) %>% 
  rename(provenance = Site) %>%
  mutate(species = "Aciphylla glacialis",
         site = "Merritts Creek, Kosciuszko NP",
         Treatment = ifelse(Treatment == "A","Ambient","Warmed"),
         provenance = gsub("kosi","Kosciuszko Summit",provenance),
         provenance = gsub("pass","Charlotte Pass",provenance),
         provenance = gsub("hut","Seamans Hut",provenance),
         provenance = gsub("river","Snowy River",provenance),
         context = paste0("seeds from ",provenance," grown under ",Treatment," conditions")
         ) %>%
  write_csv("data/Geange_2020/data.csv") -> data_to_use


  data_to_use %>%
    distinct(context, .keep_all = TRUE) %>%
    select(context, provenance, Treatment) %>%
    write_csv("data/Geange_2020/raw/contexts.csv")

  read_csv("data/Geange_2020/raw/contexts.csv") -> contexts
  