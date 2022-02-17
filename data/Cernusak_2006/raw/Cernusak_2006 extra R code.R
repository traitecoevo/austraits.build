read_csv("data/Cernusak_2006/raw/leaf_photosynthesis.csv") %>%
  subset(Irradiance == "saturating" & `Leaf type` == "mature") %>%
  select(-Obs,-`initial entry`, -Irradiance, -`Leaf type`) %>%  
  group_by(Tree,species,sampling_period) %>%
  mutate(count_photo = n()) %>%
  summarise_all(.funs=mean, na.rm = TRUE) %>% 
  ungroup() %>%
  select("species","Tree", "count_photo", "sampling_period","Photosynthesis (umol m-2 s-1)", "PARi (umol m-2 s-1)", "Stomatal conductance (mol m-2 s-1)", "Ci (umol mol-1)", 
         "Transpiration (mmol m-2 s-1)") -> photo

read_csv("data/Cernusak_2006/raw/leaf_respiration.csv") %>%
  subset(`Leaf type` %in% c("mature",NA)) %>%
  select(species,Tree,sampling_period, position, `leaf respiraton at 30degC`) %>%
  group_by(species,Tree,sampling_period) %>%
  summarise(position = first(position),
            `leaf respiraton at 30degC` = mean(`leaf respiraton at 30degC`),
            count_leaf_respiration = n()) %>% 
  subset(Tree != "saplings") %>%
  ungroup() -> leaf_respiration
  
read_csv("data/Cernusak_2006/raw/branch_Huber_values.csv") %>%
  rename(Tree = branch) %>%
  mutate(bark_thickness = (diameter - `diameter under bark`)/2) -> huber

read_csv("data/Cernusak_2006/raw/stem_respiration.csv") %>%
  rename(Tree = tree, R25_stem = R25) -> stem_respiration
  
stem_respiration %>%
  full_join(photo,by=c("species","Tree","sampling_period")) %>% 
  full_join(leaf_respiration,by=c("species","Tree","sampling_period")) %>% 
  full_join(huber,by=c("species","Tree","sampling_period","burn_status")) %>%
  mutate(context = paste(burn_status," tree sampled in ",sampling_period,sep="")) %>%
  write_csv("data/Cernusak_2006/data.csv") %>%
  distinct(context, .keep_all = TRUE) %>%
  select(context, burn_status, sampling_period) %>%
  mutate(type = "field", description = "measurements made on burnt and unburned trees at different times in the growing season")-> context
