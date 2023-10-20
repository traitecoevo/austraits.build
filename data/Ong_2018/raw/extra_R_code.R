read_csv("data/Ong_2018/raw/Collections.csv") %>% mutate(treeid = as.character(treeid)) -> collections

read_csv("data/Ong_2018/raw/leaf_data.csv") -> leaf_data

read_csv("data/Ong_2018/raw/species_lookup.csv") -> species

leaf_data %>%
  mutate(
    across(c(10:33), as.numeric),
    wet_major_vein_transmitted_mean_frequency = (sum(`w.mavc1`, `w.mavc2`, `w.mavc3`, `w.mavc4`)/4)/16, # divide by 16 to convert counts to frequency
    wet_minor_vein_transmitted_mean_frequency = ((`w.mivc1` + `w.mivc2` + `w.mivc3` + `w.mivc4`)/4)/16,
    dry_major_vein_transmitted_mean_frequency = ((`d.mavc1` + `d.mavc2` + `d.mavc3` + `d.mavc4`)/4)/16,
    dry_minor_vein_transmitted_mean_frequency = ((`d.mivc1` + `d.mivc2` + `d.mivc3` + `d.mivc4`)/4)/16,
    dry_major_vein_reflected_mean_frequency = ((`d.mavc1r` + `d.mavc2r` + `d.mavc3r` + `d.mavc4r`)/4)/16,
    dry_minor_vein_reflected_mean_frequency = ((`d.mivc1r` + `d.mivc2r` + `d.mivc3r` + `d.mivc4r`)/4)/16,
    treeid = as.character(treeid)
  ) %>%
  select(-all_of(c(10:33))) %>%
  group_by(treeid, species,	leaftype,	sampletype,	days) %>%
    mutate(across(c("wetarea", "dryarea", "drymass", "wet_major_vein_transmitted_mean_frequency", "wet_minor_vein_transmitted_mean_frequency",
                    "dry_major_vein_transmitted_mean_frequency", "dry_minor_vein_transmitted_mean_frequency", 
                    "dry_major_vein_reflected_mean_frequency", "dry_minor_vein_reflected_mean_frequency"), ~mean(.x, na.rm = TRUE))) %>%
  # XXX add in replicates
  # XXX na.omit not working
  ungroup() %>%
  select(-all_of("leafid")) %>%
  distinct() %>%
  rename(abbreviation = species) %>%
  left_join(species, by = c("abbreviation")) %>% 
  left_join(collections, by = c("Species", "treeid")) %>% View()
  write_csv("data/Ong_2018/data.csv")

  