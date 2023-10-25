read_csv("data/Ong_2018/raw/Collections.csv") %>%
  mutate(
    treeid = treeid %>% as.numeric()
    ) -> collections

collections$treeid <- paste0("treeid_", sprintf("%003d", collections$treeid))

read_csv("data/Ong_2018/raw/leaf_data.csv") -> leaf_data

read_csv("data/Ong_2018/raw/species_lookup.csv") -> species

leaf_data %>% rowwise() %>%
  mutate(
    across(c(10:33), as.numeric),
    wet_major_vein_transmitted_mean_frequency = mean(c(w.mavc1, w.mavc2, w.mavc3, w.mavc4), na.rm=TRUE)/8, # divide by 8 to convert counts to frequency, based on methods
    wet_minor_vein_transmitted_mean_frequency = mean(c(w.mivc1, w.mivc2, w.mivc3, w.mivc4), na.rm=TRUE)/8,
    dry_major_vein_transmitted_mean_frequency = mean(c(d.mavc1, d.mavc2, d.mavc3, d.mavc4), na.rm=TRUE)/8,
    dry_minor_vein_transmitted_mean_frequency = mean(c(d.mivc1, d.mivc2, d.mivc3, d.mivc4), na.rm=TRUE)/8,
    dry_major_vein_reflected_mean_frequency = mean(c(d.mavc1r, d.mavc2r, d.mavc3r, d.mavc4r), na.rm=TRUE)/8,
    dry_minor_vein_reflected_mean_frequency = mean(c(d.mivc1r, d.mivc2r, d.mivc3r, d.mivc4r), na.rm=TRUE)/8,
    treeid = as.character(treeid),
    across(c("wet_major_vein_transmitted_mean_frequency", "wet_minor_vein_transmitted_mean_frequency",
             "dry_major_vein_transmitted_mean_frequency", "dry_minor_vein_transmitted_mean_frequency", 
             "dry_major_vein_reflected_mean_frequency", "dry_minor_vein_reflected_mean_frequency"), ~na_if(.x, NaN))
  ) %>%
  select(-all_of(c(10:34))) %>%
  mutate(
    wetarea_replicates = ifelse(!is.na(wetarea), 1, 0),
    dryarea_replicates = ifelse(!is.na(dryarea), 1, 0), 
    drymass_replicates = ifelse(!is.na(drymass), 1, 0),
    wet_major_vein_transmitted_mean_frequency_replicates = ifelse(!is.na(wet_major_vein_transmitted_mean_frequency), 1, 0),
    wet_minor_vein_transmitted_mean_frequency_replicates = ifelse(!is.na(wet_minor_vein_transmitted_mean_frequency), 1, 0),
    dry_major_vein_transmitted_mean_frequency_replicates = ifelse(!is.na(dry_major_vein_transmitted_mean_frequency), 1, 0),
    dry_minor_vein_transmitted_mean_frequency_replicates = ifelse(!is.na(dry_minor_vein_transmitted_mean_frequency), 1, 0),
    dry_major_vein_reflected_mean_frequency_replicates = ifelse(!is.na(dry_major_vein_reflected_mean_frequency), 1, 0),
    dry_minor_vein_reflected_mean_frequency_replicates = ifelse(!is.na(dry_minor_vein_reflected_mean_frequency), 1, 0)
    ) %>%
  group_by(treeid, species,	leaftype,	sampletype,	days) %>%
    mutate(
      across(c("wetarea_replicates", "dryarea_replicates", "drymass_replicates", "wet_major_vein_transmitted_mean_frequency_replicates", 
               "wet_minor_vein_transmitted_mean_frequency_replicates",
               "dry_major_vein_transmitted_mean_frequency_replicates", "dry_minor_vein_transmitted_mean_frequency_replicates", 
               "dry_major_vein_reflected_mean_frequency_replicates", "dry_minor_vein_reflected_mean_frequency_replicates"), ~sum(.x)),
      across(c("wetarea", "dryarea", "drymass", "wet_major_vein_transmitted_mean_frequency", "wet_minor_vein_transmitted_mean_frequency",
                    "dry_major_vein_transmitted_mean_frequency", "dry_minor_vein_transmitted_mean_frequency", 
                    "dry_major_vein_reflected_mean_frequency", "dry_minor_vein_reflected_mean_frequency"), ~mean(.x, na.rm = TRUE))
      ) %>% 
  ungroup() %>%
  select(-all_of("leafid")) %>%
  distinct() %>%
  mutate(
    across(c("wetarea", "dryarea", "drymass", "wet_major_vein_transmitted_mean_frequency", "wet_minor_vein_transmitted_mean_frequency",
                  "dry_major_vein_transmitted_mean_frequency", "dry_minor_vein_transmitted_mean_frequency", 
                  "dry_major_vein_reflected_mean_frequency", "dry_minor_vein_reflected_mean_frequency"), ~na_if(.x, NaN)),
    across(c("wetarea", "dryarea", "drymass", "wet_major_vein_transmitted_mean_frequency", "wet_minor_vein_transmitted_mean_frequency",
             "dry_major_vein_transmitted_mean_frequency", "dry_minor_vein_transmitted_mean_frequency", 
             "dry_major_vein_reflected_mean_frequency", "dry_minor_vein_reflected_mean_frequency", "wetarea_replicates", "dryarea_replicates", 
             "drymass_replicates", "wet_major_vein_transmitted_mean_frequency_replicates", 
             "wet_minor_vein_transmitted_mean_frequency_replicates",
             "dry_major_vein_transmitted_mean_frequency_replicates", "dry_minor_vein_transmitted_mean_frequency_replicates", 
             "dry_major_vein_reflected_mean_frequency_replicates", "dry_minor_vein_reflected_mean_frequency_replicates"), ~na_if(.x, 0))
    ) %>%
  rename(abbreviation = species) %>%
  left_join(species, by = c("abbreviation")) %>%
  mutate(treeid = paste0("treeid_", treeid %>% format(000))) %>%
  left_join(collections, by = c("treeid", "Species")) %>%
  write_csv("data/Ong_2018/data.csv")

  
