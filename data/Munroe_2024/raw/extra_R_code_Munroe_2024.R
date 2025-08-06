raw_data <- read_csv("data/Munroe_2024/raw/Photosynthetic_Pathways_of_Plants_TERN_v2_19092024.csv") %>%
  mutate(species_name = str_replace_all(speciesName, "_", " ")) %>%
  filter(!is.na(species_name))

names_new_aligned <- APCalign::create_taxonomic_update_lookup(raw_data$species_name) #%>% select(original_name, suggested_name, accepted_name)

names_new_aligned2 <- names_new_aligned %>%
  mutate(taxon_name = ifelse(taxonomic_status == "unplaced", suggested_name, accepted_name)) %>%
  filter(!is.na(taxon_name)) %>%
  select(species_name = original_name, taxon_name) %>%
  distinct(species_name, .keep_all = TRUE)
  
old_data <- (austraits %>% extract_dataset("Munroe_2019"))$traits %>% select(taxon_name, old_pathway = value) %>% distinct()

raw_data_edited <- raw_data %>%
  left_join(names_new_aligned2, by = c("species_name")) %>%
  full_join(old_data, by = c("taxon_name")) %>%
  mutate(photosyntheticPathway_combined2 = ifelse(!stringr::str_to_lower(photosyntheticPathway_combined) == old_pathway | is.na(old_pathway), photosyntheticPathway_combined, NA)) %>%
  #select(speciesName, photosyntheticPathway_combined, source, species_name, taxon_name, old_pathway, photosyntheticPathway_combined2) %>%
  write_csv("data/Munroe_2024/data.csv")

