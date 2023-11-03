Nano <- read_csv("data/Nano_2011/data.csv") %>%
  mutate(
    Taxon2 = Taxon,
    Taxon2 = stringr::str_trim(Taxon2),
    Taxon2 = stringr::str_replace(Taxon2, "  ", " "),
    Taxon3 = Taxon2
  ) %>%
  filter(str_detect(Taxon3, " "))

for (i in seq_along(1:nrow(Nano))) {
  if (stringr::str_detect(Nano$Taxon[[i]], "subsp\\.|var\\.|f\\.\\s|\\sx\\s")) {
    Nano$Taxon3[[i]] <- paste(stringr::word(Nano$Taxon3[[i]], start = 1, end = 2),
                         stringr::str_extract(Nano$Taxon3[[i]], "subsp\\..+|var\\..+|f\\..+|\\sx.+") %>% stringr::word(start = 1, end = 2))
  } else {
    Nano$Taxon3[[i]] <- stringr::word(Nano$Taxon3[[i]], start = 1, end = 2)
  }
}

Nano <- Nano %>%
  mutate(
   Taxon3 = ifelse(stringr::word(Taxon3, 2)=="sp.", Taxon2, Taxon3) 
  )


APCalign::align_taxa(original_name = Nano$Taxon3, identifier = "Nano_2011") -> new_Nano

new_Nano2 <- new_Nano %>%
  select(original_name, aligned_name, reason = aligned_reason, alignment_code, taxonomic_resolution = taxon_rank) %>%
  distinct(original_name, .keep_all = TRUE) %>%
  rename(Taxon3 = original_name)

Nano %>%
  left_join(new_Nano2) %>%
  write_csv("data/Nano_2011/data.csv")

# manually checked and edited

updates_to_add <- read_csv("data/Nano_2011/data.csv") %>%
  filter(!stringr::str_detect(alignment_code, "match_06")) %>%
  select(find = original_name, replace = aligned_name, reason, taxonomic_resolution) %>%
  arrange(find)

metadata_add_taxonomic_changes_list("Nano_2011", updates_to_add)
