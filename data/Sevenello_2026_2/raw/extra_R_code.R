read_csv("data/Sevenello_2026_2/raw/Dataset_mayfield_group.csv") -> Sevenello_2026_2_data

all_values <- austraits$definitions$pollination_vector_possible$allowed_values_levels |> 
  traits.build::convert_list_to_df1() |>
  mutate(synonyms = str_match(value, "Synonyms,\\s*(.*?)\\)")[,2]) 

synonyms <- all_values |>
  separate_longer_delim(synonyms, delim = ", ") |> 
  filter(!is.na(synonyms)) |> 
  pull(synonyms)

trait_values <- all_values |> pull(key)

austraits$definitions$pollination_vector_possible$allowed_values_levels |> 
  traits.build::convert_list_to_df1() |>
  mutate(synonyms = str_match(value, "Synonyms,\\s*(.*?)\\)")[,2]) |>
  filter(!is.na(synonyms)) |>
  separate_longer_delim(synonyms, delim = ", ") |>
  pull(synonyms) -> synonyms
  
Sevenello_2026_2_data |>
  mutate(across(c(6:61), ~ as.character(.x))) |>
  pivot_longer(cols = c(6:61), names_to = "taxon_name", values_to = "value") |>
  mutate(
    value_to_use = case_when(
      value == "0" ~ NA,
      value == "X" ~ Taxa
    )) |>
  filter(!is.na(value_to_use)) |>
  mutate(
    value_to_use2 = case_when(
      str_to_lower(Species) %in% synonyms ~ Species,
      str_to_lower(Species) %in% trait_values ~ Species,
      str_to_lower(Family) %in% synonyms ~ Family,
      str_to_lower(Family) %in% trait_values ~ Family,
      Species == "Apis mellifera" ~ "honeybee", 
      Family == "Halicitidae" ~ "halictidae"
      ) ,
    value_to_use = ifelse(!is.na(value_to_use2), value_to_use2, Taxa)
  ) |>
  write_csv("data/Sevenello_2026_2/data.csv")
  
