read_csv("data/Stephens_2023/raw/data_raw.csv") -> data_original

read_csv("data/Stephens_2023/raw/taxa_to_keep.csv") -> taxon_list

data_original |>
  filter(taxon_name %in% taxon_list$canonicalName | taxon_name == "Hibbertia scandens") |>
  write_csv("data/Stephens_2023/data.csv") |> View()
