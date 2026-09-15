# get data
local_data <- leaf13C::get_data()

# filter to Australia
local_data |> filter(latitude < -10.4, latitude > -44, longitude > 113, longitude < 154) -> Australia_d13C_Cornwell

Australia_d13C_Cornwell |> write_csv("data/Cornwell_2017/raw/Australia_d13C_Cornwell.csv")

Australia_d13C_Cornwell |> 
  distinct(author, year) |>
  arrange(author, year)

# author year
# 1  Burgess_Dawson 2007
# 2   Cernusak_etal 2002
# 3   Cernusak_etal 2004 - ADDED TO AUSTRAITS
# 4   Cernusak_etal 2005
# 5     Korner_etal 1988
# 6       Read_etal 1985
# 7   Schmidt et al   NA - IN AUSTRAITS
# 8    Schulze_etal 1998 - IN AUSTRAITS
# 9    Schulze_etal 2006 - IN AUSTRAITS
# 10   Stewart_etal 1995 - IN AUSTRAITS
# 11 Tennakoon_etal 1997 - ADDED TO AUSTRAITS
# 12          unpub   NA
# 13           <NA>   NA

# likely duplicates
Australia_d13C_Cornwell |>
  filter(Accepted_name_rank != "genus") |>
  filter(str_detect(author, "Cernusak") |str_detect(author, "Schulze") |str_detect(author, "Stewart") |str_detect(author, "Schmidt")) |>
  filter(!species %in% taxa_in_AusTraits$taxon_name) |>
  arrange(author, year) |> View()


# remove clear duplicates
Australia_d13C_Cornwell |>
  filter(Accepted_name_rank != "genus") |>
  # all data in AusTraits for these studies
  filter(!(str_detect(author, "Schulze_etal") & year == 2006)) |> 
  filter(!(str_detect(author, "Schulze_etal") & year == 1998)) |> 
  filter(!(str_detect(author, "Stewart_etal") & year == 1995)) |> 
  filter(!(str_detect(author, "Schmidt et al"))) |> 
  # just added to AusTraits
  filter(!(str_detect(author, "Cernusak_etal") & year == 2004)) |>
  filter(!(str_detect(author, "Tennakoon_etal") & year == 1997)) |> 
  # big delta, remove, and not in publication cited
  filter(!(str_detect(author, "Read_etal"))) |> 
  write_csv("data/Cornwell_2017/data.csv")

