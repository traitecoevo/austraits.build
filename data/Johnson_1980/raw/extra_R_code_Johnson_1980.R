# Writes data/Johnson_1980/data.csv from raw/trait_data.csv, filling location_name for
# herbarium specimens whose collecting locality was georeferenced by Li et al. (2025,
# New Phytologist, SI Table S1; see data/Li_2025). Johnson (1980) gives only the state.
# Each Li et al. row (ID) was matched to a voucher on taxon (Li et al. use current names),
# state and identical adaxial and abaxial cuticle thicknesses. Not matched: L. scoparium
# (Li ID 1546; Johnson only examined cultivated specimens) and two L. lanigerum rows
# (IDs 1555, 1556; cuticle values fit both Carolin 918 and Pullen 4131). Excluded because
# Li et al.'s coordinates fall outside the state Johnson gives: Johnson 2709 and
# Story & Yapp 240 (Queensland, placed in Sydney) and Carolin 1088 (Western Australia,
# placed in the Blue Mountains). The coordinates are in the locations block of metadata.yml.

library(dplyr)
library(readr)

li_ids <- tribble(
  ~voucher, ~li_id,
  "Constable 16298",     1518,
  "Adams 1463",          1521,
  "Evans 2723",          1520,
  "Kaspiew 19",          1519,
  "Cheel 121544",        1522,
  "Eichler 16197",       1523,
  "Pritzel 248",         1524,
  "Kaspiew 1527",        1525,
  "Coveny 3980",         1526,
  "Aplin 2631",          1528,
  "Smith 12341",         1530,
  "Fell F672",           1531,
  "Hubbard 4748",        1543,
  "Schodde 5105",        1541,
  "Carolin 1455",        1545,
  "Constable 10968",     1548,
  "Coveny 3672",         1550,
  "Ashby 3154",          1561,
  "Donner 958",          1560,
  "Evans 2712",          1558,
  "Kraehnebuehl 933",    1559,
  "Walker 1223",         1557,
  "Constable 19175",     1562,
  "Smith 11962",         1563,
  "Pedley 1421",         1571,
  "Webb & White 2138",   1569,
  "Darbyshire 151",      1572,
  "Boorman 14161",       1573,
  "Brass 19983",         1574,
  "McKee 7495",          1575,
  "Whibley 841",         1576,
  "Hoogland 10060",      1577,
  "Darbyshire 94",       1578,
  "Boorman 14170",       1579,
  "Hubbard 8637",        1581,
  "White 6234",          1580,
  "Constable 7455",      1583,
  "Darbyshire 88",       1582,
  "Ashby 3595",          1586,
  "Morrison",            1592,
  "Constable 92186",     1593,
  "Briggs 3064",         1594,
  "v. Balgooy 1451",     1595 
)

read_csv("data/Johnson_1980/raw/trait_data.csv", col_types = cols(.default = col_character())) %>%
  mutate(location_name = if_else(entity_type == "individual" & voucher %in% li_ids$voucher,
                                 paste(voucher, "locality"), NA_character_)) %>%
  write_csv("data/Johnson_1980/data.csv", na = "", eol = "\r\n")
