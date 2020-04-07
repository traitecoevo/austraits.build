read_csv("data/Dong_2017/raw/Dong et al_2017_BG.csv") %>%
  rename(`longitude (deg)` = "longitude", `latitude (deg)`= "latitude", `elevation (m)` = "elv_m", 
         `growing degree days` = "mGDD0", `moisture index` = "MI", `PET (mm)` = "PET_mm", 
         `AET (mm)` =  "AET_mm", `MAT (C)` = "MAT", `MAP (mm)` =  "MAP_mm", 
         `mean vapor pressure deficit (kPa)` =  "mVPD0_ka") %>%
  write_csv("data/Dong_2017/data.csv") %>%
  distinct(site, .keep_all = TRUE) %>%
  select(site,`longitude (deg)`, `latitude (deg)`, `elevation (m)`, `growing degree days`, `moisture index`, 
         `PET (mm)`, `AET (mm)`, `MAT (C)`, `MAP (mm)`, `mean vapor pressure deficit (kPa)`) %>%
  write_csv("data/Dong_2017/site_data.csv") -> site_data