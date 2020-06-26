read_csv("data/Choat_2012/data.csv") %>%
  distinct(Location,.keep_all = TRUE) %>%
  select(Location, Latitude, Longitude,Elevation_m, Biome,`MAT (mean annual temperature) C`,`MAP (mean annual precipitation) mm`,
         `MPDQ (mean precipitation of the driest quarter) mm`,`PET (potential evapotranspiration) mm`,`AI (aridity index)`,Reference) %>%
  rename(`latitude (deg)` = Latitude, `longitude (deg)` = Longitude, description = Biome, `elevation (m)` = Elevation_m,
         `MAT (C)` = `MAT (mean annual temperature) C`, `MAP (mm)`= `MAP (mean annual precipitation) mm`,
         `mean precipitation of the driest quarter (mm)` =`MPDQ (mean precipitation of the driest quarter) mm`,
         `PET (mm)` = `PET (potential evapotranspiration) mm`) -> sites