read_csv("data/Detombeur_2021/data.csv") %>%
  distinct(Plot, .keep_all = TRUE) %>%
  select(Plot,`latitude (deg)`,`longitude (deg)`,`Chronosequence stage`,`Cation exchange capacity (cmolc/kg)`,`pH-CaCl2`,
         `Soil carbonates content (%)`,`Soil total P (mg/kg)`) -> sites

