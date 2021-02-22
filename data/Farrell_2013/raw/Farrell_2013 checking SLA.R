austraits$traits %>%
    subset(taxon_name %in% c("Arthropodium milleflorum","Chrysocephalum semipapposum","Calytrix tetragona",
                                               "Brachyscome multifida","Correa reflexa","Dianella revoluta var. revoluta",
                                               "Grevillea alpina", "Hibbertia obtusifolia", "Isotoma axillaris", "Lomandra longifolia",
                                               "Stypandra glauca","Veronica perfoliata")) %>%
    subset(trait_name == "specific_leaf_area") %>%  
  write_csv("data/Farrell_2013/raw/checking_SLA_Farrell_2013.csv")
