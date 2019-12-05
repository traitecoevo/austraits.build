library(jurien)

jurien_sites %>% dplyr::bind_cols(jurienplots[1:8],juriensoil_plot[2:17])
write.csv(jurien_sites,"data/Laliberte_2012/site_data.csv")

jurienleafnut %>%
  subset(state=="senesced") %>%
  select(plot, species, N, P, K) -> jurien_senesced
names(jurien_senesced) <- c("plot", "species", "N_senesced", "P_senesced", "K_senesced")

jurienleafnut %>%
  subset(state=="mature") %>%
  select(plot, species, N, P, K, C, d15N, d13C, Ca, Cu, Fe, Mg, Mn, Mo, Na, S, Zn) -> jurien_mature
names(jurien_mature) <- c("plot", "species", "N_mature", "P_mature", "K_mature", "C_mature", "d15N", "d13C",
                          "Ca_mature", "Cu_mature", "Fe_mature", "Mg_mature", "Mn_mature", "Mo_mature", 
                          "Na_mature", "S_mature", "Zn_mature")

jurien_leaf2 <- dplyr::left_join(jurien_mature,jurien_senesced,by=c("plot","species"))
jurien_traits <- dplyr::left_join(jurienleaf,jurien_leaf2,by=c("plot","species"))
jurien_traits <- dplyr::left_join(jurien_traits,juriensp,by=c("species"))
write.csv(jurien_traits,"data/Laliberte_2012/data.csv")

names(jurien_traits)
View(jurienleaf)
