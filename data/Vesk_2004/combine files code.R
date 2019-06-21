Vesk1 <- read_csv("data/Vesk_2004/data_original.csv")
Vesk2 <- read_csv("data/Vesk_2004/raw/Vesk_Leishman_Westoby_2004_austraits.csv")
Vesk_merge <- dplyr::left_join(Vesk1, Vesk2, by = "name_original")
Vesk_merge %>% mutate(
  site = paste0(site_text.x,": ",site_text.y),
  site = gsub(": NA","",site),
  site = gsub("NA: ","",site)) -> Vesk_merge

Vesk_merge <- select(Vesk_merge,c("name_original","SLA","LMA","leaf dry matter content (mg/g)","height (cm)","plant_height_cm","growth form","sprouting_w/i/s","site"))

write_csv(Vesk_merge,"data/Vesk_2004/data.csv")
