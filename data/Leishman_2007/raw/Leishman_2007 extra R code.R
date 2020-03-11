read_csv("data/Leishman_2007/raw/Hawkesbury sandstone data joined.csv") %>%
  subset(`plant type` != "Exotic") %>%
  select(-`Common name`) %>%
  mutate(site =  "Hawkesbury sandstone") -> Hawkesbury_sandstone

Hawkesbury_sandstone %>%
  mutate(`species SLA`=if_else(is.na(`species SLA`),(`Area (mm2)`/(100*`mass in chamber (g)`)),`species SLA`)) %>%
  distinct(`species SLA`,.keep_all = TRUE) %>%
  rename(`SLA (cm2/g)` = `species SLA`) %>%
  select(Species, site, Replicate, `plant type`, `SLA (cm2/g)`) %>%
  subset(!is.na(`SLA (cm2/g)`)) -> HS_SLA

Hawkesbury_sandstone %>%
  select(-`species SLA`,-`Area (mm2)`,-`mass in chamber (g)`,-`Photo`) %>%
  rename(`amass (nmol g-1 s-1)` = `Amass (photo per g)`,`aarea (umolm-2s-1)`=`Aarea`,`gs (molm-2s-1)` = Garea,
         `Rd (umol m-2 s-1)`=`Photo_Rd`) -> Hawkesbury_select

read_csv("data/Leishman_2007/raw/data_Leishman_2007_from_Richards_2008_edited_by_E_Wenk.csv") %>%
  rename(Species=name_original) %>%
  bind_rows(Hawkesbury_select) %>%
  bind_rows(HS_SLA) %>%
  mutate(`Rd (umol m-2 s-1)`=-`Rd (umol m-2 s-1)`) %>%
  mutate_at(c("amass (nmol g-1 s-1)","aarea (umolm-2s-1)"), ~na_if(.,0)) %>%
  write_csv("data/Leishman_2007/data.csv")
  
  names(Hawkesbury_select)
  