read_csv("data/Guilherme_Pereira_2019/raw/resorption.csv") %>%
  select(-`P (mature) (mg/g)`, -`Asat (area) umol m-2 s-1`, -`Amax (area) umol m-2 s-1`,
  -`Asat (mass) nmol g-1 s-1`, -`Amax (mass) nmol g-1 s-1`,-`PNUEsat (umol CO2 g-1 N s-1)`,
  -`PNUEmax (umol CO2 g-1 N s-1)`, -`PPUEsat (umol CO2 g-1 P s-1)`, -`PPUEmax (umol CO2 g-1 P s-1)`) -> resorption

read_csv("data/Guilherme_Pereira_2019/raw/leaf_traits.csv") %>%
  left_join(resorption,by=c("Species","Replicate","Stage")) %>%
  mutate(`N (mature) (mg/g)`= replace_na(`N (mature) (mg/g)`,0,`N (mature) (mg/g)`)) %>%
  mutate(`N (mg g-1)`= replace_na(`N (mg g-1)`,0,`N (mg g-1)`)) %>%
  mutate(`N (mature) (mg/g)` = `N (mature) (mg/g)` + `N (mg g-1)`) %>%
  select(-`N (mg g-1)`) %>%
  mutate(Stage = paste("Stage_",Stage,sep="")) %>%
  write_csv("data/Guilherme_Pereira_2019/data.csv")