read_csv("data/Bolza_1975/data.csv") %>%
  mutate(secondary_ref = "Bolza_1975") -> Bolza_1975

read_csv("data/CAB_2009/data.csv") %>%
  mutate(secondary_ref = "CAB_2009") -> CAB_2009

read_csv("data/Chave_2009/data.csv") %>%
  mutate(secondary_ref = "Chave_2009") -> Chave_2009

read_csv("data/Chudnoff_1984/data.csv") %>%
  mutate(secondary_ref = "Chudnoff_1984") -> Chudnoff_1984

read_csv("data/CIRAD_2009/data.csv") %>%
  mutate(secondary_ref = "CIRAD_2009") -> CIRAD_2009

read_csv("data/Desh_1996/data.csv") %>%
  mutate(secondary_ref = "Desh_1996") -> Desh_1996

read_csv("data/Flynn_2001/data.csv") %>%
  mutate(secondary_ref = "Flynn_2001") -> Flynn_2001

read_csv("data/Hong_1999/data.csv") %>%
  mutate(secondary_ref = "Hong_1999") -> Hong_1999

read_csv("data/ICRAF_2009/data.csv") %>%
  mutate(secondary_ref = "ICRAF_2009") -> ICRAF_2009

read_csv("data/Meier_2007/data.csv") %>%
  mutate(secondary_ref = "Meier_2007") -> Meier_2007

read_csv("data/Prospect_2009/data.csv") %>%
  mutate(secondary_ref = "Prospect_2009") -> Prospect_2009

read_csv("data/Seng_1951/data.csv") %>%
  mutate(secondary_ref = "Seng_1951") -> Seng_1951

read_csv("data/Wells_2009/data.csv") %>%
  mutate(secondary_ref = "Wells_2009") -> Wells_2009

Bolza_1975 %>%
  bind_rows(CAB_2009) %>%
  bind_rows(Chave_2009) %>%
  bind_rows(Chudnoff_1984) %>%
  bind_rows(CIRAD_2009) %>%
  bind_rows(Desh_1996) %>%
  bind_rows(Flynn_2001) %>%
  bind_rows(Hong_1999) %>%
  bind_rows(ICRAF_2009) %>%
  bind_rows(Meier_2007) %>% 
  bind_rows(Prospect_2009) %>% 
  bind_rows(Seng_1951) %>% 
  bind_rows(Wells_2009) %>% 
  write_csv("data/Zanne_2009/data.csv")


#Notes:
#  not included: Goldsmith_1981, Dimitri_1973 - probably remove
#  Ilic_2000 - left as independent study