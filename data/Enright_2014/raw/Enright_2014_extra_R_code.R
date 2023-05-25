
library(tidyverse)

data1 <- read_csv("data/Enright_2014/raw/PFT_datasetJEcol_1.csv")
data2 <- read_csv("data/Enright_2014/raw/PFT_datasetJEcol_2_ResproutersSeedAdult.csv")
plot_data <- read_csv("data/Enright_2014/raw/PFT_PlotScaleData_JEcol.csv")
plot_coordinates <- read_csv("data/Enright_2014/raw/plot_UTM_coordinates_converted.csv")

data1 %>% full_join(data2) -> data
#data %>% left_join(plot_data) -> data

# ------------ Investigate data --------------
data %>% filter(resprouter == "Yes") -> resprouters

resprouters %>% 
  group_by(genus, species, speciesID) %>% 
  summarise(variation_in_regen_type = length(unique(RegenType))) %>% 
  ungroup() -> variation_in_regen_type 
# Only three species 'loganiaSP', 'hypocalSP' and 'eremaste' are resprouting only
# (no post-fire recruitment recorded) across all plots

resprouters %>% 
  group_by(genus, species, speciesID, plotID) %>% 
  summarise(variation_in_regen_type = length(unique(RegenType))) %>% 
  ungroup() -> variation_in_regen_type_by_plot
# Regeneration type varies by plot

data %>% distinct(speciesID, soilCanopy) -> distinct_seed_storage
# Species are designated as *either* canopy or soil

# ------------

data <-
  data %>% 
  mutate(location_name = paste("Geraldton Sandplain", plotID, sep = "_"))

plot_data <-
  plot_data %>% 
  mutate(location_name = paste("Geraldton Sandplain", plotID, sep = "_")) %>% 
  left_join(plot_coordinates) %>% 
  mutate(fireRan = if_else(fireRan == "Yes", "fire", "flamethrower")) %>% 
  rename(`site code` = plotID, 
         `fire before last` = fireYr, 
         `fire history (years since experimental fire)` = treatYr,
         `fire interval (years)` = fireInterval,
         `burned using fire or flamethrower` = fireRan,
         `soil type` = soilType,
         `elevation (m)` = elevation,
         `summer rain after experimental fire (mm)` = SummerRain,
         `winter rain after experimental fire (mm)` = WinterRain,
         `winter rain one year before experimental fire (mm)` = Lag1WinterRain,
         `winter rain two years before experimental fire (mm)` = Lag2WinterRain
         )

data %>% 
  group_by(location_name, plotID, genus, species, speciesID, soilCanopy, resprouter) %>% 
  summarise(
    resprouted_after_fire = if_else("r" %in% RegenType, "Yes", "No"),
    seed_recruitment = if_else("s" %in% RegenType, "Yes", "No"),
  ) -> data

data %>% filter(resprouter == "Yes", resprouted_after_fire == "No") %>% View() 
# There are 32 plots where resprouters did not regenerate via resprouting (but they
# probably did resprout in other plots)

data %>% 
  mutate(taxon_name = paste(genus, species)) %>% 
  relocate(taxon_name, .before = genus) -> data

data %>% write_csv("data/Enright_2014/data.csv")
