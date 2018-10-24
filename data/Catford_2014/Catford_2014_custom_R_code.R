data0 <- read_csv("data/Catford_2014/data.csv", col_types = cols())

data1 <- data0

data1$plant_growth_form <- paste(data1$Tree,data1$Gram,data1$Forb,sep="_")
for (i in 1:nrow(data1)){
  if(data1$plant_growth_form[i]=="1_NA_NA") {
    data1$plant_growth_form[i] <- "tree"
  } 
  if(data1$plant_growth_form[i]=="NA_1_NA") {
    data1$plant_growth_form[i] <- "graminoid"
  } 
  if(data1$plant_growth_form[i]=="NA_NA_1") {
    data1$plant_growth_form[i] <- "herb"
  } 
  if(data1$plant_growth_form[i]=="NA_NA_NA") {
    data1$plant_growth_form[i] <- NA
  } 
}

data1$life_history <- paste(data1$Annual,data1$Biennial,data1$Perennial,sep=" ")
for (i in 1:nrow(data1)){
  if(data1$life_history[i]=="1 NA NA") {
    data1$life_history[i] <- "annual"
  } 
  if(data1$life_history[i]=="NA 1 NA") {
    data1$life_history[i] <- "biennial"
  } 
  if(data1$life_history[i]=="NA NA 1") {
    data1$life_history[i] <- "perennial"
  } 
  if(data1$life_history[i]=="1 NA 1") {
    data1$life_history[i] <- "annual perennial"
  } 
  if(data1$life_history[i]=="1 1 NA") {
    data1$life_history[i] <- "annual biennial"
  } 
  if(data1$life_history[i]=="NA NA NA") {
    data1$life_history[i] <- NA
  } 
}

data1$growth_habit <- paste(data1$rhizomes,data1$stolons,data1$`prostrate/decumbent`,sep=" ")
for (i in 1:nrow(data1)){
  if(data1$growth_habit[i]=="1 NA NA") {
    data1$growth_habit[i] <- "rhizomatous"
  } 
  if(data1$growth_habit[i]=="NA 1 NA") {
    data1$growth_habit[i] <- "stoloniferous"
  } 
  if(data1$growth_habit[i]=="NA NA 1") {
    data1$growth_habit[i] <- "prostrate"
  } 
  if(data1$growth_habit[i]=="NA 1 1") {
    data1$growth_habit[i] <- "stoloniferous prostrate"
  } 
  if(data1$growth_habit[i]=="1 1 1") {
    data1$growth_habit[i] <- "rhizomatous stoloniferous prostrate"
  }
  if(data1$growth_habit[i]=="1 NA 1") {
    data1$growth_habit[i] <- "rhizomatous prostrate"
  }
  if(data1$growth_habit[i]=="1 1 NA") {
    data1$growth_habit[i] <- "rhizomatous stoloniferous"
  } 
  if(data1$growth_habit[i]=="NA NA NA") {
    data1$growth_habit[i] <- NA
  } 
}

data1$aquatic_terrestrial <- paste(data1$Terrestrial,data1$Tda,data1$Tdr,data1$Amphibious,data1$Submerged,sep=" ")
for (i in 1:nrow(data1)){
  if(data1$aquatic_terrestrial[i]=="1 NA NA NA NA") {
    data1$aquatic_terrestrial[i] <- "terrestrial"
  } 
  if(data1$aquatic_terrestrial[i]=="1 1 NA NA NA") {
    data1$aquatic_terrestrial[i] <- "terrestrial tda"
  } 
  if(data1$aquatic_terrestrial[i]=="1 NA 1 NA NA") {
    data1$aquatic_terrestrial[i] <- "terrestrial tdr"
  } 
  if(data1$aquatic_terrestrial[i]=="NA NA NA 1 NA") {
    data1$aquatic_terrestrial[i] <- "amphibious"
  } 
  if(data1$aquatic_terrestrial[i]=="NA NA NA NA 1") {
    data1$aquatic_terrestrial[i] <- "submerged"
  } 
  if(data1$aquatic_terrestrial[i]=="NA NA NA NA NA") {
    data1$aquatic_terrestrial[i] <- NA
  } 
}


for (v in c("Authority","Family","Latitude","Longitude","Altitude","Sampling date", 
           "Maturity","Plant condition","Growing conditions","Tree","Forb","Gram","Perennial","Biennial","Annual",
           "Specific leaf mass (mg/mm2)","rhizomes","stolons","prostrate/decumbent",
           "Terrestrial","Tda","Tdr","Amphibious","Submerged")) {
  data1[[v]] <- NULL
}

data2 <- data0 %>%
  # replace NA with 0
  replace_na(list(Tree = 0, Gram = 0, Forb=0, Annual=0, Biennial=0, Perennial=0, rhizomes=0,stolons=0,`prostrate/decumbent`=0, Terrestrial=0, Tda=0, Tdr=0, Amphibious=0)) %>%
  # Replace binary 1-0 with names
  mutate(
      Tree = ifelse(Tree, "tree", ""),
      Gram = ifelse(Gram, "graminoid", ""),
      Forb = ifelse(Forb, "herb", ""),
      Annual = ifelse(Annual, "annual", ""),
      Biennial = ifelse(Biennial, "biennial", ""),
      Perennial = ifelse(Perennial, "perennial", ""),
      rhizomes = ifelse(rhizomes, "rhizomatous", ""),
      stolons = ifelse(stolons, "stoloniferous", ""),
      `prostrate/decumbent` = ifelse(`prostrate/decumbent`, "prostrate", ""),
      Terrestrial = ifelse(Terrestrial, "terrestrial",""),
      Tda = ifelse(Tda, "tda",""),
      Tdr = ifelse(Tdr, "tdr",""),
      Amphibious = ifelse(Amphibious, "amphibious","")
      ) %>%
  # Merge separate variables into single trait
  unite(col = "plant_growth_form", Tree, Gram, Forb, sep = " ") %>%
  unite(col = "life_history", Annual, Biennial, Perennial, sep = " ") %>%
  unite(col = "growth_habit", rhizomes, stolons, `prostrate/decumbent`, sep = " ") %>%
  unite(col = "aquatic_terrestrial", Terrestrial, Tda, Tdr, Amphibious, sep = " ")  %>%
  # Clean up white space
  mutate(
      plant_growth_form = stringr::str_squish(plant_growth_form) %>% na_if(""),
      life_history = stringr::str_squish(life_history) %>% na_if(""),
      growth_habit = stringr::str_squish(growth_habit) %>% na_if(""),
      aquatic_terrestrial = stringr::str_squish(aquatic_terrestrial) %>% na_if("")
      ) %>% 
  # remove extra variables
  select(-Authority,-Family,-Latitude,-Longitude,-Altitude,-"Sampling date", 
    -Maturity,-`Plant condition`,-`Growing conditions`, 
    -`Specific leaf mass (mg/mm2)`, - Submerged)

all.equal(data2, data1[,names(data2)])
