data1 <- read.csv("~/austraits/data1/Catford_2014/data.csv")

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
    data1$plant_growth_form[i] <- ""
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
    data1$life_history[i] <- ""
  } 
}

data1$growth_habit <- paste(data1$rhizomes,data1$stolons,data1$prostrate.decumbent,sep=" ")
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
  if(data1$growth_habit[i]=="NA NA NA") {
    data1$growth_habit[i] <- ""
  } 
}

data1$aquatic_terrestrial <- paste(data1$Terrestrial,data1$Tda,data1$Tdr,data1$Amphibious,data1$Submerged,sep=" ")
for (i in 1:nrow(data1)){
  if(data1$aquatic_terrestrial[i]=="1 NA NA NA NA") {
    data1$aquatic_terrestrial[i] <- "terrestrial"
  } 
  if(data1$aquatic_terrestrial[i]=="1 1 NA NA NA") {
    data1$aquatic_terrestrial[i] <- "terrestrial-Tda"
  } 
  if(data1$aquatic_terrestrial[i]=="1 NA 1 NA NA") {
    data1$aquatic_terrestrial[i] <- "terrestrial-Tdr"
  } 
  if(data1$aquatic_terrestrial[i]=="NA NA NA 1 NA") {
    data1$aquatic_terrestrial[i] <- "amphibious"
  } 
  if(data1$aquatic_terrestrial[i]=="NA NA NA NA 1") {
    data1$aquatic_terrestrial[i] <- "submerged"
  } 
  if(data1$aquatic_terrestrial[i]=="NA NA NA NA NA") {
    data1$aquatic_terrestrial[i] <- "NA"
  } 
}


for (v in c("Authority","Family","Latitude","Longitude","Altitude","Sampling date", 
           "Maturity","Plant condition","Growing conditions","Tree","Forb","Gram","Perennial","Biennial","Annual",
           "Specific leaf mass (mg/mm2)","rhizomes","stolons","prostrate/decumbent",
           "Terrestrial","Tda","Tdr","Amphibious","Submerged")) {
  data1[[v]] <- NULL
}


