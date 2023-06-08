
README.

Description of three files (plot locations, two datasets) pertaining to Enright et al. Journal of Ecology.  TITLE: Resistance and resilience to changing climate and fire regime depend on plant functional traits

These files contain plot scale information (location, site history) and two datasets analysed in the publication.  

###########################################################################
FILE:  PFT_PlotScaleData_JEcol.csv
Location, fire history, treatment year, soil type, and climate information for each of 34 plots included in the study.

13 columns and 36 rows

COLUMN HEADERS:
PlotID:  unique alphanumeric code for each plot
fireYr:  Year when the plot was previously burnt
treatYr: Year when the plot was experimentally burnt as part of this study
fireInterval:  Years elapsed between fireYr and treatYr
fireRan:  Yes=fire propagated once lit and spread across plot; No=subplots burnt via truck mounted flamethrower
soilType:  acid sandplain (sandplain) or sand over limestone (calcareous); see paper for explanation
utmE:  UTM Easting coordinates, Zone 15
utmN: UTM Northing coordinates, Zone 15
elevation:  meters above sea level
Summer Rain:  mm of rainfall recorded at Eneabba weather station during summer following experimental fires (Summer=Nov-April)
Winter Rain: mm of rainfall recorded at Eneabba weather station during winter following experimental fires (Winter = May-Oct)
Lag1WinterRain:  mm of rainfall recorded at Eneabba weather station during winter in the year preceding experimental fires (Winter = May-Oct)
Lag2WinterRain:  mm of rainfall recorded at Eneabba weather station during winter two years preceding experimental fires (Winter = May-Oct)


Data processing steps: None
Description of associated data stored elsewhere: Other data files associated with this accession
Contact Person:  Dr Joe Fontaine, j.fontaine@murdoch.edu.au Murdoch University

#######################################################################
FILE: PFT_datasetJEcol_1.csv

This is the core dataset associated with Enright et al. Journal of Ecology Resistance and resilience to changing climate and fire regime depend on plant functional traits.  These data detail changes recorded prior and 2-3 years following fire in Mediterranean shrublands of Western Australia.  See publication for study area descriptions and experimental design.

N= 4,319 observations with 9 columns.

COLUMN HEADERS
plotID:  alphanumeric code detailing plot
plot.sub:  alphanumeric code of plot and each of 8 subplots per plot
KangarooScat:  Index of herbivore activity.  Mean of Kangaroo scats counted in n=5 0.5m2 quadrats per subplot at time of final measurement
Genus:  Latin genus for each plant
Species: species name.  
speciesID: Eight letter code created from first four letters of genus and species
soilCanopy: soil: persistent seeds stored in soil (soil) or canopy:  persistent seeds stored in woody fruits in plant canopy (canopy)
resprouter:  Yes=capacity to resprout following fire or No=killed by fire.  This could be any form of resprouting (epicormic, basal etc) but given the crown fire regime of the ecosystem virtually all species resprouting basally either from lignotuberous tissue or root stock.
Chng:  A ratio of post and pre fire count within each subplot.  The ratio is calculated as: (post-pre)/(post+pre) and therefore ranges from -1 (complete loss; absent after fire) to 0 (no change) to +1 (new species; not present before fire).


Data processing steps: None
Description of associated data stored elsewhere: Other data files associated with this accession
Contact Person:  Dr Joe Fontaine, j.fontaine@murdoch.edu.au Murdoch University




###########################################################################
FILE:  PFT_datasetJEcol_2_ResproutersSeedAdult.csv

This datafile comprises data for resprouting species and their mode of regeneration following fire (resprouting extant plants or seedlings).  

COLUMN HEADERS
plotID:  alphanumeric code detailing plot
subplot:  alphanumeric of 8 subplots per plot
Genus:  Latin genus for each plant
Species: species name.  
speciesID: Eight letter code created from first four letters of genus and species
soilCanopy: soil: persistent seeds stored in soil (soil) or canopy:  persistent seeds stored in woody fruits in plant canopy (canopy)
resprouter:  Yes=capacity to resprout following fire or No=killed by fire.  This could be any form of resprouting (epicormic, basal etc) but given the crown fire regime of the ecosystem virtually all species resprouting basally either from lignotuberous tissue or root stock.
RegenType: “r”=resprouting, “s”=seedling; this pertains to observations of species  following treatment with fire.  Plants either resprouted (extant, adult plants) or regenerated from seed (new recruits, seedlings).
Chng:  A ratio of post and pre fire count within each subplot.  The ratio is calculated as: (post-pre)/(post+pre) and therefore ranges from -1 (complete loss; absent after fire) to 0 (no change) to +1 (new species; not present before fire).  

Data processing steps: None
Description of associated data stored elsewhere: Other data files associated with this accession
Contact Person:  Dr Joe Fontaine, j.fontaine@murdoch.edu.au Murdoch University

