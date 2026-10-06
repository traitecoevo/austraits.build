#### Description of the data and file structure

The dataset contains all raw data used in the manuscript: The post-fire recovery of soil seed banks along a fire severity gradient in an Australian threatened mesic forest. The data is in the format of a csv.

File:\
Data_for__The_post-fire_recovery_of_soil_seed_banks_along_a_fire_severity_gradient_in_an_Australian_threatened_mesic_forest_.csv

##### **Description:** 

###### *Variables*

* ExtantorSSB: ‘Extant vegetation’ indicated plant surveys and ‘soil seed bank’ indicates soil cores.
* Severity: Fire severity determined from GEEBAM (Google Earth Engine Burnt Area Map), as outlined in methods of paper (Department of Planning and Environment, 2020). Unburnt, Moderate, High, Extreme.
* FireIntervalGroups: Using publicly available online maps supplied by the State Government of NSW and NSW DCCEEW (2010), as outlined in methods of paper.
* Site: Unique site codes, equivalent to previous NSW BioNet surveys (NSW Government Environtment and Heritage, 2021).
* Quadrat: Plots at each site, 11 2x1m quadrats distributed along a 50 m transect.
* Depth: Differing soil depths, Leaf litter (separated from the top of the soil core) and Soil (8cm soil core). Only relevant for soil seed bank. NA = not available.
* DateFirstEmergence: Date first seedling emerged for each species for each sample. Only relevant for soil seed bank. NA = not available.
* Species: Species name.
* FunctionalGroup: No fire response, Serotinous resprouters, Obligate resprouters, Facultative resprouters, Obligate seeders. For species where resprouting or seed storage could not be determined, the functional group was recorded as Undetermined.
* Count: Number of seedlings that emerged from soil seed bank during year. Only relevant for soil seed bank. NA = not available.
* Cover: Percent of 2x1 metre plot covered by species in extant vegetation aboveground. Only relevant for extant vegetation, NA = not available.

##### **Software**

R studio version 4.3.0 was used for all data analysis (RStudio Team, 2015).

##### References

Department of Planning and Environment 2020. Google Earth Engine Burnt Area Map (GEEBAM). The Seed Initiative.
Nsw Government Environtment and Heritage 2021. BioNet Atlas.
Rstudio Team. 2015. RStudio: Integrated development for R [Online]. Boston, MA: RStudio, Inc. Available: [http://www.rstudio.com/](http://www.rstudio.com/) [Accessed 22 March 2020].
State Government of Nsw and Nsw Dcceew 2010. NPWS Fire History - Wildfires and Prescribed Burns, accessed from The Sharing and Enabling Environmental Data Portal.
