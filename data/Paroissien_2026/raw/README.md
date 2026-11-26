# Dryad dataset

Dataset DOI: [10.5061/dryad.cjsxksnjw](10.5061/dryad.cjsxksnjw)

## Preface

The following README provides information regarding two csv files containing the raw data for the manuscript ‘The effectiveness of seed rain in recolonising an ecotonal mesic forest following extreme severity fire’.

## Files and variables

## Dataset Title

SeedRain.csv

### Overview

This data set contains all raw data for seeds recovered from seed traps and related trait groupings for the manuscript ‘The effectiveness of seed rain in recolonising an ecotonal mesic forest following extreme severity fire’. The data is in the format of a csv.

### Table Columns

Site – Mt Wilson or Mt Tomah

SeverityGroup – Severity as described in the methods of the paper, (Unburnt, Low/Moderate, High/Extreme)

SeedTrapline – Row of seed traps, unique values based on site, severity, and distance from fire edge.

DistanceEdge – Distance from fire edge, numeric value, positive values indicate distance into burnt area, negative values indicate distance outside of burnt area (unburnt).

TrapCount – Unique identifier for trap per site, severity and distance.

DateInitial – Indicates earliest date seed can have dispersed into trap

DateCollected – indicates date seed collected

Family – Species family. NA indicates no associated species with seed trap for time point.

Species – Scientific species name. NA indicates no associated species with seed trap for time point.

DispersalType – Common dispersal type of species. Categorical: Adhesion, Ant, Ballistic, Bird or wind, Gravity, Vertebrate, Wind. NA indicates no species. Vertebrate indicates seed types likely dispersed by animals, including mammals and birds, usually possessing a fleshy fruit. Bird or wind referred to seed types likely dispersed by either, usually not possessing a fleshy fruit. NA indicates no associated species with seed trap for time point.

HeightRange – Average plant height in metres grouped into categories: 0.5 to 1.5, 0.1 to 0.5, 0.5 to 3, 0.5 to 10, 10 to 25. NA indicates no associated species with seed trap for time point. ‘Unknown’ indicates species was unknown, and value could not be determined.

Form – life form of species. Categories: Climber, Grass, Herb, Sedge, Shrub, Tree. NA indicates no associated species with seed trap for time point.

SeedSize – Average weight of seed per species as described in the methods of the paper. Numeric value in mg. NA indicates no associated species with seed trap for time point.

Count – number of seeds per species for time period in trap. NA indicates no associated species with seed trap for time point.

MinimumDist - Distance of closest mature adult to trap for each species. NA indicates no associated species with seed trap for time point. ‘Unknown’ indicates species was unknown, and value could not be determined, and ‘Not found’ indicates closest mature adult could not be located.

SeedTrapCoverT1 – cover over traps as described in Supplementary material 2 methods measured 3 years post fire.

SeedTrapCoverT2 - cover over traps as described in Supplementary material 2 methods measured 3 years 8 months post fire.

## Dataset Title

2024_distance_dispersal_surveys.csv

### Overview

This data set contains all raw data for vegetation surveys conducted in 2024 to determine dispersal distances of species found in seed traps. This data is associated with the manuscript ‘The effectiveness of seed rain in recolonising an ecotonal mesic forest following extreme severity fire’. The data is in the format of a csv.

### Table Columns

Date – Date plant surveys conducted

Site – Mt Wilson or Mt Tomah

Species – Scientific species name

SeedTrapline – Row of seed traps, unique values based on site, severity, and distance from fire edge. NA values indicate no related line of seed traps.

SurveySiteCode - unique values for surveys based on site, severity, and distance from fire edge or transect number.

DistanceEdge – Distance from fire edge, numeric value, positive values indicate distance into burnt area, negative values indicate distance outside of burnt area (unburnt).

SeverityGroup - Severity as described in the methods of the paper, (Unburnt, Low/Moderate, High/Extreme)

## Code/software

## Software

RStudio version 4.3.0 was used for all data analysis (RStudio Team, 2023).

## References

NSW Government 2023. BioNet Atlas. NSW Government.

RStudio Team. 2023. *RStudio: Integrated development for R *[Online]. Boston, MA: RStudio, Inc. Available: [http://www.rstudio.com/](http://www.rstudio.com/) [Accessed 22 March 2020].
