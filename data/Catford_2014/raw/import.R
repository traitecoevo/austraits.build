# Code to make a single dataset from two Excel sheets supplied by Catford

library(dplyr)
library(readr)

# main trait data set
data1 <- read_csv("data/Catford_2014/raw/traits.csv", col_types = cols()) %>% 
				select(-Authority,-Family,-Latitude,-Longitude,-Altitude,
				       -`Sampling date`, -Maturity,-`Plant condition`,
				       -`Growing conditions`, -`Specific leaf mass (mg/mm2)`, - Submerged)

# extra leaf traits measured in this study to fill gaps
data2 <- read_csv("data/Catford_2014/raw/leaves.csv", col_types = cols()) %>% 
					filter(`Leaf part`!="sheath") %>%
  				rename(Taxa = Species,
  				       `Leaf size (cm^2)` = `Area per leaf (cm^2)`
  				       ) %>%
  				group_by(Taxa) %>%
  				summarise_at(c("Leaf size (cm^2)","SLA (mm2/mg)", "Mass per leaf (g)"), .funs=mean) %>%
  				ungroup()

# match spp from data 2 in data 1
i <- match(data2$Taxa, data1$Taxa)

data1 %>% 
	bind_rows( 
	          # Add data leaf size and SLA for spp with no records in master traits
	          data2 %>% filter(is.na(i)) %>% select(-`Mass per leaf (g)`)
	          ) %>% 
	# Now add data for final trait
	full_join(by = "Taxa", 
	          data2 %>% select(Taxa, `Mass per leaf (g)`)
	          ) %>% 
	write_csv("data/Catford_2014/data.csv", na = "")
