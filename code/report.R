```{r, echo=FALSE, results="hide"}
austraits <- remake::make('austraits')
id = 'dataset_002' # need to work out a way to derive this from the directory
study_data <- subset(austraits$data, study == id)
study_metadata <- subset(austraits$metadata, dataset_id == id)
```

# Report for study: `r paste0(id, study_metadata$data_description, sep = ": ")`

## Contact Information

**Data contributor:** `r study_metadata$data_contributor`

**Email:** `r paste0('')`

**Institution:**
  
  `r study_metadata$contributor_institution`

## Data source

**Primary source:** `r study_metadata$primary_source`

## Overview of data provided

The dataset contains `r nrow(study_data)` individual datapoints across `r length(unique(study_data$species_name))` taxa, with data included for the following variables:
  
  for `r length(unique(study_data$trait_name))` plant traits,
  
  ```{r summary_table, results='asis', echo=FALSE}
# From the ms functions:
tab <- summary_table(data_study$data, baad_dictionary)
kable(tab[names(tab) != "Studies"], "markdown", align="l", padding=10)
```


## Plots of data

This is how the study `r study_name` fits in the entire dataset (grey). each colour represents a species. A legend of species names with colours is included at the end for reports with 1 < n < 20 species.

```{r variable_plots, fig.height=12, fig.width=12, echo=FALSE}
report_variable_plots(data_study, baad_data, baad_dictionary)
```