

# Contributing to austraits.build

We envision AusTraits as an on-going collaborative community resource that:

1. Increases our collective understanding of the Australian flora;
2. Facilitates accumulating and sharing of trait data;
3. Aspires to fully transparent and reproducible research of highest standard, and 
4. Builds a sense of community among contributors and users.

We'd love for you to contribute. You can read more about the ways you can contribute below.

- [Contributing new data](#data)
- [Improving data quality and reporting errors ](#quality)
- [Improving documentation](#documentation)
- [Development of `traits.build`` workflow](#traits_build)

Please note that the AusTraits project hasd adopted a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project you agree to abide by its terms.

## Improving data quality and reporting errors{#quality}

All users can contribute to continual improvement, by reporting issues you encounter. 

If you notice a possible error in AusTraits, please [post an issue on GitHub](https://github.com/traitecoevo/austraits.build/issues). If you can, please provide code illustrating the problem.
## Improving documentation {#documentation}

All users can contribute to continual improvement of AusTraits documentation, by letting us know what parts of our documentation were unclear. 

If you have a suggestion, please [post an issue on GitHub](https://github.com/traitecoevo/austraits.build/issues). 
## Development of `traits.build`` workflow {#traits_build}

AusTraits uses the `traits.build` package to haronise different sources. Interested users can help us develop this package at the package website <https://github.com/traitecoevo/traits.build/>

## Contributing new data {#data}

We gladly accept new data contributions to AusTraits, including recently collected trait data, legacy trait data from your file archives, transcribed reference works, and transcribed datasets from the literature.

If you would like to contribute data, the requirements are:

-   Data was collected for Australian plant species growing in Australia
-   You collected data on one of the traits listed in the [trait definitions table](http://traitecoevo.github.io/austraits.build/articles/trait_definitions.html)
-   You are willing to release the data under an open license for reuse by the scientific community
-   You make it is as easy as possible for us to incorporate your data by following the instructions.

### What do i need to do?

The AusTraits curtaors will merge each dataset into AusTraits. For each study we carefully check to ensure units are accurate, continuous trait values map in the expected range, categorical trait values map onto sensible terms, location data are accurate, taxon names are aligned to current standards, and all metadata are recorded. 



As a first step, all we really require is a **Data Spreadsheet** and a copy of your **Manuscript**. 

After completing a series of quality checks, we will send you a report to review that summarises the data and metadata. The reports include plots for each continuous trait, comparing values in your submission to those already in AusTraits. It plots your study locations (sites) on a map. It summarises your metadata and indicates the taxonomic alignments made. The report includes both targeted questions (sometimes) and automated questions, acting as prompts to review aspects of the report. Reviewing your report should not take long, and confirms the transparent, thorough process used to build AusTraits.

### Data

**Your dataset, preferably in a spreadsheet format.**

•	**Traits:** Make sure the trait names used in your dataset are easy to interpret or, alternatively, provide a brief definition

•	**Units:** Please make sure the units for each trait are provided as part of the trait name or in a separate spreadsheet/worksheet

•	**Value type:** We prefer to incorporate raw values (or individual means) in AusTraits, but can use population or multi-site means if that is what is available. For mean values, please provide sample size.

•	**Location:** For field studies, please provide location details (see more below).

•	**Context:** Optional, but AusTraits can read in one (or more) column(s) with contextual information, such as canopy position, experimental manipulation, dry vs. wet season, etc.

•	**Collection date:** Optional, but AusTraits can read in a column with sampling date (in any format)

•	**Species/taxa:** Please provide complete species names or a look-up table to match species codes. Out-dated taxonomy is fine – we have name-matching algorithms.


### Metadata 

The AusTraits structure has fields to input all metadata associated with your study, including methods, location details, and context. In detail:

•	**Methods:** For published studies the necessary methods and study information can be extracted from a publication; just attach a copy of the manuscript or the DOI. 

  -	The only commonly missing information is the general sampling period, such as ‘October-December 2020’; this is only required if your data file doesn't have a date column.
  
  -	For unpublished studies, provide brief methods for how each trait was measured; you can simply refer to a standard published protocol

•	**Study locations:** Whenever possible, AusTraits includes location names, location coordinates (latitude/longitude), and any other location properties you have measured/recorded (vegetation description, soil chemistry, climate data, etc.). This information can be provided as a second spreadsheet or as additional columns in the main data spreadsheet. Just make sure the location name is the same in both spreadsheets.

•	**Context:** If your study includes contextual variables, make sure the context values are included as columns in the data spreadsheet. Also, please make sure the contextual values are self-explanatory or provide the necessary explanation.

•	**Authors:** Authorship is extended to anyone who played a key intellectual role in the experimental design and data collection. Most studies have 1-3 authors. For each author, please provide a **name**, **institutional affiliation**, **email address**, and their **ORCID** (if available). Please nominate a single contributor to be the dataset's point of contact; this person's email will not be listed in the metadata file, but is the person future AusTraits users are likely to seek out if they have questions. Additional field assistants can be listed.

•	**Source:** The published manuscript is generally the source. If different traits or observations from a single dataset were published separately, please provide both references. If the dataset you are submitting is a compilation from many sources, please provide a complete list of sources and indicate which rows of data are attributable to which source.


### Common hang-ups

•	**Categorical trait values:** If you have categorical traits, please define any trait values (i.e. entries for that trait) that are not self-explanatory. A copy of our definitions file, including allowable values for each trait is available [here](http://traitecoevo.github.io/austraits.build/articles/trait_definitions.html). The definitions file is a work-in-progress and additional trait values can be added if needed to capture the exact meaning you intended.

•	**Data sourced from others:** For numerical traits, AusTraits strives to only include data collected by you for this project, to avoid having multiple entries of the same measurement/observation. If you have certain trait values that were sourced from the literature, an online database, or colleagues, please indicate that clearly. If trait values for some species were collected by you and others were sourced, it is very helpful if you could add a column to your spreadsheet that indicates the source for different rows of data.

