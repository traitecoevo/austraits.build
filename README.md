
![](docs/logo.png)

# `AusTraits (build)`: source for curated plant trait database for the Australian flora

## Introduction

AusTraits is a transformative database, containing measurements on the
traits of Australia’s plant species, standardised from hundreds of
disconnected primary sources. So far, data have been assembled from 100
distinct sources, describing more than 80 plant traits and over 19k
species.

In addition to any general license applying to the dataset, any usage requires acknowledgement of the following institutions permitting inclusion of specific resources in this compilation:  

- Barlow_1981: Australian Biological Resources Study.
- BRAIN_2007: Brisbane Rainforest Action and Information Network
- Hyland_2003, CPRR_2002: CSIRO's Centre for Australian National Biodiversity Research
- NTH_2014: Northern Territory Herbarium
- RBGK_2014, Kew_2019_1, Kew_2019_2, Kew_2019_3, Kew_2019_4, Kew_2019_5, Kew_2019_6: Kew Botanic Gardens
- NHNSW_2016, RBGSYD_0000, RBGSYD_2014: PLantNet, National Herbarium of NSW
SAH_2014: State Herbarium of South Australia

## Building the database from source
To handle the harmonising of diverse data sources, we use a reproducible
workflow to implement the various changes required for each source to
reformat it suitable for incorporation in AusTraits. Such changes
include restructuring datasets, renaming variables, changing variable
units, changing taxon names.

This repository contains the raw data and code used to compile the 
AusTraits database from the original sources. For the sake of 
transparency and continuing development, the entire workflow is made 
available here. This material will be of interest to anyone 
interested in contributing to the ongoing development of AusTraits. 

## Accessing and use of data

Those interested in using data from AusTraits (most people), should
visit the repositories for the compiled resource, at either: the 
AusTraits repository on github: 
[traitecoevo/austraits](https://github.com/traitecoevo/austraits) 
or the versioned releases archived on zenodo at doi: [10.5281/zenodo.3568417](http://doi.org/https://doi.org/10.5281/zenodo.3568417).

That repository contains detailed information on the use of AusTraits. 

## A versioned database 

AusTraits is continually evolving, as new datasets are contributed. As 
such, there is no single canonical version. We are continually making 
new versions available. Each new version is achieved in the release database
available at [traitecoevo/austraits](https://github.com/traitecoevo/austraits)) and
[on zenodo at doi: 10.5281/zenodo.3568417](http://doi.org/https://doi.org/10.5281/zenodo.3568417).

Overtime, we expect that different versions will be released and used in different analyses. 

![](docs/Workflow.png)


## Building the database

AusTraits can be rebuilt from source (raw data files) using the materials provided 
here, which make transparent the process of combining data from multiple sources.

Full details on building the are available at 

## Contributing

We envision AusTraits as an on-going collaborative community resource
that:

1.  Increases our collective understanding the Australian flora; and
2.  Facilitates accumulating and sharing of trait data;
3.  Builds a sense of community among contributors and users; and
4.  Aspires to fully transparent and reproducible research of highest
    standard.

As a community resource, we are very keen for people to contribute. Here
are some of the ways you can contribute:

**Reporting Errors**: If you notice a possible error in AusTraits,
please [post an issue](https://github.com/traitecoevo/austraits.build/issues) .

**Refining documentation:** We welcome additions and edits that make
using the existing data or adding new data easier for the community.

**Contributing new data**: We gladly accept new data contributions to
AusTraits. If you would like to contribute data, the minimal requirements are

1. Data were collected for Australian plant species growing in Australia
2. You collected data on one of the traits list in the [trait definitions table](config/definitions.yml).
3. You are willing to release the data under an open license for reuse by the scientific community.

See [these instructions](docs/CONTRIBUTING.md) on how to prepare data.

## Acknowledgements


