
<!-- README.md is generated from README.Rmd. Please edit that file -->

![](docs/logo.png)

# `AusTraits (build)`: source for curated plant trait database for the Australian flora

<!-- badges: start -->

<!-- badges: end -->

## Introduction

AusTraits is a transformative database, containing measurements on the
traits of Australia’s plant species, standardised from hundreds of
disconnected primary sources. So far, data have been assembled \> 150
distinct sources, describing more than 100 plant traits and over 19k
species.

The AusTraits project has two connected repositories

  - [austraits.build](https://github.com/traitecoevo/austraits.build/)
    (this repository):
  - [austraits](https://github.com/traitecoevo/austraits/) (this
    repository):

## Building the database from source

To handle the harmonising of diverse data sources, we use a reproducible
workflow to implement the various changes required for each source to
reformat it suitable for incorporation in AusTraits. Such changes
include restructuring datasets, renaming variables, changing variable
units, changing taxon names.

This repository contains the raw data and code used to compile the
AusTraits database from the original sources. For the sake of
transparency and continuing development, the entire workflow is made
available here. This material will be of interest to anyone interested
in contributing to the ongoing development of AusTraits.

## Accessing and use of data

Those interested in using data from AusTraits (most people), should
visit the repositories for the compiled resource, at either: the
AusTraits repository on github:
[traitecoevo/austraits](https://github.com/traitecoevo/austraits) or the
versioned releases archived on zenodo at doi:
[10.5281/zenodo.3568417](http://doi.org/https://doi.org/10.5281/zenodo.3568417).

That repository contains detailed information on the use of AusTraits.

license

## A versioned database

AusTraits is continually evolving, as new datasets are contributed. As
such, there is no single canonical version. We are continually making
new versions available. Each new version is achieved in the release
database available at
[traitecoevo/austraits](https://github.com/traitecoevo/austraits)) and
[on zenodo at
doi: 10.5281/zenodo.3568417](http://doi.org/https://doi.org/10.5281/zenodo.3568417).

Overtime, we expect that different versions will be released and used in
different analyses.

![](docs/Workflow.png)

## Building the database

AusTraits can be rebuilt from source (raw data files) using the
materials provided here, which make transparent the process of combining
data from multiple sources.

Full details on building the are available at

## Contributing

We envision AusTraits as an on-going collaborative community resource
that:

1.  Increases our collective understanding the Australian flora; and
2.  Facilitates accumulating and sharing of trait data;
3.  Builds a sense of community among contributors and users; and
4.  Aspires to fully transparent and reproducible research of highest
    standard.

As a community resource, we are very keen for people to contribute. For
more information on how you can contribute see
[Contributing](docs/Contributing.md).

## Acknowledgements

ARDC **Funding**: The project is supported by the [Australian Research
Data Commons (ARDC)](https://ardc.edu.au). …..

In addition to any general license applying to the dataset, any usage
requires acknowledgement of the following institutions permitting
inclusion of specific resources in this compilation:

  - Barlow\_1981: Australian Biological Resources Study.
  - BRAIN\_2007: Brisbane Rainforest Action and Information Network
  - Hyland\_2003, CPRR\_2002: CSIRO’s Centre for Australian National
    Biodiversity Research
  - NTH\_2014: Northern Territory Herbarium
  - RBGK\_2014, Kew\_2019\_1, Kew\_2019\_2, Kew\_2019\_3, Kew\_2019\_4,
    Kew\_2019\_5, Kew\_2019\_6: Kew Botanic Gardens
  - NHNSW\_2016, RBGSYD\_0000, RBGSYD\_2014: PLantNet, National
    Herbarium of NSW SAH\_2014: State Herbarium of South Australia
