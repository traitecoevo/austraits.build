---
output:
  github_document:
    html_preview: false
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



![](docs/logo.png)

# `AusTraits (build)`: source for curated plant trait database for the Australian flora

<!-- badges: start -->
<!-- badges: end -->


AusTraits is a transformative database, containing measurements on the
traits of Australia’s plant species, standardised from hundreds of
disconnected primary sources. So far, data have been assembled > 150
distinct sources, describing more than 100 plant traits and over 19k
species.

The AusTraits project has two connected repositories.

- [austraits.build](https://github.com/traitecoevo/austraits.build/) (this repository): Is for those constructing and contributing data to AusTraits,
- [austraits](https://github.com/traitecoevo/austraits/): For those interesting in accessing or using AusTraits.

## Usage and access

Those interested in using data from AusTraits, should visit the repositories for the compiled resource, at either: the 

- AusTraits repository on GitHub at [traitecoevo/austraits](https://github.com/traitecoevo/austraits), or 
- the versioned releases archived on Zenodo at doi: [10.5281/zenodo.3568417](http://doi.org/https://doi.org/10.5281/zenodo.3568417).

There you will also find detailed information regarding appropriate use of AusTraits. 

## We're always rebuilding

This repository ([austraits.build](https://github.com/traitecoevo/austraits.build/)) contains the raw data and code used to compile AusTraits from diverse, original sources.

To handle the harmonising of diverse data sources, we use a reproducible workflow to implement the various changes required for each source to reformat it suitable for incorporation in AusTraits. Such changes include restructuring datasets, renaming variables, changing variable units, changing taxon names. For the sake of transparency and continuing development, the entire workflow is made  available here. 

AusTraits is continually evolving, as new datasets are contributed. As 
such, there is no single canonical version. We are continually making 
new versions available. Overtime, we expect that different versions will be released and used in different analyses. 

![](docs/Workflow.png)

## Contributing

We envision AusTraits as an on-going collaborative community resource
that:

1.  Increases our collective understanding the Australian flora; and
2.  Facilitates accumulating and sharing of trait data;
3.  Builds a sense of community among contributors and users; and
4.  Aspires to fully transparent and reproducible research of highest
    standard.

As a community resource, we are very keen for people to contribute. For more information on how you can contribute see [here](docs/Contributing.md).

For anyone working with the [austraits.build](https://github.com/traitecoevo/austraits.build/) repository, detailed documentation is available on:

- Overview of Austraits from [Falster et al 2020](XXXX)
- [Building Austraits](docs/Building.md)
- [Contributing](docs/Contributing.md)
- [Working with our GitHub repository](docs/Working_with_github.md)
- [Tips & Tricks for AusTraits developers](docs/TipTricks.md), and
- [Full list of trait definitions](docs/Trait_definitions.md)

## Acknowledgements

**Funding**: The project is supported by the [Australian Research Data
Commons (ARDC)](https://ardc.edu.au). .....



In addition to any general license applying to the dataset, any usage requires acknowledgement of the following institutions permitting inclusion of specific resources in this compilation:  

- Barlow_1981: Australian Biological Resources Study.
- BRAIN_2007: Brisbane Rainforest Action and Information Network
- Hyland_2003, CPRR_2002: CSIRO's Centre for Australian National Biodiversity Research
- NTH_2014: Northern Territory Herbarium
- RBGK_2014, Kew_2019_1, Kew_2019_2, Kew_2019_3, Kew_2019_4, Kew_2019_5, Kew_2019_6: Kew Botanic Gardens
- NHNSW_2016, RBGSYD_0000, RBGSYD_2014: PLantNet, National Herbarium of NSW
SAH_2014: State Herbarium of South Australia

