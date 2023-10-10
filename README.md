
# `austraits.build`: source for `AusTraits`

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3568417.svg)](https://doi.org/10.5281/zenodo.3568417)
[![build](https://github.com/traitecoevo/austraits.build/actions/workflows/check-build.yml/badge.svg)](https://github.com/traitecoevo/austraits.build/actions/workflows/check-build.yml)
<!-- badges: end -->

![](inst/figures/logo.png)

AusTraits is a transformative database, containing measurements on the traits of Australia’s plant species, standardised from hundreds of disconnected primary sources. So far, data have been assembled \> 300 distinct sources, describing > 500 plant traits for > 25k taxa. The dataset and approach is documented in detail in the following publication

> Falster D, Gallagher R, Wenk, E et al. (2021) AusTraits, a curated plant trait database for the Australian flora. Scientific Data 8: 254. DOI: [10.1038/s41597-021-01006-6](http://doi.org/10.1038/s41597-021-01006-6)

The repo contains the data for rebuilding AusTraits, while the workflow to rebuild the dataset is on the [traits.build repo](https://github.com/traitecoevo/traits.build).

AusTraits is continually evolving, as new datasets are contributed. As such, there is no single canonical version. We are continually making new versions available. Overtime, we expect that different versions will be released and used in different analyses.

## Accessing data

Those interested in simply using data from AusTraits, should visit download the compiled resource from the versioned releases archived on Zenodo at DOI: [10.5281/zenodo.3568417](https://doi.org/10.5281/zenodo.3568417).

Users will want to read up on the [database structure, described in the `traits.build` manual](https://traitecoevo.github.io/traits.build-book/database_structure.html).

Definitions for the traits are described the AusTraits Plant Dictionary (APD), at

- Formalised vocabulary at <http://w3id.org/APD/>
- preprint Wenk et al 2023, DOI: [10.1101/2023.06.16.545047](https://doi.org/10.1101/2023.06.16.545047)

## Citation

Users of AusTraits are requested to cite the source publication, which documents the dataset and approach:

> Falster D, Gallagher R, Wenk, E et al. (2021) AusTraits, a curated plant trait database for the Australian flora. Scientific Data 8: 254.  DOI: [10.1038/s41597-021-01006-6](http://doi.org/10.1038/s41597-021-01006-6)

## Rebuilding AusTraits from source

This repository (`austraits.build`) contains the raw data and code used to compile AusTraits from diverse, original sources. 

To handle the harmonising of diverse data sources, we use a reproducible workflow to implement the various changes required for each source to reformat it into a form suitable for incorporation in AusTraits. Such changes include restructuring datasets, renaming variables, changing variable units, changing taxon names. For the sake of transparency and continuing development, the entire workflow is made available here.

![](inst/figures/Workflow.png)

We use the [`traits.build`](https://traitecoevo.github.io/traits.build/)  R package and workflow to harmonise  > 300 different sources into a unified dataset. The workflow is fully-reproducible and open, meaning it exposes the decisions made in the processing of data into a harmonised and curated dataset and can also be rerun by others. AusTraits is built so that the database can be rebuilt from its parts at any time. This means that decisions made along the way (in how data is transformed or encoded) can be inspected and modified, and new data can be easily incorporated.

To build the database follows these steps

***Install `traits.build`***

The first step is to install a copy of [traits.build](https://github.com/traitecoevo/austraits.build/): 

```{r, eval=FALSE, echo=TRUE}
remotes::install_github("traitecoevo/traits.build", quick = TRUE)
```
***Clone repository***

Next you need to download a copy of this repository from Github. Then open the Rstudio project, or open R into the right repo directory.

***Compile via `remake`***

One of the packages that will be installed with the `traits.build` is [`remake`](https://github.com/richfitz/remake). This package manages the compiling, and also helps streamline the amount of recompiling needed when new sources are added.

Running the following command will rebuild AusTraits and save the assembled database into an RDS file located in `export/data/curr/austraits.rds`.

```{r, eval=FALSE, echo=TRUE}
remake::make()
austraits <- readRDS("export/data/curr/austraits.rds")
```

Remake can also load the compiled dataset directly into R by calling:

```{r, eval=FALSE, echo=TRUE}
austraits <- remake::make("austraits")
```

## Contributing to AusTraits

We envision AusTraits as an ongoing collaborative community resource that:

1.  Increases our collective understanding of the Australian flora
2.  Facilitates the accumulation and sharing of trait data
3.  Builds a sense of community among contributors and users
4.  Aspires to be fully transparent and reproducible research of the highest standard.

We'd love for you to contribute to the projects. Below are some ways you can contribute:

- Contributing new data
- Improving data quality and reporting errors 
- Improving documentation
- Development of `traits.build`` workflow

For details on on how to contribute, please see the file [CONTRIBUTING.md](https://github.com/traitecoevo/austraits.build/blob/develop/.github/CONTRIBUTING.md)

The AusTraits project is released with a [Contributor Code of Conduct](https://github.com/traitecoevo/austraits.build/blob/develop/.github/CODE_OF_CONDUCT.md). By contributing to this project you agree to abide by its terms.
## Acknowledgements

**Funding**: This work was supported via the following investments:

- Investment (https://doi.org/10.47486/TD044, https:// doi.org/10.47486/DP720) from the Australian Research Data Commons (ARDC). The ARDC is funded by the National Collaborative Research Infrastructure Strategy (NCRIS).
- Fellowship from the Australian Research Council to Falster (FT160100113), Gallagher (DE170100208) and Wright (FT100100910),
- A UNSW Research Infrastructure Grant to Falster, and
- A grant from Macquarie University to Gallagher.

**Recognition**: Many people have contributed to AusTraits. A list of contributors  is provided on the on Zenodo at DOI:
    [10.5281/zenodo.3568417](https://doi.org/10.5281/zenodo.3568417).

Further information about the AusTraits project is available at the project website [austraits.org](https://austraits.org).

**Resuse**: At this stage, only the compiled AusTraits dataset is available for reuse, via Zenodo. The raw data sources provided in this repository are not available for reuse in their current form, without further discussion from data contributors.
