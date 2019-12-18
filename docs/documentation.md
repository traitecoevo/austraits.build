---
title: "Descriptor of the `AusTraits` data compilation - a curated plant trait database for the Australian flora"
author: "Daniel Falster, Rachael Gallagher, Sam Andrew, Dony Indiarto, James Lawson, Lizzy Wenk"
date: "2019-09-04"
output:
  html_document:
    keep_md: yes
    smart: no
    theme: yeti
    toc: yes
    toc_depth: 4
    toc_float:
      collapsed: true
      smooth_scroll: true
editor_options:
  chunk_output_type: console
---

<!-- hack to get indentation on 3rd level of floating TOC; see
https://stackoverflow.com/questions/46201753/rmarkdown-indentation-of-toc-items-in-html-output
 -->
<script>
$(document).ready(function() {
  $items = $('div#TOC li');
  $items.each(function(idx) {
    num_ul = $(this).parentsUntil('#TOC').length;
    $(this).css({'text-indent': num_ul * 10, 'padding-left': 0});
  });

});
</script>





# Overview

This document describes the AusTraits compilation - a compilationdict of plant traits for the Australian Flora. AusTraits harmonises data on 95 traits from 0 different sources, including field campaigns, published literature, taxonomic monographs, and individual species descriptions. Traits link to ecological strategy variation and vary in scope from physiological measures of performance (e.g. photosynthetic gas exchange, water-use efficiency) to morphological parameters (e.g. leaf size, seed size, maximum height). AusTraits contains curated and harmonised species- and genus-level observations coupled to, where available, contextual information on site properties. 

This document provides information on the structure of AusTraits and corresponds to Version 0.9.1 of the dataset. 

<!-- An overview of the actual data is provided in another document: see XXXX. -->

# Ethos and Usage

We envision AusTraits as an on-going collaborative community resource that:

1. Increases our collective understanding the Australian flora; and 
2. Facilitates accumulating and sharing of trait data;
3. Builds a sense of community among contributors and users; and
4. Aspires to fully transparent and reproducible research of highest standard.

To achieve these goals, the resource is built with the following objectives:

* The workflow for harmonising different datasets is made fully open and reproducible;
* All data are shared under a standard open license permitting reuse;
* Successive versions of the data remain available to ensure reproducibility of research;
* Contributions towards AusTraits are recognised via invitations to co-author data papers (i.e. releases of the data resource) and citation;
* Users of AusTraits are encouraged to both: provide adequate recognition via citation and also contribute towards the further development of the database.

Please note that the AusTraits project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

## License

Raw and harmonised data are made available under the Creative Commons CC-BY license 
XXXX



## Access

XXXXX

Wilkinson et al 2015 The FAIR Guiding Principles for scientific data management and stewardship http://doi.org/10.1038/sdata.2016.18


## Citation {#citations}

XXXXX

Force 11 data citation: https://www.force11.org/datacitationprinciples


## Transparency 

Prior to the development of AusTraits, data on Australian plant traits existed as a series of largely disconnected datasets collected by individual laboratories or initiatives. Our goal is to harmonise these different sources.

vignettes/assets/Workflow.png


To facilitate the sharing of trait data under the FAIR principles (Findability, Accessibility, Interoperability, and Reusability), AusTraits uses a fully-reproducible workflow which exposes the decisions made in the processing of data into a harmonised and curated dataset (Figure 1). For instance, AusTraits makes no alterations to the primary datasets provided by co-authors, instead making use of R packages such as `make` and `remake` to configure and harmonise (standardise) data into a single curated resource. Several additional quality-assurance steps are undertaken, including detailed error-checking by dataset custodians. 

Error checking is facilitated by the generation of bespoke reports for each dataset that can be compared to all observation in AusTraits and by the implementation of a series of tests in the `remake` pipeline that flag trait values that cannot be incorporated into AusTraits (e.g. constraining trait values to plausible minima and maxima; see Methods for full details). Whilst all care is taken to curate datasets in AusTraits this is not intended to absolve individual researchers from ensuring the appropriate use of the data provided. 

## Contributing

AusTraits is a community resource and we are very keen for people to contribute. Here are some of the ways you can contribute: 

**Reporting Errors**: If you notice a possible error in AusTraits, please [post an issue on GitHub](https://github.com/traitecoevo/austraits/issues) (not available until dataset is made public) or send us an email, describing the error and possible fix in detail. If you can, please provide code illustrating the problem.

**Refining documentation:** Data contributors and data users who are less familiar with the AusTraits format and code than the custodians may determine that important descriptions or steps are omitted from this documentation file. We welcome additions and edits that make using the existing data or adding new data easier for the community.

**Value adding / expanding existing data**: If you would like to value-add to AusTraits in some other way, please get in contact by [posting an issue](https://github.com/traitecoevo/austraits/issues) with an idea or offer of time.

**Contributing new data**: We gladly accept new data contributions to AusTraits. In the future we hope to publish a data paper, including contributors as co-authors on the article. If you would like to contribute data, the requirements are:

- Data were collected for Australian plant species growing in Australia
- You collected data on one of the traits list in the [trait definitions table](#trait_defs).
- You are willing to release the data under an open license for reuse by the scientific community.
- You make it is as easy as possible for us to incorporate your data by carefully following the instructions below.

For full instructions on preparing data for inclusion in AusTraits see the sections below on [formatting of raw data files for AusTraits](#format).


## Loading data

There are several ways to access the Austraits data, including 

- downloading plain text files and loading these into R manually
- using the `austraits` R package to fetch and load particular versions of the data (not yet available)
- rebuilding the Austraits data from scratch (see instructions above)
- downloading the compressed `austraits.rds` binary and loading this directly into R.

These instructions are written assuming you have followed the latter approach. 

Let's assume you have a variable `path` with the directory of your data. You can then load the Austraits data package using the `readRDS` package  



You should now have a object called `austraits` with the following elements:

[1] "traits"        "sites"         "methods"       "excluded_data"
[5] "taxonomy"      "definitions"   "sources"       "build_info"   

These names correspond to the sections described in full above under ["elements of austraits"](#elements_of_austraits). 

The trait data are in the element called data


```
## # A tibble: 389,875 x 10
##    dataset_id species_name site_name observation_id trait_name value unit 
##    <chr>      <chr>        <chr>     <chr>          <chr>      <chr> <chr>
##  1 ANBG_2018  Abelmoschus… <NA>      ANBG_2018_0001 seed_shape sphe… <NA> 
##  2 ANBG_2018  Abrophyllum… <NA>      ANBG_2018_0002 seed_mass  46.3… mg   
##  3 ANBG_2018  Abrotanella… <NA>      ANBG_2018_0003 seed_mass  0.31… mg   
##  4 ANBG_2018  Abutilon in… <NA>      ANBG_2018_0004 seed_mass  4.022 mg   
##  5 ANBG_2018  Abutilon ha… <NA>      ANBG_2018_0005 seed_mass  3.56… mg   
##  6 ANBG_2018  Abutilon ot… <NA>      ANBG_2018_0006 seed_shape cord… <NA> 
##  7 ANBG_2018  Acacia abbr… <NA>      ANBG_2018_0007 seed_mass  6.15  mg   
##  8 ANBG_2018  Acacia acin… <NA>      ANBG_2018_0008 dispersal… aril  <NA> 
##  9 ANBG_2018  Acacia acin… <NA>      ANBG_2018_0008 seed_mass  7.313 mg   
## 10 ANBG_2018  Acacia acin… <NA>      ANBG_2018_0008 seed_shape glob… <NA> 
## # … with 389,865 more rows, and 3 more variables: value_type <fct>,
## #   replicates <chr>, original_name <chr>
```

(Note that data appears as a `tibble` rather than a `data.frame`. Tibbles are an enhanced type dataframe, with some desirable features, such as not printing the whole dataframe to screen. (If you prefer, you can easily use a traditional R `data.frame` by converting it using `df <- data.frame(austraits$traits)`. But from here on we'll assume you're sticking with a tibble.)


# Structure of AusTraits {#structure}

(More here)

It is essential that users of AusTraits data are confident the data have the meaning they expect it to and were collected using methods they trust. As such, each dataset within Austraits must include descriptions of the study, sites, and methods used as well as the data itself.

(More here)

The Extensible Observation Ontology (OBOE) is a formal ontology for capturing the semantics of scientific observation and measurement. The ontology supports researchers to add detailed semantic annotations to scientific data, thereby clarifying the inherent meaning of scientific observations. 

> Mark Schildhauer, Matthew B. Jones, Shawn Bowers, Joshua Madin, Sergeui Krivov, Deana Pennington, Ferdinando Villa, Benjamin Leinfelder, Christopher Jones, and Margaret O'Brien. 2016. OBOE: the Extensible Observation Ontology, version 1.1. KNB Data Repository. [doi:10.5063/F11C1TTM](http://doi.org/10.5063/F11C1TTM)

The original publication describing the OBOE concept is:

> Madin, J., S. Bowers, M. Schildhauer, S. Krivov, D. Pennington, and F. Villa. 2007. An ontology for describing and synthesizing ecological observation data. Ecological Informatics 2:279–296. [doi:10.1016/j.ecoinf.2007.05.004](http://doi.org/10.1016/j.ecoinf.2007.05.004)

Source code on Github: [NCEAS/oboe](https://github.com/NCEAS/oboe)

(More here)

## Elements of AusTraits {#elements}

The compiled AusTraits database has the following main components:



```
austraits
├── traits
├── sites
├── methods
├── excluded_data
├── taxonomy
├── definitions
└── build_info
```

These elements include all the data and contextual information submitted with each contributed dataset, but are not in the same format as data contributions. See [Format of raw data files for AusTraits](#format) for how to format data to contribute.

Each component is the above table is defined as follows:


### traits {#traits}

**Description:** A table containing measurements of plant traits

**Content:** 

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left:30px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> key </th>
   <th style="text-align:left;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> dataset_id </td>
   <td style="text-align:left;"> Primary identifier for each study contributed into AusTraits; most often these are scientific papers, books, or online resources. By default should be name of first author and year of publication, e.g. `Falster_2005`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> species_name </td>
   <td style="text-align:left;"> Name of species after aligning taxonomy with standard sources. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> site_name </td>
   <td style="text-align:left;"> Name of site where species was sampled. Cross-references between similar columns in context and data. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> observation_id </td>
   <td style="text-align:left;"> A unique identifier for the observation, useful for joining traits coming from the same observation_id. Observation ids are assigned automatically, based on `dataset_id` and row number in the file `data.csv`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trait_name </td>
   <td style="text-align:left;"> Name of trait sampled. Allowable values specified in the table `traits`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> value </td>
   <td style="text-align:left;"> Value of trait sampled. In the master dataset these are all stored as characters, then converted to numeric later where appropriate. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> unit </td>
   <td style="text-align:left;"> Units of the sampled trait value after aligning with AusTraits standards. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> value_type </td>
   <td style="text-align:left;"> A categorical variable describing the type of trait value recorded </td>
  </tr>
  <tr>
   <td style="text-align:left;"> replicates </td>
   <td style="text-align:left;"> Number of replicate measurements that comprise the data points for the trait for each measurement. A numeric value (or range) is ideal and appropriate if the value type is a `mean`, `median`, `min` or  `max`. For these value types, if replication is unknown the entry should be `unknown`. If the value type is `raw_value` the replicate value should be `1`. If the value type is `expert_mean`, `expert_min`, or `expert_max` the replicate value should be `.na`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> original_name </td>
   <td style="text-align:left;"> Taxonomic name given to species in the original data supplied by the authors </td>
  </tr>
</tbody>
</table>

### sites {#sites}

**Description:** A table containing observations of site characteristics associated with information in `data`. Cross referencing between the two dataframes is possible using combinations of the variables `dataset_id`, `site_name`.

**Content:** 

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left:30px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> key </th>
   <th style="text-align:left;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> dataset_id </td>
   <td style="text-align:left;"> Primary identifier for each study contributed into AusTraits; most often these are scientific papers, books, or online resources. By default should be name of first author and year of publication, e.g. `Falster_2005`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> site_name </td>
   <td style="text-align:left;"> Name of site where species was sampled. Cross-references between similar columns in context and data. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> site_property </td>
   <td style="text-align:left;"> The site characteristic being recorded. Name should include units of measurement, e.g. `longitude (deg)`. Ideally we have at least these variables for each site - `longitude (deg)`, `latitude (deg)`, `description`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> value </td>
   <td style="text-align:left;"> Value of trait sampled. In the master dataset these are all stored as characters, then converted to numeric later where appropriate. </td>
  </tr>
</tbody>
</table>

### methods {#methods}

**Description:** A table containing details on methods with which data were collected, including time frame and source

**Content:** 

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left:30px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> key </th>
   <th style="text-align:left;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> dataset_id </td>
   <td style="text-align:left;"> Primary identifier for each study contributed into AusTraits; most often these are scientific papers, books, or online resources. By default should be name of first author and year of publication, e.g. `Falster_2005`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trait_name </td>
   <td style="text-align:left;"> Name of trait sampled. Allowable values specified in the table `traits`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> methods </td>
   <td style="text-align:left;"> A textual description of the methods used to collect the trait data. Whenever available, methods are taken near-verbatim from referenced souce. Methods can include descriptions such as 'measured on herbarium specimens','data from the literature', or a detailed description of the field or lab methods used to collect the data. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> year_collected_start </td>
   <td style="text-align:left;"> The year data collection commenced </td>
  </tr>
  <tr>
   <td style="text-align:left;"> year_collected_end </td>
   <td style="text-align:left;"> The year data collection was completed </td>
  </tr>
  <tr>
   <td style="text-align:left;"> description </td>
   <td style="text-align:left;"> A 1-2 sentence description of the purpose of the study. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> collection_type </td>
   <td style="text-align:left;"> A field to indicate where the plants on which traits were measured were collected -  in the `field`, `lab`, `glasshouse`, `herbarium specimens`, or `literature`. The latter should only be used when the data were sourced from the literature and the collection type is unknown. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sample_age_class </td>
   <td style="text-align:left;"> A field to indicate if the study was completed on `adult` or `juvenile` plants </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sampling_strategy </td>
   <td style="text-align:left;"> A written description of how study sites were selected and how study individuals were selected. When available, this information is lifted verbatim from a published manuscript. For herbarium studies, this field ideally indicates which herbaria were 'sampled' to measure a specific trait </td>
  </tr>
  <tr>
   <td style="text-align:left;"> source_primary_citation </td>
   <td style="text-align:left;"> Citation for primary source. This detail is genearted from the primary source in the metadata </td>
  </tr>
  <tr>
   <td style="text-align:left;"> source_primary_key </td>
   <td style="text-align:left;"> Citation key for primary source in `sources`. The key is typically of format Surname_year </td>
  </tr>
  <tr>
   <td style="text-align:left;"> source_secondary_citation </td>
   <td style="text-align:left;"> Citation for secondary source. This detail is genearted from the secondary source in the metadata </td>
  </tr>
  <tr>
   <td style="text-align:left;"> source_secondary_key </td>
   <td style="text-align:left;"> Citation key for secondary source in `sources`.  The key is typically of format Surname_year </td>
  </tr>
</tbody>
</table>

### excluded_data {#excluded_data}

**Description:** A table of data that did not pass quality test and so were excluded from the master dataset

**Content:** Structure is identical to that presented in the `data` table, only with an extra column called `error` indicating why the record was excluded


### taxonomy {#taxonomy}

**Description:** A table containing details on species taxonomy associated with information in `data`. Cross referencing between the two dataframes is possible using the variable `species_name`. We have attempted to align species names with known taxonomic units, focussing primarily on the [`The Plant List` (TPL)](http://www.theplantlist.org/) -- a global working list of all known plant species. In addition we have tried to align these names with the [`Australian Plant Census` (APC)](https://biodiversity.org.au/nsl/services/apc) and the [`Australian Plant Names Index` (APNI)](https://biodiversity.org.au/nsl/services/APNI).

**Content:** 

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left:30px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> key </th>
   <th style="text-align:left;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> species_name </td>
   <td style="text-align:left;"> Name of species after aligning taxonomy with standard sources. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> family </td>
   <td style="text-align:left;"> Family of aligned species_name </td>
  </tr>
  <tr>
   <td style="text-align:left;"> authority </td>
   <td style="text-align:left;"> Authority for aligned species_name </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TPL_ID </td>
   <td style="text-align:left;"> ID of species in the TPL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> status </td>
   <td style="text-align:left;"> Status of species in TPL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> APC_name </td>
   <td style="text-align:left;"> Name of species in APC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> APC_ID </td>
   <td style="text-align:left;"> ID of species in the APC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> APNI_ID </td>
   <td style="text-align:left;"> ID of species in the APNI </td>
  </tr>
</tbody>
</table>

### definitions {#definitions}

**Description:** A copy of the definitions for all tables and terms. Information included here was used to process data and generate any documentation for the study.

**Content:** A structured yaml file, represented as a list in R. See file `config/definitions.yaml` for more details.


### sources {#sources}

**Description:** Bibtex entries for all primary and secondary sources in the compilation


### build_info {#build_info}

**Description:** Description of the computing environment used to create this version of the dataset, including version number, git commit and R session_info

## Observation IDs

Each trait measurement has an associated `observation_id`. Observation IDs bind together related measurements within a dataset. For example, if multiple traits were collected on the same individual, the `observation_id` allows us to gather these together. For floras, which report a species averages, the `observation_id` is determined via the species name. Importantly, the `observation_id` allows translation between long  (e.g. with variables `trait_name` and `value`) and wide (e.g. with traits as columns) formats.

Generally, `observation_id` has the format `dataset_id_XX` where `XX` is a unique number within each dataset.

For datasets that arrive in wide format we assume each row has a unique `observation_id`. 

For datasets that arrive in long format, the `observation_id` is assigned based on a specified grouping variable. This variable can be specified in the `metadata.yml` file under the section `variable_match`. If missing, `observation_id` is assigned based on `species_name`. 

## Values and Value types {#value_types}

Each record in the table of [trait data](#traits) has an associated `value` and `value_type`. Traits are either `numeric` or `categorical`.

For traits with numerical values, the recorded value has been converted into standardised units and we have check that the value can be converted into a number and lies within the allowable range. 

For categorical variables, we only include records that are defined in the definitions (see [trait definitions below](#traits)). Moreover, we use a format whereby


- we use `_` for multi-word terms, e.g. `semi_deciduous`
- use a space for situations where there are two possible values for that trait, e.g. `annual biennial` for something which is either annual or biennial 

Each trait measurement also an associated `value_type`, which gives `A categorical variable describing the type of trait value recorded`. Possible value_types are:

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left:30px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> key </th>
   <th style="text-align:left;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> raw_value </td>
   <td style="text-align:left;"> Value is a direct measurement </td>
  </tr>
  <tr>
   <td style="text-align:left;"> site_min </td>
   <td style="text-align:left;"> Value is the minimum of measurements on multiple individuals of the species at a single site </td>
  </tr>
  <tr>
   <td style="text-align:left;"> site_mean </td>
   <td style="text-align:left;"> Value is the mean or median of measurements on multiple individuals of the species at a single site </td>
  </tr>
  <tr>
   <td style="text-align:left;"> site_max </td>
   <td style="text-align:left;"> Value is the maximum of measurements on multiple individuals of the species at a single site </td>
  </tr>
  <tr>
   <td style="text-align:left;"> multisite_min </td>
   <td style="text-align:left;"> Value is the minimum of measurements on multiple individuals of the species across multiple sites </td>
  </tr>
  <tr>
   <td style="text-align:left;"> multisite_mean </td>
   <td style="text-align:left;"> Value is the mean or median of measurements on multiple individuals of the species across multiple sites </td>
  </tr>
  <tr>
   <td style="text-align:left;"> multisite_max </td>
   <td style="text-align:left;"> Value is the maximum of measurements on multiple individuals of the species across multiple sites </td>
  </tr>
  <tr>
   <td style="text-align:left;"> expert_min </td>
   <td style="text-align:left;"> Value is the minimum observed for a species across its entire global range, as estimated by an expert based on their knowledge of the species. The value has not been measured directly. Data from herabarium studies that represent a species' entire range fit in this category. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> expert_mean </td>
   <td style="text-align:left;"> Value is the average observed for a species across its entire global range, as estimated by an expert based on their knowledge of the species. The value has not been measured directly. Data from herabarium studies that represent a species' entire range fit in this category. Categorical variable values obtained from a reference book or identified by an expert also have this value type. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> expert_max </td>
   <td style="text-align:left;"> Value is the maximum observed for a species across its entire global range, as estimated by an expert based on their knowledge of the species. The value has not been measured directly. Data from herabarium studies that represent a species' entire range fit in this category. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> experiment_min </td>
   <td style="text-align:left;"> Value is the minimum of measurements from an experimental study either in the field or a glasshouse </td>
  </tr>
  <tr>
   <td style="text-align:left;"> experiment_mean </td>
   <td style="text-align:left;"> Value is the mean or median of measurements from an experimental study either in the field or a glasshouse </td>
  </tr>
  <tr>
   <td style="text-align:left;"> experiment_max </td>
   <td style="text-align:left;"> Value is the maximum of measurements from an experimental study either in the field or a glasshouse </td>
  </tr>
  <tr>
   <td style="text-align:left;"> unknown </td>
   <td style="text-align:left;"> Value type is not currently known </td>
  </tr>
</tbody>
</table>

## Species taxonomy {#taxonomy}

Within AusTraits there are records for 21091 different species. A full list of all known species is available in the table `taxonomy` (see details above).

We have attempted to align species names with known taxonomic units, focussing primarily on the [`The Plant List` (TPL)](http://www.theplantlist.org/) -- a global working list of all known plant species. In addition we have tried to align these names with the [`Australian Plant Census` (APC)](https://biodiversity.org.au/nsl/services/apc) and the [`Australian Plant Names Index` (APNI)](https://biodiversity.org.au/nsl/services/APNI). The `APNI_ID` can also be used to access relevant records for the species in the [`Atlas of Living Australia`](https://bie.ala.org.au/) (ALA).

Links to species records in these systems can be accessed via the relevant IDs as in these examples. 

- The Plant List: [http://www.theplantlist.org/tpl1.1/record/kew-450649](http://www.theplantlist.org/tpl1.1/record/kew-450649) where `kew-450649` is the `TPL_ID`
- Australian Plant Census: [https://biodiversity.org.au/nsl/services/node/apc/2908862](https://biodiversity.org.au/nsl/services/node/apc/2908862) where `2908862` is the `APC_ID`
- Australian Plant Names Index: [http://id.biodiversity.org.au/node/apni/2899106](http://id.biodiversity.org.au/node/apni/2899106
) where `2899106` is the `APNI_ID`
- Atlas of Living Australia: [https://bie.ala.org.au/species/http://id.biodiversity.org.au/node/apni/2899106](https://bie.ala.org.au/species/http://id.biodiversity.org.au/node/apni/2899106) where `2899106` is the `APNI_ID`.

## Sources

For each dataset in the compilation there is the option to list a primary and secondary citation. The primary citation The original study in which data were collected while the secondary citation is A subsequent study where data were compiled or re-analysed and then made available. these references are included in two places:

1. Within the table [methods](#methods), where we provide a formatted version of each.
2. In the element [sources](#sources), where we provided bibtex versions of all sources which can be imported into your reference library. The keys for these references are listed within the [methods]{#methods}. 

As noted above under [citations](#citations), anyone using the compilation is expected to cite primary sources. 

## Trait definitions {#trait_defs}

Below is the standard definition for each trait in AusTraits (drawn from the the file `config/definitions.yml`). As described above in the section on [Value types](#value_types), traits are labelled as either `numeric` or `categorical`.


**bark_mass_area**


- label: Bark mass per unit surface area of stem
- description: Bark mass per unit surface area of stem
- number of records: 27
- number of studies: 1
- type: numeric
- units: g/cm2
- allowable range: 0.01 - 1 g/cm2

**bark_thickness**


- label: Bark thickness
- description: Thickness of the bark of the stem
- number of records: 1054
- number of studies: 3
- type: numeric
- units: mm
- allowable range: 0.01 - 50 mm

**branch_mass_fraction**


- label: Fraction of plant dry mass comprised of branch material
- description: Fraction of plant dry mass comprised of branch material
- number of records: 43
- number of studies: 1
- type: numeric
- units: dimensionless
- allowable range: 0.01 - 1 dimensionless

**diaspore_mass**


- label: Mass of entire diaspore
- description: Mass of seed including dispersal appendages
- number of records: 249
- number of studies: 1
- type: numeric
- units: mg
- allowable range: 0.001 - 10000 mg

**dispersal_appendage**


- label: Appendage of propagule which facilitates dispersal
- description: Type of dispersal appendage present
- number of records: 1569
- number of studies: 6
- type: categorical
- allowable values:
    - *aril*: Fleshy outgrowth of a seed, that often attracts animals like birds or ants
    - *awn_bristle*: A slender, bristle-like projection, e.g. from the back or tip of the glumes and lemmas in some grasses and on the fruit of some other species.
    - *barbs*: A rear-facing point, as in a fish hook that aid in seed and fruit dispersal. Includes "hooks", a term used in Ranunculus
    - *beak*: Name of dispersal appendage in the genus Carex
    - *bladdery_wings*: Maybe referring to the wings present on the pollen grains of some conifers to aid in their dispersal by wind
    - *bract*: When the bracts below the inflorescence are the dispersal appendage. For the family Poaceae "bract" refers to the glumes, the modified membranous bracts surrounding the spikelet of a grass.
    - *caruncle*: A type of eliasome, term especially used in the family Euphorbiaceae; an outgrowth or appendage at or near the hilum of certain seeds that aids in dispersal. Includes "strophiole"
    - *drupe*: Fruit with membranous exocarp, fleshy mesocarp and hard endocarp
    - *elaiosome*: Fleshy (often fatty) appendage on seeds that attracts ants
    - *fleshy_dehiscent_capsule*: A fleshy fruit that splits open and releases seeds at maturity; fruit type
    - *fleshy_fruit*: A fruit where part or all of the pericarp (fruit wall) is fleshy at maturity; fruit type
    - *fleshy_wings_capsule*: A fruit whose capsule or wings are fleshy
    - *floral_parts*: When a plant's floral parts, the petals and/or sepals, are persistent and aid in seed and fruit dispersal
    - *funicle*: The stalk that joins the seed to the pod frame
    - *hairs*: When modified hairs aid in seed dispersal
    - *indehiscent*: Fruit that does not open at maturity in a pre-defined way, but instead relies on predation or decomposition to release the seeds;fruit type
    - *inflated_parts*: When some part of the seed, fruit, or associated tissues is inflated, aiding in seed or fruit dispersal
    - *none*: When a fruit and associated tissues lack any dispersal appendages
    - *paddles*: unknown meaning; currently used only for "Brachyscome dentata"
    - *pappus*: The pappus is the modified calyx, the part of an individual floret, that surrounds the base of the corolla tube in flower heads of the plant family Asteraceae
    - *placental_endocarp*: Exact dispersal mechanism unclear; currently used only for "Crowea saligna"
    - *receptacle*: UNKNOWN
    - *samara*: A winged achene, a type of fruit in which a flattened wing of fibrous, papery tissue develops from the ovary wall
    - *sarcotesta*: Fleshy seed coat; term especially used for cycads; dispersal appendage?
    - *scales*: When scales assist in seed or fruit dispersal
    - *seed_airsac*: Seed containing a sac of air
    - *seed_unilaterally_winged*: A seed wing that arises from one side; dispersal appendage
    - *seed_wing_obsolete*: Seed wing absent
    - *spines*: Spines that aid in seed and fruit dispersal
    - *strophiole_on_seed*: Outgrowth of the hilum region which restricts water movement into and out of some seeds, especially legumes
    - *style*: Cases where a species' style, stigma or carpel are persistent and aid in seed or fruit dispersal
    - *winged_fruit*: A fruit where some tissue layers have become flattened and papery to function as wings; includes reference to a winged nut and winged achene (samara); fruit type
    - *wings*: Referring to wing-like seed extensions that aid in wind dispersal
    - *woody_bract_scales*: UNSURE; assume it refers to conifers, where woody bracts protect the seeds


**dispersal_syndrome**


- label: Dispersal syndrome
- description: Type of dispersal syndrome displayed by species, although the list includes many dispersal appendages and fruit types
- number of records: 2006
- number of studies: 17
- type: categorical
- allowable values:
    - *adhesion*: Dispersal syndrome where fruit is transported by attaching itself to something, usually an animal; true dispersal syndrome; will overlap with exozoochory
    - *anemochory*: Dispersal by wind; true dispersal syndrome
    - *animal_vector*: Seeds are dispersed by an animal; true dispersal syndrome
    - *aril*: Fleshy outgrowth of a seed, that often attracts animals like birds or ants; dispersal appendage
    - *ballistic*: Seeds are launched away from the plant by explosion as soon as the seed capsule opens; true dispersal syndrome
    - *elaiosome*: Fleshy (often fatty) appendage on seeds that attracts ants; dispersal appendage
    - *endozoochory*: Diaspores that dispersed through ingestion by animals and are transported before dropping off; includes the term ingestion; true dispersal syndrome
    - *exarillate*: A seed that lacks an aril; fruit type or dispersal appendage but not a dispersal syndrome
    - *exozoochory*: When seeds are dispersed on the surface of an animal; will overlap significantly with the terms adhesion and animal_vector
    - *fleshy_fruit*: A fruit where part or all of the pericarp (fruit wall) is fleshy at maturity; fruit type
    - *fleshy_wings_capsule*: A fruit whose capsule or wings are fleshy
    - *hydrochory*: Dispersal on the surface of water; see also the term water; true dispersal syndrome
    - *mobile*: UNKNOWN
    - *myrmecochory*: Dispersules with elaiosomes (specialised nutritious appendages) that make them attractive for capture, transport and use by ants or related insects; true dispersal syndrome
    - *samara*: A winged achene, a type of fruit in which a flattened wing of fibrous, papery tissue develops from the ovary wall; fruit type; only used in TMAG_2009 for Allocasuarina species - maybe change to wind
    - *unassisted*: Seeds are dispersed without assistance; true dispersal syndrome
    - *undefined*: UNKNOWN
    - *vertebrate*: Seeds are dispersed by a vertebrate species; true dispersal syndrome
    - *water*: Seeds dispersal depends in some way on water
    - *wind*: Seeds are dispersed by wind; true dispersal syndrome


**fire_response**


- label: Resprouts or is killed by fire
- description: Distinguishes between plants that are killed by fire and resprout following fire
- number of records: 2967
- number of studies: 7
- type: categorical
- allowable values:
    - *fire_killed*: Plants killed by hot fires
    - *weak_resprouting*: Plant shows weak resprouting following fire
    - *intermediate_resprouting*: Plant shows intermediate resprouting following fire
    - *strong_resprouting*: Plant shows strong resprouting following fire
    - *resprouts*: Plants resprout from underground storage organ following fire. (For studies that don't differentiate between respouting strength)


**fire-cued_seeding**


- label: Fire-cued seeding
- description: Distinguishes between plants that do and do not have fire-cued seeding
- number of records: 2418
- number of studies: 1
- type: categorical
- allowable values:
    - *fire-cued_seeding*: Plants that germinate robustly following fire
    - *no_fire-cued_seeding*: Plants that do not show increased seeding following fire


**flower_colour**


- label: Flower colour
- description: Flower colour, with six possible outcomes
- number of records: 8633
- number of studies: 1
- type: categorical
- allowable values:
    - *blue_purple*: blue or purple
    - *green*: green flower
    - *pink*: pink flower
    - *red_brown*: red or brown flower
    - *white_cream*: white or cream flower
    - *yellow_orange*: yellow orange flower


**flowering_time**


- label: Range of flowering period
- description: Months during which species is flowering; keyed as a sequences of 12 0s (not flowering) and 1s (flowering) starting with January
- number of records: 17398
- number of studies: 27
- type: character
- allowable values:
    - **: 


**fruit_width**


- label: Fruit diameter
- description: The longest width dimension of a fruit; orthogonal to the length
- number of records: 501
- number of studies: 3
- type: numeric
- units: mm
- allowable range: 1 - 100 mm

**fruit_length**


- label: Fruit length
- description: Longest fruit dimension or if clearly recognizable the length from its base to its apex
- number of records: 622
- number of studies: 3
- type: numeric
- units: mm
- allowable range: 1 - 1000 mm

**fruit_type**


- label: Fruit type
- description: Fruit types
- number of records: 4484
- number of studies: 2
- type: categorical
- allowable values:
    - *achene*: A small, dry, indehiscent one-seeded fruit with a thin wall
    - *anthocarp_nut*: A  false fruit consisting of the true fruit and the base of the floral whorls; instance where the true fruit is a nut
    - *anthocarp_viscid*: UNCERTAIN what the viscid refers to (See anthocarp above)
    - *berry*: Fleshy, indehiscent fruit in which the seeds are generally more than 1 and are not encased in a stone
    - *berry_inf*: UNCERTAIN how this differs from general definition for berry
    - *capsule*: Dry fruit from compound pistil, nearly always dehiscent
    - *capsule_explosive*: Dry fruit from compound pistil, that releases its seeds in an explosive manner
    - *capsule_aril*: Dry fruit from compound pistil, with a fleshy outgrowth (aril) to attract pollinators
    - *capsule_wing*: Dry fruit from compound pistil, with a wing for dispersal by wind
    - *cypsela*: same as achene
    - *drupe*: Fleshy or pulpy, indehiscent, superficially berry-like fruit in which 1 seed is encased in a stone or more than 1 seed is encased in an equal number of free or variously fused stones
    - *drupe_fibrous*: Fibrous or dry drupe; indehiscent, superficially berry-like fruit in which 1 seed is encased in a stone or more than 1 seed is encased in an equal number of free or variously fused stones
    - *follicle*: Dry fruit from a simple pistil, dehiscent on generally only one side, along a single suture
    - *legume*: Fruits from a legume; a dry or somewhat fleshy, 1- to many-seeded fruit from a simple pistil, typically dehiscent longitudinally along two sutures and splitting into halves that remain joined at the base
    - *legume_indehiscent*: Fruits from a legume; a legume (see above) that is indehiscent
    - *nut*: Mostly dry, sometimes fleshy or pulpy, usually indehiscent fruit in which a single seed is encased in a hard shell
    - *nut_winged*: A nut that is enclosed in a winglike bract
    - *nutlet*: Small, dry nut or nut-like fruit, usually several of which are produced by a single flower; especially an achene
    - *pod*: A legume or superficially similar fruit
    - *samara*: A winged achene, a type of fruit in which a flattened wing of fibrous, papery tissue develops from the ovary wall
    - *schizocarp*: A dry fruit which splits into individual carpels
    - *syconium*: A fleshy hollow receptacle that develops into a multiple fruit, as in the fig
    - *syncarp*: A multiple fruit consisting of several united fruits, originating from several originally free carpels, usually fleshy
    - *utricle*: Small, bladderlike, thin-walled indehiscent fruit
    - *utricle_fleshy*: Small, bladderlike, thin-walled indehiscent fruit, that is partially fleshy
    - *utricle_spiny*: Small, bladderlike, thin-walled indehiscent fruit, that is spiny


**fruit_type_botany**


- label: Fruit type (fleshy / dry)
- description: Binary variable, dividing fruits into 'dry' versus 'fleshy' based on their appearance
- number of records: 4044
- number of studies: 1
- type: categorical
- allowable values:
    - *dry*: fruit whose appearance is dry
    - *fleshy*: fruit whose appearance is fleshy


**fruit_type_function**


- label: Fruit type (fleshy / dry)
- description: Binary variable, dividing fruits into 'dry' versus 'fleshy' based on their function
- number of records: 4126
- number of studies: 2
- type: categorical
- allowable values:
    - *dry*: fruit that are functionally dry
    - *fleshy*: fruit that are functionally fleshy


**fruiting_time**


- label: Plant reproductive phenology timing (fruiting)
- description: Months during which species is fruiting; keyed as a sequences of 12 0s (not flowering) and 1s (flowering) starting with January
- number of records: 1144
- number of studies: 4
- type: character
- allowable values:
    - **: 


**genome_size**


- label: Plant genome size
- description: Mass of the plant's genome
- number of records: 92
- number of studies: 1
- type: numeric
- units: pg
- allowable range: 1 - 10 pg

**glaucous**


- label: Glaucous
- description: Variable indicating if a plant's leaves are glaucous or not
- number of records: 5
- number of studies: 1
- type: categorical
- allowable values:
    - *0*: not glaucous
    - *1*: glaucous


**growth_habit**


- label: Growth habit (data actually describes plant vegetative reproduction capacity)
- description: Variable that defines a combination of growth habit and plant vegetative reproductive potential
- number of records: 245
- number of studies: 4
- type: categorical
- allowable values:
    - *prostrate*: Plants that lie flat on the ground
    - *rhizomatous*: Plants having modified underground stems
    - *stoloniferous*: Plants having horizontal branches from the base of the plant that produce new plants from buds at its tip or nodes
    - *tufted*: A dense clump attached at the base or growing close together; can refer to grasses, shrubs, or trees


**leaf_area**


- label: Leaf area
- description: Area of the leaf surface
- number of records: 12446
- number of studies: 36
- type: numeric
- units: mm2
- allowable range: 0.1 - 1e+07 mm2

**leaf_area_per_sapwood_area**


- label: Leaf area per unit sapwood area
- description: Ratio of leaf area to sapwood area
- number of records: 119
- number of studies: 1
- type: numeric
- units: m2/m2
- allowable range: 100 - 1e+05 m2/m2

**leaf_area_ratio**


- label: Leaf area per unit plant dry mass
- description: Ratio of leaf area to total plant dry mass
- number of records: 39
- number of studies: 3
- type: numeric
- units: mm2/mg
- allowable range: 1 - 100 mm2/mg

**leaf_arrangement**


- label: Arrangement of leaves
- description: Describes leaf arrangement on the stem
- number of records: 5983
- number of studies: 1
- type: categorical
- allowable values:
    - *alternate*: A single leaf is attached at each stem node
    - *opposite*: Leaves attach to the stem in pairs
    - *reduced_to_scales*: Plant lacks true leaves. Leaves are instead small scales
    - *whorled_tufted_crowded*: Three or more leaves are attached at each node


**leaf_C_per_dry_mass**


- label: Leaf carbon (C) content per unit leaf dry mass
- description: Leaf carbon (C) content per unit leaf dry mass
- number of records: 270
- number of studies: 3
- type: numeric
- units: mg/g
- allowable range: 100 - 1000 mg/g

**leaf_cell_wall_fraction**


- label: Leaf cell wall fraction (fraction of cell wall material recovered from total leaf biomass)
- description: Fraction of total leaf biomass that is cell wall material
- number of records: 55
- number of studies: 2
- type: numeric
- units: dimensionless
- allowable range: 0.1 - 1 dimensionless

**leaf_cell_wall_N**


- label: Leaf cell wall nitrogen concentration
- description: Proportion of leaf cell wall material that is nitrogen
- number of records: 29
- number of studies: 1
- type: numeric
- units: mmol/g
- allowable range: 0.1 - 10 mmol/g

**leaf_cell_wall_N_fraction**


- label: Leaf cell wall nitrogen fraction
- description: Proportion of all N in leaves that is found in the leaf cell walls
- number of records: 29
- number of studies: 1
- type: numeric
- units: dimensionless
- allowable range: 0.01 - 1 dimensionless

**leaf_chlorophyll_per_dry_mass**


- label: Leaf chlorophyll content per unit leaf dry mass
- description: Leaf chlorophyll content per unit leaf dry mass
- number of records: 213
- number of studies: 1
- type: numeric
- units: umol/g
- allowable range: 1 - 10 umol/g

**leaf_CN_ratio**


- label: Leaf carbon/nitrogen (C/N) ratio
- description: Leaf carbon/nitrogen (C/N) ratio
- number of records: 56
- number of studies: 1
- type: numeric
- units: g/g
- allowable range: 10 - 1000 g/g

**leaf_compoundness**


- label: Leaf compoundness
- description: Indicates whether or not a leaf is compound; different 'simple' terminology used by different studies
- number of records: 16985
- number of studies: 18
- type: categorical
- allowable values:
    - *compound*: A leaf that is divided into multiple leaflets
    - *leafless*: A stem that lacks leaves
    - *lobed*: A leaf that is lobed but not compound
    - *palmate*: A leaf that is palmately compound
    - *simple*: A leaf with a single undivided blade


**leaf_dark_respiration_per_area**


- label: Leaf respiration rate per unit leaf area
- description: Leaf respiration rate per unit leaf area
- number of records: 267
- number of studies: 5
- type: numeric
- units: umolCO2/m2/s
- allowable range: 1e-05 - 10000 umolCO2/m2/s

**leaf_dark_respiration_per_dry_mass**


- label: Leaf respiration rate per unit leaf dry mass
- description: Leaf respiration rate per unit leaf dry mass
- number of records: 128
- number of studies: 2
- type: numeric
- units: umolCO2/g/s
- allowable range: 1e-05 - 10000 umolCO2/g/s

**leaf_delta13C**


- label: Leaf carbon (C) isotope signature (delta 13C)
- description: Leaf carbon stable isotope signature
- number of records: 914
- number of studies: 7
- type: numeric
- units: per mille
- allowable range: -50 - 0 per mille

**leaf_delta15N**


- label: Leaf nitrogen (N) isotope signature (delta 15N)
- description: Leaf nitrogen stable isotope signature
- number of records: 580
- number of studies: 4
- type: numeric
- units: per mille
- allowable range: -100 - 100 per mille

**leaf_dry_mass**


- label: Leaf dry mass
- description: Leaf dry mass
- number of records: 1753
- number of studies: 5
- type: numeric
- units: mg
- allowable range: 0.1 - 10000 mg

**leaf_dry_matter_content**


- label: Leaf dry mass per unit leaf fresh mass (Leaf dry matter content, LDMC)
- description: Leaf dry mass per unit leaf fresh mass
- number of records: 513
- number of studies: 8
- type: numeric
- units: g/g
- allowable range: 0.001 - 10 g/g

**leaf_elastic_modulus**


- label: Leaf elastic modulus
- description: The ratio of the change in cell turgor relative to the change in cell volume
- number of records: 0
- number of studies: 0
- type: numeric
- units: mmol/m2/s
- allowable range: 0.1 - 100 mmol/m2/s

**leaf_fracture_force**


- label: Leaf force of fracture
- description: Measures of leaf mechanical resistant; how much force is required to shear, punch or rip a leaf
- number of records: 82
- number of studies: 2
- type: numeric
- units: N
- allowable range: 0.01 - 10 N

**leaf_K_per_area**


- label: Leaf potassium (K) content per unit leaf area
- description: Leaf potassium (K) content per unit leaf area
- number of records: 25
- number of studies: 2
- type: numeric
- units: g/m2
- allowable range: 0.1 - 10 g/m2

**leaf_K_per_dry_mass**


- label: Leaf potassium (K) content per unit leaf dry mass
- description: Leaf potassium (K) content per unit leaf dry mass
- number of records: 334
- number of studies: 6
- type: numeric
- units: mg/g
- allowable range: 0.1 - 100 mg/g

**leaf_length**


- label: Leaf length
- description: Length of the leaf, including petiole and rachis in compound leaves
- number of records: 37465
- number of studies: 32
- type: numeric
- units: mm
- allowable range: 0.1 - 1e+05 mm

**leaf_lifespan**


- label: Leaf lifespan (longevity)
- description: Leaf lifespan (longevity)
- number of records: 199
- number of studies: 4
- type: numeric
- units: month
- allowable range: 1 - 1000 month

**leaf_margin**


- label: Leaf margin
- description: Description of leaf margin are lobed versus entire. Trait values could be expanded to include toothed as a separate category
- number of records: 10192
- number of studies: 5
- type: categorical
- allowable values:
    - *entire*: Leaf margin without lobes or toothed
    - *lobed*: Leaf margin that is either lobed or toothed


**leaf_mass_fraction**


- label: Leaf mass fraction
- description: Ratio of leaf dry mass to total plant dry mass
- number of records: 18
- number of studies: 1
- type: numeric
- units: g/g
- allowable range: 0.1 - 1 g/g

**leaf_N_per_area**


- label: Leaf nitrogen (N) content per unit leaf area
- description: Leaf nitrogen (N) content per unit leaf area
- number of records: 807
- number of studies: 11
- type: numeric
- units: g/m2
- allowable range: 0.2 - 10 g/m2

**leaf_N_per_dry_mass**


- label: Leaf nitrogen (N) content per unit leaf dry mass
- description: Leaf nitrogen (N) content per unit leaf dry mass
- number of records: 2326
- number of studies: 26
- type: numeric
- units: mg/g
- allowable range: 0.1 - 100 mg/g

**leaf_P_per_area**


- label: Leaf phosphorus (P) content per unit leaf area
- description: Leaf phosphorus (P) content per unit leaf area
- number of records: 640
- number of studies: 9
- type: numeric
- units: g/m2
- allowable range: 0.005 - 1 g/m2

**leaf_P_per_dry_mass**


- label: Leaf phosphorus (P) content per unit leaf dry mass
- description: Leaf phosphorus (P) content per unit leaf dry mass
- number of records: 1289
- number of studies: 17
- type: numeric
- units: mg/g
- allowable range: 0.03 - 8 mg/g

**leaf_phenology**


- label: Leaf phenology type (evergreen, deciduous, semideciduous)
- description: Variable indicating whether a plant has deciduous versus evergreen leaves; different types of deciduousness included as trait values
- number of records: 8820
- number of studies: 21
- type: categorical
- allowable values:
    - *deciduous*: Plant where all leaves are shed yearly, either due to drought or cold
    - *evergreen*: Plant which retains its leaves year-round
    - *facultative_drought_deciduous*: Plant that sometimes sheds its years in response to drought
    - *semi_deciduous*: Plant that sheds its years each year for just a very brief period of time


**leaf_photosynthetic_nitrogen_use_efficiency**


- label: Leaf photosynthesis rate per unit leaf nitrogen (N) content (photosynthetic nitrogen use efficiency, PNUE)
- description: Ratio of photosynthesis (CO2 assimilation rate) to leaf nitrogen content
- number of records: 12
- number of studies: 1
- type: numeric
- units: umolCO2/s/gN
- allowable range: 1 - 10 umolCO2/s/gN

**leaf_photosynthetic_water_use_efficiency**


- label: Leaf photosynthesis rate per unit leaf transpiration (water use efficiency, WUE)
- description: Ratio of photosynthesis (CO2 assimilation rate) to leaf transpiration (water loss)
- number of records: 12
- number of studies: 1
- type: numeric
- units: umolCO2/umolH2O
- allowable range: 0.01 - 1 umolCO2/umolH2O

**leaf_saturated_water_content_per_mass**


- label: Water content per unit mass of saturated leaf
- description: Ratio of water in a saturated leaf (maximal water holding capacity at full turgidity) to leaf dry mass
- number of records: 271
- number of studies: 2
- type: numeric
- units: g/g
- allowable range: 0.1 - 10 g/g

**leaf_shape**


- label: Leaf shape
- description: Leaf shape
- number of records: 3244
- number of studies: 11
- type: categorical
- allowable values:
    - *article*: Leaf comprised of jointed segments, as in family Casuarinaceae
    - *branchlets_articles*: Branches with article leaves, as in family Casuarinaceae
    - *broadly_lanceolate*: Like lanceolate, but broader
    - *circular*: Circular
    - *cladode*: A flattened stem or internode that resembles and functions as a leaf
    - *cuneate*: Triangular, wedge-shaped, stem attaches to point
    - *deltoid*: Triangular, stem attaches to side
    - *dimidate*: A leaf in which only one side is developed
    - *elliptical*: Oval shaped, without a distinct point
    - *falcate*: Sickle-shaped
    - *filiform*: Thread-like or filament-shaped
    - *hastate*: Spear-shaped - pointed, with barbs, shaped like a spear point, with flaring pointed lobes at the base
    - *lanceolate*: Long, wider in the middle, shaped like a lance tip
    - *linear*: Long and very narrow like a blade of grass
    - *linear_oblanceolate*: A particularly long, narrow oblanceolate leaf
    - *lobed*: Being divided by clefts, may be pinnately lobed or palmately lobed
    - *lorate*: Having the form of a strap
    - *narrowly_cuneate*: like cuneate, but narrower
    - *narrowly_elliptical*: like elliptical, but narrower
    - *narrowly_obovate*: like obovate, but narrower
    - *narrowly_ovate*: like ovate, but narrower
    - *needle*: Needle-shaped
    - *obcordate*: Heart-shaped, stem attaches at the tapering end
    - *oblanceolate*: Much longer than wide and with the widest portion near the tip, reversed lanceolate
    - *oblate*: An elongate leaf that is wider in the middle than at the two ends
    - *oblong*: Having an elongated form with slightly parallel sides, roughly rectangular
    - *obovate*: Teardrop-shaped, stem attaches to the tapering end; reversed ovate
    - *obovoid*: A 3-dimensional shape; teardrop-shaped, stem attaches to the tapering end
    - *orbicular*: Circular
    - *ovate*: Oval, egg-shaped, with a tapering point and the widest portion near the petiole
    - *palm*: UNCERTAIN if this refers to a leaf from a palm (Araceeae) or a palmately lobed leaf
    - *palmately_lobed*: Palm-shaped, i.e., with lobes or leaflets stemming from the leaf base
    - *peltate*: A round leaf where the petiole attaches near the center
    - *reniform*: Shaped like a kidney - an oval with an inward curve on one side
    - *rhomboidal*: Diamond-shaped
    - *spathulate*: Spoon-shaped; having a broad flat end which tapers to the base
    - *terete*: Circular in cross-section; more or less cylindrical without grooves or ridges
    - *triangular*: Triangular leaf where stem attaches to a side; similar to deltoid


**leaf_specific_conductivity**


- label: Leaf area-specific conductivity / leaf specific conductivity
- description: The ratio of hydraulic conductance of a segment to leaf area
- number of records: 38
- number of studies: 1
- type: numeric
- units: 10^4 x kg/m/s/MPa
- allowable range: 0.1 - 100 10^4 x kg/m/s/MPa

**leaf_thickness**


- label: Leaf thickness
- description: Thickness of the leaf lamina
- number of records: 684
- number of studies: 9
- type: numeric
- units: mm
- allowable range: 0.1 - 10 mm

**leaf_toughness**


- label: Leaf toughness
- description: The work (energy) required to fracture a leaf by tearing, shearing or punching
- number of records: 82
- number of studies: 2
- type: numeric
- units: N/m
- allowable range: 100 - 10000 N/m

**leaf_transpiration**


- label: Leaf transpiration
- description: Rate of water loss from leaf
- number of records: 111
- number of studies: 1
- type: numeric
- units: mmol/m2/s
- allowable range: 1e-04 - 10000 mmol/m2/s

**leaf_type**


- label: Leaf type
- description: Broad definitions of leaf type
- number of records: 741
- number of studies: 11
- type: categorical
- allowable values:
    - *article*: Leaf comprised of jointed segments
    - *broadleaf*: Flat leaf lamina
    - *needle*: Needle or awl-shaped leaf
    - *phyllode*: Winged leaf stalk which functions as a leaf
    - *scale_leaf*: A small flat leaf resembling a scale


**leaf_water_content_per_area**


- label: Leaf water content per unit area
- description: The ratio of the mass of water in a leaf relative to its surface area
- number of records: 47
- number of studies: 2
- type: numeric
- units: g/m2
- allowable range: 10 - 1000 g/m2

**leaf_water_content_per_mass**


- label: Leaf water content per unit mass
- description: The ratio of the mass of water in a leaf relative to its dry mass
- number of records: 326
- number of studies: 4
- type: numeric
- units: g/g
- allowable range: 0.001 - 100 g/g

**leaf_width**


- label: Leaf width
- description: The longest width axis of a leaf; orthogonal to its length
- number of records: 36544
- number of studies: 34
- type: numeric
- units: mm
- allowable range: 0.01 - 1e+05 mm

**life_history**


- label: Life history
- description: Categorical description of plant's life history
- number of records: 22926
- number of studies: 37
- type: categorical
- allowable values:
    - *annual*: Plant that lives for only a single year
    - *biennial*: Plant with a two-year lifespan
    - *ephemeral*: Very short-lived plant; the lifespan is usually only a few months
    - *indefinite*: Very long-lived plant, whose exact lifespan is unknown. Category also includes plants that reproduce vegetatively, such that their 'genetic' lifespan is indefinite
    - *long_lived*: Very long-lived plant, whose exact lifespan is unknown
    - *perennial*: Plant that lives for 3 or more growing seasons
    - *short_lived*: Unclear term that only suggests a plant is not long-lived - could be ephemeral, annual or biennial


**lifespan**


- label: Life span (years)
- description: Broad categories of plant life span, in years
- number of records: 230
- number of studies: 1
- type: categorical
- allowable values:
    - *<1*: Plant that lives less than 1 year
    - *<5*: Plant that lives less than 5 years
    - *<10*: Plant that lives less than 10 years
    - *<20*: Plant that lives less than 20 years
    - *<50*: Plant that lives less than 50 years
    - *<100*: Plant that lives less than 100 years
    - *>100*: Plant that lives more than 100 years
    - *?*: Plant with unknown lifespan


**modulus_of_elasticity**


- label: Bulk modulus of elasticity (Young's modulus)
- description: A measure of xylem's resistance to being deformed elastically (i.e., non-permanently) when a stress is applied to it
- number of records: 125
- number of studies: 1
- type: numeric
- units: Pa
- allowable range: 1 - 100000 Pa

**modulus_of_rupture**


- label: Bulk modulus of rupture
- description: A measure of the force required to rupture xylem vessel
- number of records: 125
- number of studies: 1
- type: numeric
- units: Pa
- allowable range: 1 - 100000 Pa

**nitrogen_fixing**


- label: Plant nitrogen fixation capacity
- description: Binary variable describing whether or not a plant hosts a nitrogen-fixing bacteria
- number of records: 1332
- number of studies: 14
- type: categorical
- allowable values:
    - *0*: plant does not exhibit nitrogen-fixation
    - *1*: plant does exhibit nitrogen-fixation


**photosynthetic_pathway**


- label: Photosynthetic pathway
- description: Type of photosynthetic pathway displayed by plants
- number of records: 913
- number of studies: 12
- type: categorical
- allowable values:
    - *c3*: Plant using the 'standard' photosynthetic pathway, where a 3-carbon compound is produced after the first stage in the photosynthetic pathway
    - *c4*: Plants in which the photosynthetic light-dependent reactions and the Calvin cycle are physically separated to reduce photo respiration
    - *cam*: Plants which display crassulacean acid metabolism


**photosynthetic_rate_per_area**


- label: Leaf photosynthesis rate per unit leaf area
- description: Maximum rate at which a plant consumes carbon dioxide through photosynthesis, normalised to leaf area
- number of records: 564
- number of studies: 9
- type: numeric
- units: umolCO2/m2/s
- allowable range: 0.25 - 40 umolCO2/m2/s

**photosynthetic_rate_per_area_saturated**


- label: Leaf photosynthesis rate per unit leaf area
- description: Maximum rate at which a plant consumes carbon dioxide through photosynthesis, normalised to leaf area
- number of records: 20
- number of studies: 1
- type: numeric
- units: umolCO2/m2/s
- allowable range: 0.25 - 40 umolCO2/m2/s

**photosynthetic_rate_per_dry_mass**


- label: Leaf photosynthesis rate per unit leaf dry mass
- description: Maximum rate at which a plant consumes carbon dioxide through photosynthesis, normalised to leaf dry mass
- number of records: 448
- number of studies: 6
- type: numeric
- units: umolCO2/g/s
- allowable range: 0.01 - 1 umolCO2/g/s

**plant_growth_form**


- label: Plant Growth Form
- description: Different growth forms displayed by plants, including both standard plant growth form descriptors (tree, shrub, etc.) and specific plant characteristics (i.e. patristic)
- number of records: 32921
- number of studies: 50
- type: categorical
- allowable values:
    - *aquatic*: Grows primarily in water
    - *climber*: Plant that climbs up another plant's trunk, branches, rather than being able to support itself
    - *climber_tree*: Tree that climbs up another plant
    - *climber_herb*: Herbaceous plant that climbs up another plant
    - *climber_shrub*: Shrubby plant that climbs up another plant
    - *epiphyte*: Plant that grows on top of another plant
    - *fern*: A group of vascular plants that reproduce via spores
    - *geophyte*: A perennial plant with an underground food storage organ, such as a bulb, tuber, corm, or rhizome
    - *graminoid*: Herbaceous plants with a grass-like morphology
    - *herb*: A seed-bearing plant which does not have a woody stem and dies down to the ground after flowering
    - *palm*: Flowering plants in the family Arecaceae, most with large, compound evergreen leaves referred to as fronds
    - *parasite*: A plant that derives some or all of its nutritional requirements from another living plant
    - *prostrate*: Plant that lies flat against the ground
    - *subshrub*: A dwarf shrub, especially one that is only woody at the base
    - *succulent*: A plant where some parts are thick and fleshy and usually involved in water storage
    - *shrub*: A woody plant that has multiple stems arising at or near ground level
    - *treelet*: A woody plant that is intermediate between a shrub and a tree
    - *tree*: A tall, woody, perennial plant, usually with a single main trunk


**plant_height**


- label: Plant height
- description: Vegetative plant height
- number of records: 51593
- number of studies: 58
- type: numeric
- units: m
- allowable range: 0.001 - 130 m

**plant_width**


- label: Plant width
- description: The width of the plant canopy
- number of records: 630
- number of studies: 2
- type: numeric
- units: m
- allowable range: 0.001 - 20 m

**regen_strategy**


- label: Regeneration strategy
- description: Different regeneration strategies displayed by plants. Trait values include both generic terms (i.e. vegetative) and quite specific ones (i.e. wood rootstock)
- number of records: 3018
- number of studies: 5
- type: categorical
- allowable values:
    - *apical*: Regenerating from apical meristems
    - *basal*: Regenerating from basal meristems
    - *epicormic*: Plants that resprout from epicormic buds
    - *fleshy_organ*: Plants that use fleshy, generally underground organs to regenerate; examples include corms, tubers, bulbs, and rhizomes
    - *lignotuber*: Plant contains an underground lignotuber
    - *not_vegetative*: Plant that regenerate only by seed
    - *seed*: Plant that regenerate only by seed
    - *vegetative*: Generic term including all plants that can regenerate through the growth and division of vegetative material, not just from seed; examples include runners, rhizomes, and bulbs
    - *woody_rootstock*: Plant that regenerate by resprouting from wood rootstock following a disturbance


**root_mass_fraction**


- label: Fraction of plant dry mass comprised of root material
- description: Fraction of plant dry mass comprised of root material
- number of records: 11
- number of studies: 1
- type: numeric
- units: dimensionless
- allowable range: 0.01 - 1 dimensionless

**root_structure**


- label: Root structure
- description: Specific specialized types of root structures and root symbioses
- number of records: 289
- number of studies: 3
- type: categorical
- allowable values:
    - *arbuscular_mycorrhizal*: Symbiosis in which the mycorrhizal fungi does penetrate the cortex cells of the plant roots and its hyphae form arbuscules and vesicles inside the plant root; endomycorrhizal
    - *dauciform_root*: A type of cluster root found in the family Cyperaceae
    - *ectomycorrhizal*: Symbiosis in which the mycorrhizal fungi does not penetrate the cortex cells of the plant roots
    - *ericoid_mycorrhizal*: A form of arbuscular mycorrhizal relationship only found on plants in the order Ericales. The fungi's hyphae penetrate the plant root but do not form arbuscules
    - *orchid_mycorrhizal*: A mycorrhizal relationship specific to orchid species, and for most orchids, essential for seedlings to establish
    - *non_mycorrhizal*: Plants lacking a mycorrhizal symbiont
    - *proteoid_root*: Also known as cluster roots, are plant roots that form clusters of closely spaced short lateral rootlets and aid in nutrient upake in nutrient-poor soils; common in members of the family Proteaceae, but present in other families as well
    - *sand-binding*: Persistent sheaths of sand that form around the roots of species in the families Restionaceae, Cyperaceae, Haemodoraceae, and Lomandraceae in western Australia. Assumed to function in nutrient uptake.


**root_wood_density**


- label: Root wood density
- description: For root wood, the ratio of mass to volume, yielding density
- number of records: 62
- number of studies: 1
- type: numeric
- units: mg/mm3
- allowable range: 0.1 - 10 mg/mm3

**sapwood_specific_conductivity**


- label: Sapwood specific conductivity / stem specific conductivity
- description: Describes the flow rate of water (kg/s) along a stem for a given drop in pressure (1/MPa), extremenormalised to the length of the segment (1/m)
- number of records: 141
- number of studies: 2
- type: numeric
- units: kg/m/s/MPa
- allowable range: 0.05 - 50 kg/m/s/MPa

**seed_breadth**


- label: Seed breadth
- description: The shorter width axis of a seed; orthogonal to its length
- number of records: 1281
- number of studies: 13
- type: numeric
- units: mm
- allowable range: 0.01 - 100 mm

**seed_length**


- label: Seed length
- description: Longest seed dimension
- number of records: 12430
- number of studies: 27
- type: numeric
- units: mm
- allowable range: 0.01 - 1000 mm

**seed_mass**


- label: Seed dry mass
- description: Seed carbon dry mass
- number of records: 18555
- number of studies: 31
- type: numeric
- units: mg
- allowable range: 1e-04 - 1e+05 mg

**seed_mass_reserve**


- label: Seed dry mass reserve
- description: Energy reserves stored in seeds that are mobilized at the time of germination; on a carbon dry mass basis
- number of records: 45
- number of studies: 2
- type: numeric
- units: mg
- allowable range: 1e-04 - 1000 mg

**seed_P_concentration**


- label: Seed phosphorus concentration
- description: Seed phosphorus concentration on a weight basis
- number of records: 77
- number of studies: 2
- type: numeric
- units: mg/g
- allowable range: 1 - 100 mg/g

**seed_shape**


- label: Seed shape
- description: Possible seed shapes. Note that some terms currently used refer to 2-dimensional shapes, not 3-dimensional shapes.
- number of records: 2797
- number of studies: 8
- type: categorical
- allowable values:
    - *angular*: Doesn't refer to a specific shape, but simply that instead of being smoothly rounded, it is angular
    - *broadly_obovoid*: A 3-dimensional shape; like obovoid, but particularly broad in the middle
    - *circular*: Both width dimensions are similar; rounded
    - *comma_shaped*: Seed shaped liked a comma; includes the 2-dimensional term falcate
    - *compressed_ellipsoid*: Like ellipsoid, but a flatter 3-dimensional shape
    - *compressed_obovoid*: Like obovoid, but a flatter 3-dimensional shape
    - *conical*: A 3-dimensional shape; shaped like a cone; the opposite of turbinate
    - *cordiform*: A 3-dimensional shape; heart-shaped
    - *cylindrical*: A 3-dimensional shape; shaped like a cylinder; includes "terete-shaped", which is defined as circular in cross-section
    - *discoidal*: A 3-dimensional shape; resembling a disc, with two convex faces; similar to plano-convex and lenticular, flatter
    - *ellipsoid*: A 3-dimensional shape; elliptic in outline and with a length:breadth ratio between 3:2 and 2:1
    - *elliptical*: SHOULD BE ELLIPSOID?; a 2-dimensional shape; elliptic in outline and with a length:breadth ratio between 3:2 and 2:1
    - *flat*: Suggests a quite flattened seed, but no information about shape
    - *fusiform*: A 2-dimensional shape; spindle-shaped, cigar-shaped, circular in cross-section and tapering at both ends
    - *globose*: Same as globular
    - *globular*: Nearly spherical
    - *lachrimiform*: A 3-dimensional shape; tear-shaped
    - *lanceolate*: A 2-dimensional shape; much longer than wide, with the widest point below the middle; in reference to a seed, assume a rounded 2-D shape
    - *lenticular*: A 3-dimensional shape; shaped like a biconvex lens. Similar to plano-convex??
    - *muricate*: A 2 or 3-dimensional shape; has numerous short hard outgrowths
    - *obclavate*: A 2 or 3-dimensional shape; slender, broadening towards the base; club-shaped, with the thickened part at the base
    - *oblong*: A 2-dimensional shape; rectangular with a length:breadth ratio between 3:2 and 2:1
    - *obovate*: A 2-dimensional shape; similar to ovate, but attached at the narrower end and with a length:breadth ratio between 3:2 and 2:1
    - *obovoid*: A 3-dimensional shape; similar to ovoid, but attached at the narrower end and with a length:breadth ratio between 3:2 and 2:1
    - *orbicular*: A 2 or 3-dimensional shape; fairly flat, but round in 2-dimensional outline
    - *ovate*: A 2-dimensional shape; egg-shaped (i.e., widest below the middle)
    - *ovoid*: A 3-dimensional shape; egg-shaped (i.e., widest below the middle)
    - *pea_shape*: Assume same as spherical
    - *plano_convex*: A 3-dimensional shape; fairly flat, but with slightly raised, convex faces
    - *pyramidal*: A 3-dimensional shape; resembling a pyramid
    - *pyriform*: A 3-dimensional shape; resembling a pear, attached at the broader end
    - *rectangular*: A 3-dimensional shape; resembling a rectangular-prism
    - *reniform*: A 2 or 3-dimensional shape; having a circular or roughly circular shape with a notch, like a kidney
    - *rhombic*: A 2-dimensional shape; diamond shaped in outline with the broadest axis in the middle and with a length:breadth ratio between 3:2 and 2:1
    - *round*: Assume same as spherical
    - *samara*: Aa type of fruit in which a flattened wing of fibrous, papery tissue develops from the ovary wall; not truly a shape
    - *sectoroid*: A 3-dimension shape; shaped like an orange segment
    - *segment_shaped*: UNCERTAIN; assume the outline is a flattened rectangular prism
    - *semicircular*: Semi-circular in cross-section
    - *spherical*: A 3-dimensional shape; round with all dimensions approximately equal
    - *spheroidal*: A 3-dimensional shape; nearly, but imperfectly spherical
    - *square*: A 3-dimensional shape; with four sides perpendicular to the base and resembling a square prism
    - *subcircular*: Nearly or almost circular in shape
    - *subcylindrical*: Nearly or almost cylindrical in shape
    - *subglobular*: Nearly or almost globular in shape
    - *suborbicular*: Nearly or almost orbicular in shape
    - *subtriangular*: Nearly or almost triangular in shape
    - *triangular*: A 3-dimensional shape; with three sides and broadest below the middle
    - *turbinate*: A 3-dimensional shape; top shaped; inversely conical
    - *wedge_shaped*: A 2 or 3-dimensional shape; tapers to a point at one end; includes the 2-D classificatiion "cuneate" and possibly plants that are more conical in outline


**seed_volume**


- label: Seed volume
- description: Volume of a seed
- number of records: 519
- number of studies: 1
- type: numeric
- units: mm3
- allowable range: 0.001 - 1e+05 mm3

**seed_width**


- label: Seed width
- description: The longest width dimension of a seed; orthogonal to the length
- number of records: 7403
- number of studies: 23
- type: numeric
- units: mm
- allowable range: 0.01 - 100 mm

**serotiny**


- label: Serotiny
- description: When a fruit or cone only releases its seeds following an environmental trigger, often fire
- number of records: 379
- number of studies: 4
- type: categorical
- allowable values:
    - *not_serotinous*: Plant does NOT display serotiny
    - *serotinous*: Plant does display serotiny
    - *serotiny_low*: Plant displays a low level of serotiny
    - *serotiny_moderate*: Plant displays a moderate level of serotiny


**specific_leaf_area**


- label: Leaf area per unit leaf dry mass (specific leaf area, SLA)
- description: Leaf area per unit leaf dry mass; SLA
- number of records: 20870
- number of studies: 57
- type: numeric
- units: mm2/mg
- allowable range: 0.5 - 500 mm2/mg

**stem_cross_section_area**


- label: Stem cross-sectional area
- description: Cross-sectional area of the stem
- number of records: 0
- number of studies: 0
- type: numeric
- units: mm2
- allowable range: 1 - 1000 mm2

**stem_cross_section_area_no_bark**


- label: Stem cross-sectional area without bark
- description: Cross-sectional area of the stem once bark is removed
- number of records: 0
- number of studies: 0
- type: numeric
- units: mm2
- allowable range: 1 - 1000 mm2

**basal_diameter**


- label: Stem diameter
- description: Diameter at the base of the plant, standardly "DBH" except in short plants
- number of records: 1900
- number of studies: 2
- type: numeric
- units: mm
- allowable range: 0.1 - 1000 mm

**stomatal_conductance_per_area**


- label: Stomatal conductance per unit leaf area
- description: Rate of water loss through stomata, per unit leaf area
- number of records: 82
- number of studies: 2
- type: numeric
- units: mmolH2O/m2/s
- allowable range: 10 - 1e+06 mmolH2O/m2/s

**storage_organ**


- label: Storage organ types
- description: Variable, defining whether a storage organ is present and types of storage organs
- number of records: 319
- number of studies: 3
- type: categorical
- allowable values:
    - *lignotuber*: Plant contains an underground lignotuber
    - *rhizome*: Plant contains a rhizome
    - *no_storage_organ*: No storage organ present
    - *storage_organ*: Storage organ present, but type  is unknown
    - *unknown*: Assume this trait value means it is unknown if a storage organ is present, not that the type is unknown


**water_potential_midday**


- label: Midday water potential
- description: A plant's water potential during the heat of the day
- number of records: 126
- number of studies: 1
- type: numeric
- units: MPa
- allowable range: -50 - 0 MPa

**water_potential_predawn**


- label: Pre-dawn water potential
- description: A plant's water potential just before sunrise
- number of records: 126
- number of studies: 1
- type: numeric
- units: MPa
- allowable range: -30 - 0 MPa

**water_use_efficiency**


- label: Water use efficiency
- description: Rate of carbon dioxide uptake relative to water loss, per unit leaf area
- number of records: 111
- number of studies: 1
- type: numeric
- units: mmolCO2/molH2O/m2/s
- allowable range: 1 - 10 mmolCO2/molH2O/m2/s

**wood_density**


- label: Stem dry mass per unit fresh stem volume
- description: Stem dry mass per unit stem fresh volume (stem specific density or SSD or wood density)
- number of records: 4617
- number of studies: 35
- type: numeric
- units: mg/mm3
- allowable range: 0.1 - 1.3 mg/mm3

**woodiness**


- label: Woodiness
- description: A plant's degree of lignification in stems
- number of records: 6575
- number of studies: 5
- type: categorical
- allowable values:
    - *herbaceous*: Plant with non-lignified stems
    - *semi_woody*: Plant with partially lignified stems
    - *woody*: Plant that produces secondary xylem, have lignin

# Building and contributing to AusTraits

Anyone wishing to use the AusTraits data will need to begin by building AusTraits to create a current compiled dataset. Below, under [Approach](#approach) we describe conceptually how individual datasets are merged into the complete AusTraits compilation and under [Rebuilding from scratch](#building) is the code needed to actually build AusTraits.

## Approach {#approach}

Data in AusTraits is stored by 

XXXXX Daniel, can you write this?

(More here)

## Structure of raw data files for AusTraits {#format}

(More here)

XXXXX

(needs updating)



```
austraits
├── austraits.Rproj
├── config
├── data
├── R
├── README.md
├── remake.yml
├── tests
└── vignettes
```

The folder `data` contains a long list of folders, one for each study and each containing two files:


```
data
├── Angevin_2010
│   ├── data.csv
│   └── metadata.yml
├── Barlow_1981
│   ├── data.csv
│   └── metadata.yml
├── Bean_1997
│   ├── data.csv
│   └── metadata.yml
├── Blackman_2014
│   ├── data.csv
│   └── metadata.yml
├── ....

```

where `Angevin_2010`, `Barlow_1981`, `Bean_1997`, `Westoby_2014` are each a unique `dataset_id` in the final dataset.

## Rebuilding from scratch {#building}

XXXXX clone austraits from github (Daniel include details)

XXXXX
Once you have cloned AusTraits onto your computer, run the following code to ensure you have a compiled version of AusTraits and have sourced any relevant functions


```r
austraits <- remake::make("austraits")
source("R/support.R")
source("R/setup.R")
source("R/steps.R")
source("R/austraits.R")
source("R/support.R")
source("R/report_utils.R")
source("R/report_notetaker.R")
```

Within the compiled matrix `austraits` are the elements listed in [Elements of AusTraits](#elements). The actual trait data can be found in the dataframe `austraits$traits`.


# Format of raw data files for AusTraits {#format}

This section describes how to prepare / modify the raw data from individual studies for inclusion in the AusTraits compilation.

Data from each study is organised into a separate folder, with two files:<br/>
  - `data.csv`: a table containing the actual trait data in comma-separated values format, ideally with each trait in a single column and each observation on a single row (i.e. wide format).<br/>
  - `metadata.yml`: contains further information about the study, maps trait names and units onto standard types, and lists any substitutions applied to the data in processing (see below). 

Below are the Instructions on how to format files to contribute a new study to AusTraits. It is important that all steps are followed so that our automated workflow proceeds without problems.

1. XXXXX create a github branch?
2. Create a new folder within the folder `data` with a name corresponding to the paper or study, e.g. `Gallagher_2014` (do not include *et al* or similar).
3. Prepare the files `data.csv` and `metadata.yml` and place them within the folder, as per instructions [below](#create_data). 
4. Add the new study into the build framework and rebuild AusTraits, as described under [Adding data to AusTraits](#adding) below.
5. Run tests on the contributed data and correct the `data.csv` and `metadata.yml`files as necessary. See the section [Tests](#running_tests) below for more details.
6. Generate and proofread a report on the data. In particular, check that numeric trait values fall within a logical range relative to other studies and that individual trait observations are not unnecessarily excluded because their trait values are unsupported. 
7. Return to step 3 if changes are made to the `data.csv` or `metadata.yml` files.
8. Push to GitHub. 


### Data.csv {#create_data}

The file `data.csv` file can be in either long or wide format.

Required columns include the species name, the trait name (column in long format, header in wide format), units (column in long format, part of header in wide format), site (if applicable), and trait values.

If multiple trait measurements were made on the same individual or are the mean of a species' measurements from the same site, they should be kept linked. If the data is in wide format, each row should represent measurements made on a single individual or a single species-by-site mean, with different trait values as consecutive columns. If the data is in long format, an observation ID (or other identifier) must be assigned to identify which measurements are linked to a unique individual or site.

Keep the data file in rawest form possible (i.e. as few changes as possible) but it must be a single csv file. Additional custom R code may be required to make the file exactly compatible with the AusTraits format, but these changes should be executed as AusTraits is compiled and should be in the `metadata.yml` file under `config/custom_R_code`. Any files used to create the submitted `data.csv` file (e.g. Excel ...) should be supplied so they can be archived in a subfolder within the study folder named `raw`.

### Metadata.yml

The metadata is compiled in a `.yml` file, a structured data file where information is presented in a hierarchical format (see [Appendix for details](#yaml)).  There are 9 values at the top hierarchical level: source, people, dataset, sites, config, traits, substitutions, taxonomic_updates, questions. These are each described below.

As a start, you may want to checkout some examples from [existing studies in Austraits](https://github.com/traitecoevo/austraits/tree/master/data), e.g. [Angevin_2010](https://github.com/traitecoevo/austraits/blob/master/data/Angevin_2010/metadata.yml) or [Wright_2004](https://github.com/traitecoevo/austraits/blob/master/data/Wright_2004/metadata.yml).

#### Source

This section provides citation details for the original source(s) for the data, whether it is a published journal article, book, website, or thesis. In general we aim to reference the primary source. References are written in structured yml format, under the category `source` and then sub-groupings `primary` and `secondary`. General guidelines for describing a source

- maximum of one primary and secondary source allowed
- elements are names as in [bibtex format](https://en.wikipedia.org/wiki/BibTeX)
- keys should be named in the format `Surname_year` and should be identical to the name given to the dataset folder.
- a secondary source may be needed if the main collector is not an author on the paper where data was released, or data were otherwise released via a subsequent study.
- if your data is from an unpublished study, only include the elements that are applicable

Following are some examples for different types of source.

A journal article:

```
source:
  primary:
    key: Falster_2005
    bibtype: Article
    author: Daniel S. Falster, Mark Westoby
    year: 2005 title: Alternative height strategies among 45 dicot rain forest species from tropical Queensland, Australia
    journal: Journal of Ecology volume: 93 pages: 521--535
    publisher: Wiley-Blackwell
    doi: 10.1111/j.0022-0477.2005.00992.x
  secondary: .na
```
A book:

```
source:
  primary:
    key: Cooper_2004
    bibtype: Book
    author: Wendy Cooper, William T. Cooper
    year: 2004
    title: Fruits of the Australian tropical rainforest
    publisher: Nokomis Editions
    isbn: '9780958174213'
  secondary: .na
```

An online resource:

```
source:
  primary: key: WAH_1998
    bibtype: Misc
    author: Western Australian Herbarium
    year: 1998
    title: FloraBase--the Western Australian Flora
    publisher: Department of Parks and Wildlife
    url: https://florabase.dpaw.wa.gov.au/
  secondary: .na
```

An unpublished resource:

```
source:
  primary:
    key: Duncan_1998
    bibtype: Unpublished
    author: David H. Duncan
    year: 1998
    title: Leaf anatomy of Australian plant species
    note: Collected while at Macquarie University
  secondary: .na
```

Note that in these first examples `secondary` is set as `.na`. If a secondary source is included it may look like:

```
  primary:
    key: Chave_2009
    bibtype: Article
    author: Jerome Chave, David Coomes, Steven Jansen, Simon L. Lewis, Nathan G. Swenson, Amy E. Zanne
    year: 2009 title: Towards a worldwide wood economics spectrum
    journal: Ecology Letters
    volume: 12
    pages: 351--366
    publisher: Wiley-Blackwell
    doi: 10.1111/j.1461-0248.2009.01285.x
  secondary:
    key: Zanne_2009
    bibtype: Misc
    author: Amy E. Zanne, G. Lopez-Gonzalez, David A. Coomes, Jugo Ilic, Steven Jansen, Simon L. Lewis, Regis B. Miller, Nathan G. Swenson, Michael C. Wiemann, Jerome   Chave
    year: 2009
    title: 'Data from: Towards a worldwide wood economics spectrum'
    volume: .na
    pages: .na
    publisher: Dryad Digital Repository
    doi: 10.5061/dryad.234
```

#### People

This section provides a list of the key contributors to the study, their respective institutions and roles in the study. Roles are defined as follows:

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left:30px; width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> key </th>
   <th style="text-align:left;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> collector </td>
   <td style="text-align:left;"> The person (people) leading data collection (up to 2 permitted) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> contributor </td>
   <td style="text-align:left;"> Person responsible for entering data into AusTraits </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lab_leader </td>
   <td style="text-align:left;"> Leader of lab group at time of collection </td>
  </tr>
  <tr>
   <td style="text-align:left;"> assistant </td>
   <td style="text-align:left;"> Anyone else who assisted in collection of the data </td>
  </tr>
  <tr>
   <td style="text-align:left;"> contact </td>
   <td style="text-align:left;"> The person to contact with questions about the data set </td>
  </tr>
</tbody>
</table>

An example is as follows:

```
people:
- name: Daniel Falster
  institution: Macquarie University
  role: collector, contact, contributor
- name: Mark Westoby
  institution: Macquarie University
  role: lab_leader

```

Note that only the AusTraits custodians have the contributors e-mail addresses on file. This information will not be directly available to AusTraits users or new contributors via Github.

#### Dataset

This section details about the study where the data were collected

The following elements are included under the element `dataset`:

- **year_collected_start**: The year data collection commenced
- **year_collected_end**: The year data collection was completed
- **description**: A 1-2 sentence description of the purpose of the study.
- **collection_type**: A field to indicate where the plants on which traits were measured were collected -  in the `field`, `lab`, `glasshouse`, `herbarium specimens`, or `literature`. The latter should only be used when the data were sourced from the literature and the collection type is unknown.
- **sample_age_class**: A field to indicate if the study was completed on `adult` or `juvenile` plants
- **sampling_strategy**: A written description of how study sites were selected and how study individuals were selected. When available, this information is lifted verbatim from a published manuscript. For herbarium studies, this field ideally indicates which herbaria were 'sampled' to measure a specific trait
- **original_file**: The name of the file initially submitted to AusTraits
- **notes**: Generic notes about the study and processing of data

An example is as follows:

```
  year_collected_start: 2004
  year_collected_end: 2004
  description: Trait values for species with faster versus slower height growth following disturbance for Myall Lakes species.
  collection_type: field
  sample_age_class: adult
  sampling_strategy: Fire is a recurrent disturbance in the park (interval – 0–30 years; Fox and Fox 1986). A mosaic of fire histories has facilitated previous use of space-for-time substitutions in studies of small mammal succession (Fox and McKay 1981). Here we employ the same methodology to reconstruct species height-growth trajectories (Enright and Goldblum 1999). Sites were identified at a range of times since fire with the use of NSW national parks GIS fire history records and personal observations of Karen Ross (Ross et al. 2002). Patches of vegetation 1, 2, 4, 8, 10, 12, 15, 27 and 28 years since fire were identified. Where possible several patches within a given age class were surveyed to determine species presence or absence. Nineteen species recorded in a majority of patches were selected for further study. This included eight resprouting species and 11 obligate seeders (full list in Appendix 1).
  original_file: Falster & Westoby 2005 Oikos appendix.doc
  notes: none
```

#### Config

This section information on the format of the submitted data file

Values are as follows:


- **data_is_long_format**: Indicates if the data spreadsheet has a vertical (long) or  horizontal (wide) configuration with `yes` or `no` terminology
- **variable_match**: Identifies which information is in each column in the file `data.csv`, excluding the actual trait data. One element within `variable_match` must be `species_name`. Datasets with `data_is_long_format` set to `yes` must also identify which column includes data on `value` and `trait_name`. Other allowed values include `site_name` and `observation_id`, if these columns are present in the `data.csv` file.
- **custom_R_code**: A field where additional R code can be included. This allows for custom manipulation of the data in the submitted spreadsheet into a different format for easy integration with AusTraits. `.na` indicates no custom R code was used.

An example is
```
config:
  data_is_long_format: yes
  variable_match:
    species_name: Taxon
    value: trait value
    trait_name: trait
  custom_R_code: mutate(data, `trait value` = ifelse(trait == 'flowering time', convert_month_range_vec_to_binary(`trait value`), `trait value`))
```

A common use of the `custom_R_code` is to automate the conversion of a verbal description of flowering or fruiting periods into the supported trait values, as occurs in  this example. It might also be used if values for a single trait are expressed across multiple columns and need to be merged. See the `Catford_2014` as an example of this. Additional examples on adding `custom_R_code` are provided [in the appendix](#custom_R_code).

#### Traits

This section A translation table mapping traits and units in the original study onto corresponding variables in AusTraits. Also specified here are methods used to collect the data.

For each trait submitted to Austraits, there is the following information:

- **var_in**: Names of trait in the original data submitted
- **unit_in**: Units of trait in the original data submitted
- **trait_name**: Name of trait sampled. Allowable values specified in the table `traits`.
- **value_type**: A categorical variable describing the type of trait value recorded
- **replicates**: Number of replicate measurements that comprise the data points for the trait for each measurement. A numeric value (or range) is ideal and appropriate if the value type is a `mean`, `median`, `min` or  `max`. For these value types, if replication is unknown the entry should be `unknown`. If the value type is `raw_value` the replicate value should be `1`. If the value type is `expert_mean`, `expert_min`, or `expert_max` the replicate value should be `.na`.
- **methods**: A textual description of the methods used to collect the trait data. Whenever available, methods are taken near-verbatim from referenced souce. Methods can include descriptions such as 'measured on herbarium specimens','data from the literature', or a detailed description of the field or lab methods used to collect the data.

Values under `trait_name` must be allowable values, as described under [the section trait definitions](#trait_defs). Similarly, values under `value_type` must be allowable values, as described under [the section  Value types](#value_types).

An example is as follows:

```
traits:
- var_in: LMA (mg mm-2)
  unit_in: mg/mm2
  trait_name: specific_leaf_area
  value_type: site_mean
  replicates: 3
  methods: LMA was calculated as the leaf dry mass (oven-dried for 48 hours at 65 °C) divided by leaf size. It was measured on the first five fully expanded leaves at the tip of each individual.
- var_in: leaf size (mm2)
  unit_in: mm2
  trait_name: leaf_area
  value_type: site_mean
  replicates: 3
  methods: Leaf size was calculated as the one-sided leaf area (flat bed scanner). It was measured on the first five fully expanded leaves at the tip of each individual.
```

#### Substitutions


This section provides a list of any "find and replace" substitutions needed to get the data to get it into the right format

Substitutions are required whenever the exact word(s) used to describe a categorical trait value in AusTraits are different from the vocabulary used by the author in the `data.csv` file. It is preferable that authors make changes using `substitutions` rather than changing the `data.csv` file. See the section [trait definitions](#trait_defs) for a list of supported values for each trait.

Each substitution is documented using the following elements:

- **trait_name**: Name of variable to search
- **find**: Value to find
- **replace**: Replacement value

An example is as follows:

```
substitutions:
- trait_name: life_history
  find: p
  replace: perennial
- trait_name: plant_growth_form
  find: s
  replace: shrub
- ...
```

#### Taxonomic updates

This section provides a list of taxonomic name changes needed to align with current taxonomy Taxonomic updates are required to align species names with an accepted species list (for details, see details on [Taxonony](#taxonomy).

Each substitution is documented using the following elements:

- **find**: Taxonomic name given to species in the original data supplied by the authors
- **replace**: Name of species after aligning taxonomy with standard sources.
- **reason**: Records why the change was implemented, e.g. `typos`, `taxonomic synonyms`, and `standardising spellings`

### Taxonomic updates

An example is as follows:

```
taxonomic_updates:
- find: Carissa lanceolata
  replace: Carissa spinarum
  reason: Synonym reported by TaxonStand (2018-09-19)
- find: Melaleuca pallida
  replace: Callistemon pallidus
  reason: Synonym reported by TaxonStand (2018-09-19)
```

#### Questions

This section provides a place to record any queries we have about the dataset (recorded as a named array), including notes on any additonal traits that may have been collected in the study but have not been icnoproated into austraits.

An example is as follows:

```
questions:
  questions for author: Triglochin procera has very different seed masses in the main traits spreadsheet and the field seeds worksheet. Which is correct? There are a number of species with values in the field leaves worksheet that are absent in the main traits worksheet - we have included this data into Austraits; please advise if this was inappropriate.
  austraits: need to map aquatic_terrestrial onto an actual trait once one is created.
```

## Building & rebuilding AusTraits {#building}

To build AusTraits from scratch:..,

(More here)

You can then rebuild Austraits, including your dataset.


```r
austraits <- remake::make("austraits")
```

## Adding data to Austraits {#adding}

This section describes how to prepare / modify the raw data from individual studies for inclusion in the Austraits compilation.

Below are the Instructions on how to format files to contribute a new study to AusTraits. It is important that all steps are followed so that our automated workflow proceeds without problems.

1. XXXXX create a github branch?
2. Create a new folder within the folder `data` with a name corresponding to the paper or study, e.g. `Gallagher_2014` (do not include *et al* or similar).
3. Prepare the files `data.csv` and `metadata.yml` and place them within the folder, as per instructions [below](#create_data).
4. Add the new study into the build framework and rebuild Austraits, as described under [Adding data to Austraits](#adding) below.
5. Run tests on the contributed data and correct the `data.csv` and `metadata.yml`files as necessary. See the section [Tests](#running_tests) below for more details.
6. Generate and proofread a report on the data. In particular, check that numeric trait values fall within a logical range relative to other studies and that individual trait observations are not unnecessarily excluded because their trait values are unsupported.
7. Return to step 3 if changes are made to the `data.csv` or `metadata.yml` files.
8. Push to GitHub.

It may help to download one of the [existing datasets](https://github.com/traitecoevo/austraits/tree/master/data) and use it as a template for your own files and a guide on required content. You should look at the files in the [config folder](https://github.com/traitecoevo/austraits/tree/master/config), in particular the `definitions` files for the list of traits we cover and the supported trait values for each trait. Or read through information on the supported traits and trait values [here](#trait_defs)

Once you have prepared your `data.csv` and `metadata.yml` files within a folder in the `data` directory, you can incorporate the new data into Austraits by running:


```r
austraits_rebuild_remake_setup()
```
(This step updates the file `remake.yml` with appropriate rules for the new dataset; similarly if you remove datasets, do the same. At this stage, [remake](https://github.com/richfitz/remake) offers no looping constructs (on purpose) so for now we generate the remake file using [whisker](https://github.com/edwindj/whisker).)

You can then rebuild Austraits, including your dataset.


```r
austraits <- remake::make("austraits")
```

### Constructing the `metadata.yml` file

One way to construct the `metadata.yml` file is to use one of the existing files and modify yours to follow the same format. As a start, checkout some examples from [existing studies in Austraits](https://github.com/traitecoevo/austraits/tree/master/data), e.g. [Angevin_2010](https://github.com/traitecoevo/austraits/blob/master/data/Angevin_2010/metadata.yml) or [Wright_2004](https://github.com/traitecoevo/austraits/blob/master/data/Wright_2004/metadata.yml).

Note, when editing the `metadata.yml`, edits should be made in a proper text editor (Microsoft word tends to stuff up the formatting). For example, Rstudio works. 

To assist you in constructing the `metadata.yml` file, we have developed functions to help fill in the different sections of the file. If you wish to include additional elements, you can afterwards edit the file further.

To use the functions, make sure you first run the following, to make the functions available



### Creating a template

The first function creates a basic template for your the `metadata.yml` file for your study. Assuming you have already created a file `data.csv` in the folder `data/your_dataset_id`, run



The function will ask a series of questions and then create a relatively empty file `data/your_dataset_id/metadata.yml`. The key questions are:

* Is the data long vs wide? A wide dataset has each variable(i.e. trait ) as a column. A long dataset has each variable as a row and column as a species. 
*Select column for 'species_name'
*Select column for 'trait_name'

### Source

Three functions are available to help entering citation details for the source data.

The function `metadata_create_template` creates a template for the primary source with default fields for a journal article, which you can then edit manually.

Alternatively, if you have a `doi` for your study, use the function:


```r
metadata_add_source_doi(dataset_id, doi)
```
and the different elements within source will automatically be generated. By default, details are added as a primary source. To override this, specify the type


```r
metadata_add_source_doi(dataset_id, doi, type="secondary")
```

Alternatively, if you have reference details saved in a bibtex file called `myref.bib` you can use the function


```r
metadata_add_source_doi(dataset_id, file = "myref.bib")
```

(These options require the package [rcorssref](https://github.com/ropensci/rcrossref) and [RefManageR](https://github.com/ropensci/RefManageR/) to be installed.)

### People

The function `metadata_create_template` creates a template for entering details about people, which you can then edit manually.

### Testing custom R code

Occasionally all the changes we want to make to dataset may not fit into the prescribed workflow used in Austraits. For example, we assume each trait has a single unit. But there are a few datasets where data on different rows have different units. So we want to make to make some custom modifications to this particular dataset before the common pipeline of operations gets applied. To make this possible, the workflow allows for some custom R code to be run as a first step in the processing pipeline. That pipeline (in the function [`load_study`](https://github.com/traitecoevo/austraits/blob/master/R/steps.R)) looks like this:


```r
  data <- 
    read_csv(filename_data_raw, col_types = cols()) %>%
    custom_manipulation(metadata[["config"]][["custom_R_code"]])() %>%
    parse_data(dataset_id, metadata) %>%
    ...
```

Note the second line. After loading the csv file, we can apply some Custom R code small manipulations to the dataframe before processing it. Custom R code is valid R code, but written inside the `metadata.yml` file. While developing this, you'll want to test your code. This can be achieved by running the function



which returns a data frame, showing how the datasets looks after being manipulated.mes

### Traits

Add traits


<!-- TODO: You will be asked to indicate the columns you wish to keep as distinct traits -->

### Sites

Add sites details



This function assumes you have site details stored in wide format, in R:

<!-- TODO: need to indicate that the following code needs to be run with the assignment site_data for the previous code to work. --> 


```
## # A tibble: 2 x 6
##   site_name description `elevation (m)` `latitude (deg)` `longitude (deg…
##   <chr>     <chr>                 <dbl>            <dbl>            <dbl>
## 1 Atherton  Tropical r…             800            -17.1             146.
## 2 Cape Tri… Complex me…              25            -16.1             145.
## # … with 1 more variable: `rainfall (mm)` <dbl>
```

If your data is in a file, you'll need to read it in first.

## Substitutions

Substitutions can be added by running:


```r
metadata_add_substitution(dataset_id, trait_name, find, replace)
```

where `find` is the trait value used in the data.csv file and `replace` is the trait value supported by Austraits.


## Taxonomic Updates

We've implemented code to semi-automate the checking of names using the R package [Taxonstand](https://cran.r-project.org/web/packages/Taxonstand/index.html) (for more documentation see [here](https://www.rdocumentation.org/packages/Taxonstand/versions/2.1/topics/TPL)). To generate a suggested name change for a specific study run:


```r
metadata_check_taxa("Westoby_2014")
```

If TaxonStand finds taxonomic changes to make it will add the relevant lines of code directly to the metadata.yml file.

TaxonStand has been configured in the above function to only permit relatively certain changes (e.g. with a minor change to spelling or known synonym).

There are additional arguments you can add for the function `metadata_check_taxa` including:
- `update` where the default is TRUE, meaning changes found will be added to the `metadata.yml` file
- `typos` where the default is FALSE, meaning typos will not be corrected
- `diffchar` which indicates the number of characters that can be different for a typo-match. Here the default is two.

Therefore, if you want the function  `metadata_check_taxa` to correct 1 and 2 character typos, run the function as follows:


```r
metadata_check_taxa("Westoby_2014", typos=TRUE)
```

If TaxonStand fails to find a suitable alignment, and you have identified one yourself, you can add it to the metadata by running


```r
metadata_add_taxnomic_change(study, find, replace, reason)
```

## Tests {#running_tests}

You can also run some automated tests to ensure the dataset meets required setup. The tests run through a collection of pre-specified checks on the files for each study. The output alerts you to possible issues needing to be fixed, by comparing the data in the files with expected structure and allowed values, as specified in the definitions. 

To run the tests, the variable `dataset_ids` must be defined in the global namespace, containing a vector of ids to check. For example


```r
# load relevant functions
source("R/setup.R")

# Tests run test on one study
dataset_ids <- "Bragg_2002"
austraits_run_tests()

# Tests run test on all studies
dataset_ids <- dir("data")
austraits_run_tests()
```

## Reports / quality checks

To enable better quality checks we have code to generate a report on the data in each study. 

(Reports are written in [Rmarkdown](https://rstudio.github.io/rmarkdown/) and generated via the [knitr](https://cran.r-project.org/web/packages/knitr/) package. The template is stored in the folder `vignettes` called `report_study.html`). 

To generate a report for a particular study:


```r
austraits <- remake::make("austraits")
source("R/report_utils.R")
build_study_report("Wright_2002")
```

**Guidelines for writing report code**

- use [knitr chunk options](https://rmarkdown.rstudio.com/lesson-3.html) to customise when code is shown and how output is displayed.
- use [tidyverse style and format](http://htmlpreview.github.io/?https://github.com/nicercode/2018_BEES_regression/blob/master/tidyverse.html)
- use [kableExtra for styling tables](https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html)

**Maps:** We use the package [leaflet](https://cran.r-project.org/web/packages/leaflet/index.html) to generate interactive maps via the JavaScript 'Leaflet' framework and based on the [Open street map](https://www.openstreetmap.org/).


## Pushing to GitHub

By far our preferred way of contributing is for you to fork the database in github, add your dataset then send us a [pull request](https://help.github.com/articles/using-pull-requests/). If this is not possible, you could email the relevant files (see above) to Rachael Gallagher.


# Usage examples




The following section provides examples for users of `R` on manipulating the AusTraits dataset. Users of other platforms can follow similar steps using commands specific to their platform. 

We like using functions from packages like [dplyr](https://dplyr.tidyverse.org/), [readr](https://readr.tidyverse.org/), [tidyr](https://tidyr.tidyverse.org/) and [`magrittr`](https://magrittr.tidyverse.org/) in the [tidyverse](https://www.tidyverse.org/) to manipulate data. These packages can be loaded individually, or together by running:


```r
# install.packages("tidyverse")
library(tidyverse)
```

The examples below assume you have the tidyverse installed and loaded. This is not essential, but without these packages the examples below won't work.

For anyone needing to learn about the tidyverse, you will in particular want to read up on [dplyr](https://dplyr.tidyverse.org/) and [the pipe operator `%>%` from `magrittr`](https://magrittr.tidyverse.org/), which pipes output from one command into the next command.

In addition, we have included some helper functions for common manipulations of the data. These are available in the file `austraits.R` (TODO: where will users access this). To make these functions available, source the file prior to use:


```r
source("R/austraits.R")
```

## Taking subsets

Using the `dplyr`, it's easy to subset by rows using `filter`. This approach can be used to filter the data to particular trait or study:


```r
austraits$traits %>% 
  filter(trait_name == "leaf_area")
```

```
## # A tibble: 12,446 x 10
##    dataset_id species_name site_name observation_id trait_name value unit 
##    <chr>      <chr>        <chr>     <chr>          <chr>      <chr> <chr>
##  1 Angevin_2… Acaena echi… <NA>      Angevin_2010_… leaf_area  835.… mm2  
##  2 Angevin_2… Ajuga austr… <NA>      Angevin_2010_… leaf_area  300.… mm2  
##  3 Angevin_2… Dichopogon … <NA>      Angevin_2010_… leaf_area  129.… mm2  
##  4 Angevin_2… Asperula co… <NA>      Angevin_2010_… leaf_area  3.307 mm2  
##  5 Angevin_2… Astroloma c… <NA>      Angevin_2010_… leaf_area  44.33 mm2  
##  6 Angevin_2… Astroloma h… <NA>      Angevin_2010_… leaf_area  53.18 mm2  
##  7 Angevin_2… Stipa semib… <NA>      Angevin_2010_… leaf_area  864.… mm2  
##  8 Angevin_2… Brunonia au… <NA>      Angevin_2010_… leaf_area  488.… mm2  
##  9 Angevin_2… Bulbine bul… <NA>      Angevin_2010_… leaf_area  923.… mm2  
## 10 Angevin_2… Caesia call… <NA>      Angevin_2010_… leaf_area  2960… mm2  
## # … with 12,436 more rows, and 3 more variables: value_type <fct>,
## #   replicates <chr>, original_name <chr>
```


```r
austraits$traits %>% 
  filter(dataset_id == "Westoby_2014")
```

```
## # A tibble: 2,633 x 10
##    dataset_id species_name site_name observation_id trait_name value unit 
##    <chr>      <chr>        <chr>     <chr>          <chr>      <chr> <chr>
##  1 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… bark_thic… 0.21… mm   
##  2 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_area  6575  mm2  
##  3 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_area… 1215… m2/m2
##  4 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_dark… 1.96  umol…
##  5 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_dark… 0.02… umol…
##  6 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_delt… -28.… per …
##  7 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_K_pe… 8.63… mg/g 
##  8 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_N_pe… 25.1… mg/g 
##  9 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_P_pe… 0.92… mg/g 
## 10 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_tran… 2.359 mmol…
## # … with 2,623 more rows, and 3 more variables: value_type <fct>,
## #   replicates <chr>, original_name <chr>
```

`filter` can also be used to select observations for a specific string of text, such as a single genus:


```r
austraits$traits %>% 
  filter(grepl("Daviesia", species_name))
```

```
## # A tibble: 3,268 x 10
##    dataset_id species_name site_name observation_id trait_name value unit 
##    <chr>      <chr>        <chr>     <chr>          <chr>      <chr> <chr>
##  1 ANBG_2018  Daviesia ab… <NA>      ANBG_2018_0683 seed_mass  2.569 mg   
##  2 ANBG_2018  Daviesia bu… <NA>      ANBG_2018_0684 dispersal… aril  <NA> 
##  3 ANBG_2018  Daviesia bu… <NA>      ANBG_2018_0684 seed_shape reni… <NA> 
##  4 ANBG_2018  Daviesia co… <NA>      ANBG_2018_0685 seed_mass  7.77… mg   
##  5 ANBG_2018  Daviesia ep… <NA>      ANBG_2018_0686 seed_mass  69.6… mg   
##  6 ANBG_2018  Daviesia ge… <NA>      ANBG_2018_0687 dispersal… aril  <NA> 
##  7 ANBG_2018  Daviesia ge… <NA>      ANBG_2018_0687 seed_mass  11.6… mg   
##  8 ANBG_2018  Daviesia ge… <NA>      ANBG_2018_0687 seed_mass  9.9   mg   
##  9 ANBG_2018  Daviesia ge… <NA>      ANBG_2018_0687 seed_mass  11.4  mg   
## 10 ANBG_2018  Daviesia ge… <NA>      ANBG_2018_0687 seed_mass  10.4… mg   
## # … with 3,258 more rows, and 3 more variables: value_type <fct>,
## #   replicates <chr>, original_name <chr>
```

We can select particular columns from the whole or filtered dataset using `select`:


```r
austraits$traits %>% 
  select(species_name, observation_id, trait_name, value)
```

```
## # A tibble: 389,875 x 4
##    species_name                   observation_id trait_name        value   
##    <chr>                          <chr>          <chr>             <chr>   
##  1 Abelmoschus ficulneus          ANBG_2018_0001 seed_shape        spheroi…
##  2 Abrophyllum ornans             ANBG_2018_0002 seed_mass         46.345  
##  3 Abrotanella nivigena           ANBG_2018_0003 seed_mass         0.3125  
##  4 Abutilon indicum var. austral… ANBG_2018_0004 seed_mass         4.022   
##  5 Abutilon halophilum            ANBG_2018_0005 seed_mass         3.5625  
##  6 Abutilon otocarpum             ANBG_2018_0006 seed_shape        cordifo…
##  7 Acacia abbreviata              ANBG_2018_0007 seed_mass         6.15    
##  8 Acacia acinacea                ANBG_2018_0008 dispersal_append… aril    
##  9 Acacia acinacea                ANBG_2018_0008 seed_mass         7.313   
## 10 Acacia acinacea                ANBG_2018_0008 seed_shape        globose 
## # … with 389,865 more rows
```

## Extracting a particular trait or study

The file `austraits.R` also include pre-built functions for extracting a particular trait or study. The difference with the examples above, is that these functions also subset the other tables and elements of the data, not just the main data table. So we need to provide the entire `austraits` object and a `dataset_id`:


```r
traits_B14 <- extract_dataset(austraits, "Westoby_2014")
```

As wiith the the main `austraits` object, this new dataset has multiple elements:

```r
names(traits_B14)
```

```
## [1] "traits"        "sites"         "methods"       "excluded_data"
## [5] "taxonomy"      "definitions"
```
but the length of the datasets is smaller:

```r
count_rows <- function(obj) {c(nrow(obj$traits), nrow(obj$sites), nrow(obj$taxonomy))}
# main data
austraits %>% count_rows()
```

```
## [1] 389875   2716  21396
```

```r
# subset
traits_B14 %>% count_rows()
```

```
## [1] 2633  188  127
```

A similar function enables sub-setting to a particular trait, including the various other tables:


```r
traits_LS <- extract_trait(austraits, "leaf_width")
```

As with the the main `austraits` object, this new dataset has multiple elements:

```r
names(traits_LS)
```

```
## [1] "traits"        "sites"         "methods"       "excluded_data"
## [5] "taxonomy"      "definitions"   "sources"       "build_info"
```
but the length of the datasets is smaller:

```r
# main data
austraits %>% count_rows()
```

```
## [1] 389875   2716  21396
```

```r
# subset
traits_LS %>% count_rows()
```

```
## [1] 36544   503 13557
```

## Merging with taxonomic data

Limited taxonomic information is provided in the main table `data`, only `original_name` (the original name provided by the authors) and `species_name` (the aligned taxonomic name; see [species taxonomy](#species_taxonomy) for details). But further information is available in the table `taxonomy`:


```r
austraits$taxonomy
```

```
## # A tibble: 21,396 x 9
##    species_name genus family authority TPL_ID status APC_name APC_ID
##    <chr>        <chr> <chr>  <chr>     <chr>  <chr>  <chr>    <chr> 
##  1 Abelmoschus  Abel… Malva… <NA>      <NA>   <NA>   <NA>     <NA>  
##  2 Abelmoschus… Abel… Malva… (L.) Wig… kew-2… Accep… Abelmos… 28979…
##  3 Abelmoschus… Abel… Malva… (L.) Med… kew-2… Accep… Abelmos… 29010…
##  4 Abelmoschus… Abel… Malva… Medik.    kew-2… Accep… Abelmos… 29005…
##  5 Abroma augu… Abro… Malva… (L.) L.f. kew-2… Accep… Abroma … 29048…
##  6 Abrophyllum… Abro… Rouss… (F.Muell… kew-2… Accep… Abrophy… 28936…
##  7 Abrotanella… Abro… Compo… (F.Muell… gcc-3… Accep… Abrotan… 29005…
##  8 Abrus preca… Abrus Legum… L.        ild-2… Accep… Abrus p… 29193…
##  9 Abrus preca… Abrus Legum… Verdc.    ild-2… Accep… Abrus p… 29161…
## 10 Abutilon     Abut… Malva… <NA>      <NA>   <NA>   <NA>     <NA>  
## # … with 21,386 more rows, and 1 more variable: APNI_ID <chr>
```

You can merge this information in with the main dataset using the `species_name` as a an identifier linking both tables:


```r
traits2 <- austraits$traits %>% 
          left_join(by="species_name", austraits$taxonomy)

names(traits2)
```

```
##  [1] "dataset_id"     "species_name"   "site_name"      "observation_id"
##  [5] "trait_name"     "value"          "unit"           "value_type"    
##  [9] "replicates"     "original_name"  "genus"          "family"        
## [13] "authority"      "TPL_ID"         "status"         "APC_name"      
## [17] "APC_ID"         "APNI_ID"
```

Similarly we could merge in only family information by using `select` to reduce the second table before merging:


```r
traits2 <- austraits$traits %>% 
          left_join(by="species_name", select(austraits$taxonomy, species_name, family))

names(traits2)
```

```
##  [1] "dataset_id"     "species_name"   "site_name"      "observation_id"
##  [5] "trait_name"     "value"          "unit"           "value_type"    
##  [9] "replicates"     "original_name"  "family"
```

## Merging with location data

As with taxonomic information, limited information about location is provided in the main table `data`.  But further information is available in the table `context`:


```r
austraits$sites
```

```
## # A tibble: 2,716 x 4
##    dataset_id    site_name    site_property   value             
##    <chr>         <chr>        <chr>           <chr>             
##  1 Blackman_2010 Mt Field_dry latitude (deg)  -42.68            
##  2 Blackman_2010 Mt Field_dry longitude (deg) 146.6336          
##  3 Blackman_2010 Mt Field_dry elevation (m)   937               
##  4 Blackman_2010 Mt Field_dry description     Dry sclerophyll   
##  5 Blackman_2010 Mt Field_wet latitude (deg)  -42.68            
##  6 Blackman_2010 Mt Field_wet longitude (deg) 146.6336          
##  7 Blackman_2010 Mt Field_wet elevation (m)   937               
##  8 Blackman_2010 Mt Field_wet description     Montane rainforest
##  9 Blackman_2010 Sandy Bay    latitude (deg)  -42.907           
## 10 Blackman_2010 Sandy Bay    longitude (deg) 147.3239          
## # … with 2,706 more rows
```

As with the main table of trait data, this information is in long format. Depending on the study, a variety of contextual information may have been provided, including


```r
austraits$sites$site_property %>% table() %>% sort(decreasing=TRUE) %>% .[1:10]
```

```
## .
##     latitude (deg)    longitude (deg)        description 
##                445                445                341 
##      elevation (m)            geology      rainfall (mm) 
##                155                107                 91 
## soil_total_P (ppm)                age               date 
##                 69                 60                 60 
##                don 
##                 60
```

In this example we'll just focus on location details. Before merging in with the main data table let's convert this data from long to wide format:


```r
sites <- austraits$sites %>% 
  filter(site_property %in%  c("longitude (deg)","latitude (deg)")) %>% 
  spread(site_property, value)
sites
```

```
## # A tibble: 445 x 4
##    dataset_id    site_name                `latitude (deg)` `longitude (deg…
##    <chr>         <chr>                    <chr>            <chr>           
##  1 Blackman_2010 Buckland                 -42.61           147.707         
##  2 Blackman_2010 Mt Field_dry             -42.68           146.6336        
##  3 Blackman_2010 Mt Field_wet             -42.68           146.6336        
##  4 Blackman_2010 Ridgeway                 -42.915          147.2931        
##  5 Blackman_2010 Sandy Bay                -42.907          147.3239        
##  6 Bragg_2002    Ku-ring-gai Chase Natio… -33.6938889      151.1430556     
##  7 Briggs_2010   Terrick Terrick Nationa… -36.1738         144.227         
##  8 Burrows_2001  The Rock                 -35.2666667      147.0666667     
##  9 Butler_2011   arid                     -18.7            141.79          
## 10 Butler_2011   dry                      -18.3            145.49          
## # … with 435 more rows
```

We can now merge in with the main data table using combinations of `dataset_id` and `site_name` as identifiers from both tables:


```r
traits_sites <- left_join(by=c("dataset_id", "site_name"), austraits$traits, sites)

names(traits_sites)
```

```
##  [1] "dataset_id"      "species_name"    "site_name"      
##  [4] "observation_id"  "trait_name"      "value"          
##  [7] "unit"            "value_type"      "replicates"     
## [10] "original_name"   "latitude (deg)"  "longitude (deg)"
```

## Removing suspected duplicates

AusTraits compiles data from many different sources and not all of these will be unique. Data from one study may be repeated in another. We have provided a workflow for removing suspected duplicate data via the function `remove_suspected_duplicates`. The function takes as an argument the entire `austraits` object and returns a modified object in which suspected duplicates have been moved from the main `traits` table to the `excluded_data` table, noting  the `observartion_id` of the matching data in the error column. To use this function you mast pass in the entire `austraits` object:



```r
austraits_deduped <- remove_suspected_duplicates(austraits)
```

Suspected duplicates are identified based on identical matches between the variables `species_name`, `trait_name`, and `value`.  Priority is given to older sources, as indicated by the year in the `dataset_id`.

Checking the number of rows in original dataset and modified dataset we can see that 70240 records have been removed from `traits`:


```r
c(traits = nrow(austraits$traits), 
  excluded = nrow(austraits$excluded_data), 
  total = nrow(austraits$traits)+ nrow(austraits$excluded_data))
```

```
##   traits excluded    total 
##   389875   168097   557972
```

```r
c(traits = nrow(austraits_deduped$traits), 
    excluded = nrow(austraits_deduped$excluded_data), 
  total = nrow(austraits_deduped$traits)+ nrow(austraits_deduped$excluded_data))
```

```
##   traits excluded    total 
##   319635   238337   557972
```

## Converting from long to wide format

As discussed above, the main data table in `austraits` is distributed in long format. A long format has each measurement on a different row, and just a single column for all the value. By contrast, a dataset in wide format has data for each different trait in it's own column. The reason we use long format is mainly for efficiency. We have data from 95 different traits, so in wide format we'd need at least this many columns. With many species, the file size gets very large very quickly. Other benefits of long format are that we can include columns for `units`, `value_type`, and `replicates`, which we could not in wide format (without created 3 more columns for each traits). 

For many analyses, however, you'll want to use wide format. We have therefore provided the ability to convert between the two formats. In wide format, each trait appears in it's own column and measurements from the same observation (ideally individual, but in floras may be a species) appear on the same row.  

As the entire austraits dataset is quite large, we recommend first sub-setting the data before spreading from long to wide format. You may also need to reduce the number of measurements in any one observation (see next section). For this first example we'll use the dataset `Westoby_2014`. 

First extract the trait data from the `Westoby_2014` study:


```r
data <- austraits$traits %>% 
  filter(dataset_id == "Westoby_2014")
data
```

```
## # A tibble: 2,633 x 10
##    dataset_id species_name site_name observation_id trait_name value unit 
##    <chr>      <chr>        <chr>     <chr>          <chr>      <chr> <chr>
##  1 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… bark_thic… 0.21… mm   
##  2 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_area  6575  mm2  
##  3 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_area… 1215… m2/m2
##  4 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_dark… 1.96  umol…
##  5 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_dark… 0.02… umol…
##  6 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_delt… -28.… per …
##  7 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_K_pe… 8.63… mg/g 
##  8 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_N_pe… 25.1… mg/g 
##  9 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_P_pe… 0.92… mg/g 
## 10 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_tran… 2.359 mmol…
## # … with 2,623 more rows, and 3 more variables: value_type <fct>,
## #   replicates <chr>, original_name <chr>
```
Now use the inbuilt function `spread_trait_data` to convert from long to wide:


```r
traits_spread <- spread_trait_data(data)
```
This function spreads not only the data in the column `value`, but also other columns. The returned object is a list with a number of wide tables:


```r
names(traits_spread)
```

```
## [1] "value"      "unit"       "value_type" "replicates"
```
Looking at each of these we see a table in wide format (i.e. with each trait being a column), as well as identifying columns at the start:

```r
traits_spread$value
```

```
## # A tibble: 140 x 27
##    dataset_id species_name site_name observation_id original_name
##    <chr>      <chr>        <chr>     <chr>          <chr>        
##  1 Westoby_2… Acacia aneu… Fowlers … Westoby_2014_… Acacia aneura
##  2 Westoby_2… Acacia coll… Round Hi… Westoby_2014_… Acacia colle…
##  3 Westoby_2… Acacia cras… Cardwell  Westoby_2014_… Acacia crass…
##  4 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… Acacia dealb…
##  5 Westoby_2… Acacia deal… Longley   Westoby_2014_… Acacia dealb…
##  6 Westoby_2… Acacia flav… Cardwell  Westoby_2014_… Acacia flave…
##  7 Westoby_2… Acacia flav… Princess… Westoby_2014_… Acacia flave…
##  8 Westoby_2… Acacia havi… Round Hi… Westoby_2014_… Acacia havil…
##  9 Westoby_2… Acacia holo… Claravil… Westoby_2014_… Acacia holos…
## 10 Westoby_2… Acacia lept… Princess… Westoby_2014_… Acacia lepto…
## # … with 130 more rows, and 22 more variables: bark_thickness <chr>,
## #   leaf_area <chr>, leaf_area_per_sapwood_area <chr>,
## #   leaf_dark_respiration_per_area <chr>,
## #   leaf_dark_respiration_per_dry_mass <chr>, leaf_delta13C <chr>,
## #   leaf_K_per_dry_mass <chr>, leaf_N_per_dry_mass <chr>,
## #   leaf_P_per_dry_mass <chr>, leaf_specific_conductivity <chr>,
## #   leaf_transpiration <chr>, modulus_of_elasticity <chr>,
## #   modulus_of_rupture <chr>, photosynthetic_rate_per_area <chr>,
## #   photosynthetic_rate_per_dry_mass <chr>, plant_height <chr>,
## #   sapwood_specific_conductivity <chr>, specific_leaf_area <chr>,
## #   water_potential_midday <chr>, water_potential_predawn <chr>,
## #   water_use_efficiency <chr>, wood_density <chr>
```
The other tables have an identical format


```r
traits_spread$unit
```

```
## # A tibble: 140 x 27
##    dataset_id species_name site_name observation_id original_name
##    <chr>      <chr>        <chr>     <chr>          <chr>        
##  1 Westoby_2… Acacia aneu… Fowlers … Westoby_2014_… Acacia aneura
##  2 Westoby_2… Acacia coll… Round Hi… Westoby_2014_… Acacia colle…
##  3 Westoby_2… Acacia cras… Cardwell  Westoby_2014_… Acacia crass…
##  4 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… Acacia dealb…
##  5 Westoby_2… Acacia deal… Longley   Westoby_2014_… Acacia dealb…
##  6 Westoby_2… Acacia flav… Cardwell  Westoby_2014_… Acacia flave…
##  7 Westoby_2… Acacia flav… Princess… Westoby_2014_… Acacia flave…
##  8 Westoby_2… Acacia havi… Round Hi… Westoby_2014_… Acacia havil…
##  9 Westoby_2… Acacia holo… Claravil… Westoby_2014_… Acacia holos…
## 10 Westoby_2… Acacia lept… Princess… Westoby_2014_… Acacia lepto…
## # … with 130 more rows, and 22 more variables: bark_thickness <chr>,
## #   leaf_area <chr>, leaf_area_per_sapwood_area <chr>,
## #   leaf_dark_respiration_per_area <chr>,
## #   leaf_dark_respiration_per_dry_mass <chr>, leaf_delta13C <chr>,
## #   leaf_K_per_dry_mass <chr>, leaf_N_per_dry_mass <chr>,
## #   leaf_P_per_dry_mass <chr>, leaf_specific_conductivity <chr>,
## #   leaf_transpiration <chr>, modulus_of_elasticity <chr>,
## #   modulus_of_rupture <chr>, photosynthetic_rate_per_area <chr>,
## #   photosynthetic_rate_per_dry_mass <chr>, plant_height <chr>,
## #   sapwood_specific_conductivity <chr>, specific_leaf_area <chr>,
## #   water_potential_midday <chr>, water_potential_predawn <chr>,
## #   water_use_efficiency <chr>, wood_density <chr>
```
...etc

The new dataset can then be used to plot traits against each other, e.g.

```r
traits_spread$value %>%
  mutate(specific_leaf_area = as.numeric(specific_leaf_area), 
         photosynthetic_rate_per_area = as.numeric(photosynthetic_rate_per_area)) %>%
  ggplot(aes(x=specific_leaf_area, y = photosynthetic_rate_per_area, colour =site_name)) +
  geom_point()
```

![](/Users/dfalster/Dropbox/_research/packages/austraits/austraits/vignettes/figures/documentation_unnamed-chunk-56-1.png)<!-- -->

There is also a function to convert back from wide to long format:


```r
traits2 <- gather_trait_data(traits_spread, austraits$definitions)
```

This manipulation should recover the original dataset.

Original:

```r
traits2
```

```
## # A tibble: 2,633 x 10
##    dataset_id species_name site_name observation_id trait_name value unit 
##    <chr>      <chr>        <chr>     <chr>          <chr>      <chr> <chr>
##  1 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… bark_thic… 0.21… mm   
##  2 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_area  6575  mm2  
##  3 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_area… 1215… m2/m2
##  4 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_dark… 1.96  umol…
##  5 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_dark… 0.02… umol…
##  6 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_delt… -28.… per …
##  7 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_K_pe… 8.63… mg/g 
##  8 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_N_pe… 25.1… mg/g 
##  9 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_P_pe… 0.92… mg/g 
## 10 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_tran… 2.359 mmol…
## # … with 2,623 more rows, and 3 more variables: value_type <chr>,
## #   replicates <chr>, original_name <chr>
```
After conversion

```r
data
```

```
## # A tibble: 2,633 x 10
##    dataset_id species_name site_name observation_id trait_name value unit 
##    <chr>      <chr>        <chr>     <chr>          <chr>      <chr> <chr>
##  1 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… bark_thic… 0.21… mm   
##  2 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_area  6575  mm2  
##  3 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_area… 1215… m2/m2
##  4 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_dark… 1.96  umol…
##  5 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_dark… 0.02… umol…
##  6 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_delt… -28.… per …
##  7 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_K_pe… 8.63… mg/g 
##  8 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_N_pe… 25.1… mg/g 
##  9 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_P_pe… 0.92… mg/g 
## 10 Westoby_2… Acacia deal… Bothwell  Westoby_2014_… leaf_tran… 2.359 mmol…
## # … with 2,623 more rows, and 3 more variables: value_type <fct>,
## #   replicates <chr>, original_name <chr>
```
Just to be sure we can run a check:

```r
all.equal(data, traits2)
```

```
## [1] "Incompatible type for column `value_type`: x factor, y character"
```

Note that the function `gather_trait_data` also needs the element `austraits$definitions` to run successfully. 

### Datasets with multiple measurements per observation

The above example works easily because there is only a single measurement of each trait per `observation_id`. But this is not always the case. Many datasets include multiple records per `observation_id`, for example studies reporting multiple `value_types` per `observation_id`. An example is the `AusGrass_2014` datasets:


```r
traits <- austraits$traits %>% 
  filter(dataset_id == "AusGrass_2014")
traits
```

```
## # A tibble: 5,528 x 10
##    dataset_id species_name site_name observation_id trait_name value unit 
##    <chr>      <chr>        <chr>     <chr>          <chr>      <chr> <chr>
##  1 AusGrass_… Stipa aquar… <NA>      AusGrass_2014… leaf_leng… 300   mm   
##  2 AusGrass_… Stipa aquar… <NA>      AusGrass_2014… leaf_leng… 600   mm   
##  3 AusGrass_… Stipa aquar… <NA>      AusGrass_2014… leaf_width 1     mm   
##  4 AusGrass_… Stipa aquar… <NA>      AusGrass_2014… leaf_width 2     mm   
##  5 AusGrass_… Stipa aquar… <NA>      AusGrass_2014… life_hist… pere… <NA> 
##  6 AusGrass_… Stipa aquar… <NA>      AusGrass_2014… plant_hei… 0.5   m    
##  7 AusGrass_… Stipa aquar… <NA>      AusGrass_2014… plant_hei… 1     m    
##  8 AusGrass_… Stipa aquar… <NA>      AusGrass_2014… seed_leng… 2     mm   
##  9 AusGrass_… Stipa aquar… <NA>      AusGrass_2014… seed_leng… 2.5   mm   
## 10 AusGrass_… Stipa arist… <NA>      AusGrass_2014… leaf_leng… 200   mm   
## # … with 5,518 more rows, and 3 more variables: value_type <fct>,
## #   replicates <chr>, original_name <chr>
```
Note the dataset includes multiple value_types: `expert_min` and `expert_max`. If you try running `spread_trait_data` on this data it will fail: 


```r
traits_spread <- spread_trait_data(traits)
```
```
## Error: Duplicate identifiers for rows (2929, 2930), (2936, 2937), (3015, 3016), (2975, 2976), ....
```

So before converting from long to wide we use an additional function called `bind_trait_values` to collapse all the different measurements for a given `trait_name` and `observation_id` into a single cell:


```r
traits_bind <- bind_trait_values(traits)
```
This function binds data from multiple rows together, separating with the string ``--`. This occurs for columns `value`, `value_type`, and `replicates`. Looking at the table we can see the bound values:


```r
traits_bind %>% 
  filter(grepl("--", value_type)) %>% 
  select(-dataset_id, -site_name, -original_name)
```

```
## # A tibble: 2,232 x 7
##    species_name observation_id trait_name value unit  value_type replicates
##    <chr>        <chr>          <chr>      <chr> <chr> <chr>      <chr>     
##  1 Stipa aquar… AusGrass_2014… leaf_leng… 300-… mm    expert_mi… NA--NA    
##  2 Stipa aquar… AusGrass_2014… leaf_width 1--2  mm    expert_mi… NA--NA    
##  3 Stipa aquar… AusGrass_2014… plant_hei… 0.5-… m     expert_mi… NA--NA    
##  4 Stipa aquar… AusGrass_2014… seed_leng… 2--2… mm    expert_mi… NA--NA    
##  5 Stipa arist… AusGrass_2014… leaf_leng… 200-… mm    expert_mi… NA--NA    
##  6 Stipa arist… AusGrass_2014… leaf_width 3--6  mm    expert_mi… NA--NA    
##  7 Stipa arist… AusGrass_2014… plant_hei… 1--2  m     expert_mi… NA--NA    
##  8 Stipa arist… AusGrass_2014… seed_leng… 3--4… mm    expert_mi… NA--NA    
##  9 Stipa bigen… AusGrass_2014… leaf_leng… 200-… mm    expert_mi… NA--NA    
## 10 Stipa bigen… AusGrass_2014… leaf_width 1--3  mm    expert_mi… NA--NA    
## # … with 2,222 more rows
```

Using this new dataset we can now go from long to wide:


```r
traits_spread <- spread_trait_data(traits_bind)
```

The bound data values are carried over to the new dataset:

```r
traits_spread$value
```

```
## # A tibble: 892 x 13
##    dataset_id species_name site_name observation_id original_name
##    <chr>      <chr>        <chr>     <chr>          <chr>        
##  1 AusGrass_… Agrostis ad… <NA>      AusGrass_2014… Lachnagrosti…
##  2 AusGrass_… Agrostis ae… <NA>      AusGrass_2014… Lachnagrosti…
##  3 AusGrass_… Agrostis ae… <NA>      AusGrass_2014… Lachnagrosti…
##  4 AusGrass_… Agrostis av… <NA>      AusGrass_2014… Lachnagrosti…
##  5 AusGrass_… Agrostis bi… <NA>      AusGrass_2014… Lachnagrosti…
##  6 AusGrass_… Agrostis bi… <NA>      AusGrass_2014… Lachnagrosti…
##  7 AusGrass_… Agrostis bi… <NA>      AusGrass_2014… Lachnagrosti…
##  8 AusGrass_… Agrostis bi… <NA>      AusGrass_2014… Lachnagrosti…
##  9 AusGrass_… Agrostis bi… <NA>      AusGrass_2014… Lachnagrosti…
## 10 AusGrass_… Agrostis bi… <NA>      AusGrass_2014… Lachnagrosti…
## # … with 882 more rows, and 8 more variables: flowering_time <chr>,
## #   growth_habit <chr>, leaf_length <chr>, leaf_width <chr>,
## #   life_history <chr>, photosynthetic_pathway <chr>, plant_height <chr>,
## #   seed_length <chr>
```

As previously, we can also convert back again:


```r
traits_bind2 <- gather_trait_data(traits_spread, austraits$definitions)
```

Thsi gets us back to the intermediate dataset

```r
all.equal(traits_bind, traits_bind2)
```

```
## [1] TRUE
```

And then finally we can separate out the values that have been bound together:


```r
traits2 <- separate_trait_values(traits_bind2, austraits$definitions)
```

We should now have a dataset identical to our original. Just to be sure we can run a check:

```r
all.equal(traits, traits2)
```

```
## [1] TRUE
```

Using pipes we can put all these steps together to short the full range of steps:
 

```r
traits %>% 
 bind_trait_values() %>% 
 spread_trait_data() %>% 
 gather_trait_data() %>% 
 separate_trait_values(austraits$definitions) -> traits2

all.equal(traits, traits2)
```

```
## [1] TRUE
```

## Categorical vs numerical traits 

List of categorical traits:


```r
traits_all <- austraits$traits$trait_name %>% unique()  %>% sort()

traits_all[trait_is_categorical(traits_all, austraits$definitions)]
```

```
##  [1] "dispersal_appendage"    "dispersal_syndrome"    
##  [3] "fire_response"          "fire-cued_seeding"     
##  [5] "flower_colour"          "flowering_time"        
##  [7] "fruit_type"             "fruit_type_botany"     
##  [9] "fruit_type_function"    "fruiting_time"         
## [11] "glaucous"               "growth_habit"          
## [13] "leaf_arrangement"       "leaf_compoundness"     
## [15] "leaf_margin"            "leaf_phenology"        
## [17] "leaf_shape"             "leaf_type"             
## [19] "life_history"           "lifespan"              
## [21] "nitrogen_fixing"        "photosynthetic_pathway"
## [23] "plant_growth_form"      "regen_strategy"        
## [25] "root_structure"         "seed_shape"            
## [27] "serotiny"               "storage_organ"         
## [29] "woodiness"
```

list of numerical traits:


```r
traits_all[trait_is_numeric(traits_all, austraits$definitions)]
```

```
##  [1] "bark_mass_area"                             
##  [2] "bark_thickness"                             
##  [3] "basal_diameter"                             
##  [4] "branch_mass_fraction"                       
##  [5] "diaspore_mass"                              
##  [6] "fruit_length"                               
##  [7] "fruit_width"                                
##  [8] "genome_size"                                
##  [9] "leaf_area"                                  
## [10] "leaf_area_per_sapwood_area"                 
## [11] "leaf_area_ratio"                            
## [12] "leaf_C_per_dry_mass"                        
## [13] "leaf_cell_wall_fraction"                    
## [14] "leaf_cell_wall_N"                           
## [15] "leaf_cell_wall_N_fraction"                  
## [16] "leaf_chlorophyll_per_dry_mass"              
## [17] "leaf_CN_ratio"                              
## [18] "leaf_dark_respiration_per_area"             
## [19] "leaf_dark_respiration_per_dry_mass"         
## [20] "leaf_delta13C"                              
## [21] "leaf_delta15N"                              
## [22] "leaf_dry_mass"                              
## [23] "leaf_dry_matter_content"                    
## [24] "leaf_fracture_force"                        
## [25] "leaf_K_per_area"                            
## [26] "leaf_K_per_dry_mass"                        
## [27] "leaf_length"                                
## [28] "leaf_lifespan"                              
## [29] "leaf_mass_fraction"                         
## [30] "leaf_N_per_area"                            
## [31] "leaf_N_per_dry_mass"                        
## [32] "leaf_P_per_area"                            
## [33] "leaf_P_per_dry_mass"                        
## [34] "leaf_photosynthetic_nitrogen_use_efficiency"
## [35] "leaf_photosynthetic_water_use_efficiency"   
## [36] "leaf_saturated_water_content_per_mass"      
## [37] "leaf_specific_conductivity"                 
## [38] "leaf_thickness"                             
## [39] "leaf_toughness"                             
## [40] "leaf_transpiration"                         
## [41] "leaf_water_content_per_area"                
## [42] "leaf_water_content_per_mass"                
## [43] "leaf_width"                                 
## [44] "modulus_of_elasticity"                      
## [45] "modulus_of_rupture"                         
## [46] "photosynthetic_rate_per_area"               
## [47] "photosynthetic_rate_per_area_saturated"     
## [48] "photosynthetic_rate_per_dry_mass"           
## [49] "plant_height"                               
## [50] "plant_width"                                
## [51] "root_mass_fraction"                         
## [52] "root_wood_density"                          
## [53] "sapwood_specific_conductivity"              
## [54] "seed_breadth"                               
## [55] "seed_length"                                
## [56] "seed_mass"                                  
## [57] "seed_mass_reserve"                          
## [58] "seed_P_concentration"                       
## [59] "seed_volume"                                
## [60] "seed_width"                                 
## [61] "specific_leaf_area"                         
## [62] "stomatal_conductance_per_area"              
## [63] "water_potential_midday"                     
## [64] "water_potential_predawn"                    
## [65] "water_use_efficiency"                       
## [66] "wood_density"
```

# Appendices

## Acknowledgements

## Primary sources

AusTraits is built from the following primary and secondary sources:

- Australian National Botanic Garden. _The National Seed Bank_.2018.  url: [http://www.anbg.gov.au/gardens/living/seedbank/](http://www.anbg.gov.au/gardens/living/seedbank/).
- T. Angevin. "Unpublished data: Trait data collected via Honoursproject at La Trobe University". 2010.
- Y. Alfonso and B. Simon. _AusGrass2: Grasses of Australia_.2014.  url: [http://ausgrass2.myspecies.info/](http://ausgrass2.myspecies.info/).
- B. A. Barlow, H. T. Clifford, A. S. George, and A. K. A.McCusker. _Flora of Australia (online)_. 1981.  url: [http://www.environment.gov.au/biodiversity/abrs/online-resources/flora/main/](http://www.environment.gov.au/biodiversity/abrs/online-resources/flora/main/).
- A. Bean. "A revision of Baeckea (Myrtaceae) in easternAustralia, Malesia and south-east Asia".   _Telopea_ 7 (1997),pp. 245-268.  doi: [10.1.1.520.6315&rep=rep1&type=pdf](https://doi.org/10.1.1.520.6315%26rep%3Drep1%26type%3Dpdf).
- Forest Department of Burma. _Standard nomenclature of forestplants, Burma, including commercial timbers_. Forest Research andTraining Circle, Forest Department, Burma, 1974.
- C. J. Blackman, T. J. Brodribb, and G. J. Jordan. "Leafhydraulic vulnerability is related to conduit dimensions anddrought resistance across a diverse range of woody angiosperms".  _New Phytologist_ 188.4 (2010), pp. 1113-1123.  doi: [10.1111/j.1469-8137.2010.03439.x](https://doi.org/10.1111%2Fj.1469-8137.2010.03439.x).
- E. Bolza. _Properties and uses of 175 timber species from PapuaNew Guinea and West Irian_. Victoria (Australia) CSIRO, Div. ofBuilding Research, 1975.
- J. G. Bragg and M. Westoby. "Leaf size and foraging for lightin a sclerophyll woodland".   _Functional Ecology_ 16 (2002),pp. 633-639.  doi: [10.1046/j.1365-2435.2002.00661.x](https://doi.org/10.1046%2Fj.1365-2435.2002.00661.x).
- Brisbane Rainforest Action & Information Network. _Traitmeasurements for Australian rainforest species_. 2016.  url: [http://www.brisrain.org.au/](http://www.brisrain.org.au/).
- A. L. Briggs and J. W. Morgan. "Seed characteristics and soilsurface patch type interact to affect germination of semi-aridwoodland species".   _Plant Ecol_ 212 (2010), pp. 91-103.  doi: [10.1007/s11258-010-9806-x](https://doi.org/10.1007%2Fs11258-010-9806-x).
- J. Brock and A. Dunlop. _Native plants of northern Australia_.Frenchs Forest, N.S.W.: Reed New Holland, 1993. ISBN:9781877069246.
- G. E. Burrows. "Comparative anatomy of the photosyntheticorgans of 39 xeromorphic species from subhumid New South Wales,Australia".   _International Journal of Plant Sciences_ 162(2001), pp. 411-430.  doi: [10.1086/319579](https://doi.org/10.1086%2F319579).
- D. W. Butler, S. M. Gleason, I. Davidson, Y. Onoda, and M.Westoby. "Safety and streamlining of woody shoots in wind: Anempirical study across 39 species in tropical Australia".   _NewPhytologist_ 193 (2011), pp. 137-149.  doi: [10.1111/j.1469-8137.2011.03887.x](https://doi.org/10.1111%2Fj.1469-8137.2011.03887.x).
- CAB International. _Forestry Compendium_. 2009.  url: [http://www.cabi.org/fc/](http://www.cabi.org/fc/).
- J. A. Catford, W. K. Morris, P. A. Vesk, C. J. Gippel, and B.J. Downes. "Species and environmental characteristics point toflow regulation and drought as drivers of riparian plantinvasion".   _Diversity and Distributions_ 20 (2014), pp.1084-1096.  doi: [10.1111/ddi.12225](https://doi.org/10.1111%2Fddi.12225).
- G. T. Chandler, M. D. Crisp, L. W. Cayzer, and R. J. Bayer."Monograph of Gastrolobium (Fabaceae: Mirbelieae)".  _Australian Systematic Botany_ 15 (2002), p. 619.  doi: [10.1071/sb01010](https://doi.org/10.1071%2Fsb01010).
- J. Chave, D. Coomes, S. Jansen, S. L. Lewis, N. G. Swenson, andA. E. Zanne. "Towards a worldwide wood economics spectrum".  _Ecology Letters_ 12 (2009), pp. 351-366.  doi: [10.1111/j.1461-0248.2009.01285.x](https://doi.org/10.1111%2Fj.1461-0248.2009.01285.x).
- S. Chen and A. T. Moles. "A mammoth mouthful? A test of theidea that larger animals ingest larger seeds".   _Global Ecologyand Biogeography_ 24 (2015), pp. 1269-1280.  doi: [10.1111/geb.12346](https://doi.org/10.1111%2Fgeb.12346).
- Chen, S.-C., Cornwell, W., Z. H. &. Moles, and A. T. "Plantsshow more flesh in the tropics: variation in fruit type alonglatitudinal and climatic gradients.".   _Ecography_ 40 (2017),pp. 531-538.  doi: [abs/10.1111/ecog.02010](https://doi.org/abs%2F10.1111%2Fecog.02010).
- N. N. Chính. _Vietnam forest trees_. Forest Inventory andPlanning Institute Agricultural Publishing House Hanoi, 1999.ISBN: 19996775635.
- R. J. Chinnock. _Eremophila and allied genera: A monograph ofthe plant family Myoporaceae_. Dural, NSW: Rosenberg, 2007. ISBN:9781877058165.
- M. Chudnoff. _Tropical timbers of the world_. US Department ofAgriculture, Forest Service, 1984, p. 472. ISBN: 3935638825.
- The French agricultural research & international cooperationorganization (CIRAD). _Wood density data_. 2009.  url: [http://www.cirad.fr/](http://www.cirad.fr/).
- P. J. Clarke, M. J. Lawes, B. P. Murphy, J. Russell-Smith, C.E. Nano, R. Bradstock, N. J. Enright, J. B. Fontaine, C. R.Gosper, I. Radford, J. J. Midgley, and R. M. Gunton. "A synthesisof postfire recovery traits of woody plants in Australianecosystems".   _Science of The Total Environment_ 534 (2015),pp. 31-42.  doi: [10.1016/j.scitotenv.2015.04.002](https://doi.org/10.1016%2Fj.scitotenv.2015.04.002).
- W. Clayton, M. Vorontsova, and K. H. &. H. Williamson._GrassBase - The online world grass flora_. 2006.  url: [http://www.kew.org/data/grasses-db.html](http://www.kew.org/data/grasses-db.html).
- W. Cooper and W. T. Cooper. _Fruits of the Australian tropicalrainforest_. Nokomis Editions, 2004. ISBN: 9780958174213.
- Centre for Plant Biodiversity Research. _EUCLID 2.0: Eucalyptsof Australia_. 2002.
- L. Craven. "A taxonomic Revision of Calytrix Labill.(Myrtaceae)".   _Brunonia_ 10 (1987), pp. 1-138.  doi: [10.1071/bru9870001](https://doi.org/10.1071%2Fbru9870001).
- L. A. Craven, B. J. Lepschi, and K. J. Cowley. "Melaleuca(Myrtaceae) of Western Australia: Five new species, three newcombinations, one new name and a new state record.".   _Nuytsia_20 (2010), pp. 27-36.
- M. D. Crisp, L. Cayzer, G. T. Chandler, and L. G. Cook. "Amonograph of Daviesia (Mirbelieae, Faboideae, Fabaceae)".  _Phytotaxa_ 300 (2017), pp. 1-308.  doi: [10.11646/phytotaxa.300.1.1](https://doi.org/10.11646%2Fphytotaxa.300.1.1).
- E. Cross. "Unpublished data: Woodlands of western Victoriatrait database". 2011.
- S. A. Cunningham, B. Summerhayes, and M. Westoby. "Evolutionarydivergences in leaf structure and chemistry, comparing rainfalland soil nutrient gradients".   _Ecological Monographs_ 69(1999), pp. 569-588.  doi: [10.1890/0012-9615(1999)069[0569:EDILSA]2.0.CO;2](https://doi.org/10.1890%2F0012-9615%281999%29069%5B0569%3AEDILSA%5D2.0.CO%3B2).
- M. D. Denton, E. J. Veneklaas, F. M. Freimoser, and H. Lambers."Banksia species (Proteaceae) from severelyphosphorus-impoverished soils exhibit extreme efficiency in theuse and re-mobilization of phosphorus".   _Plant, Cell andEnvironment_ 30 (2007), pp. 1557-1565.  doi: [10.1111/j.1365-3040.2007.01733.x](https://doi.org/10.1111%2Fj.1365-3040.2007.01733.x).
- H. E. Desch and J. M. Dinwoodie. _Timber structure, properties,conversion and use_. Palgrave Macmillan, 1996. ISBN: 156022861X.
- M. Dimitri and J. Biloni. _Libro del árbol. Vol 1. Esenciasforestales indígenas de la Argentina; and Vol 2. EsenciasForestales indígenas de la Argentina de aplicación industrial_.Editorial Celulosa Argentina S. A., 1973.
- D. H. Duncan. "Unpublished data: Leaf anatomy of Australianplant species". 1998.
- J. M. Dwyer and D. C. Laughlin. "Constraints on traitcombinations explain climatic drivers of biodiversity: Theimportance of trait covariance in community assembly".  _Ecology Letters_ 20.7 (2017), pp. 872-882.  doi: [10.1111/ele.12781](https://doi.org/10.1111%2Fele.12781).
- D. Eamus and H. Prichard. "A cost-benefit analysis of leaves offour Australian savanna species".   _Tree Physiology_ 18 (1998),pp. 537-545.  doi: [10.1093/treephys/18.8-9.537](https://doi.org/10.1093%2Ftreephys%2F18.8-9.537).
- D. Eamus, B. Myers, G. Duff, and D. Williams. "Seasonal changesin photosynthesis of eight savanna tree species".   _TreePhysiology_ 19 (1999), pp. 665-671.  doi: [10.1093/treephys/19.10.665](https://doi.org/10.1093%2Ftreephys%2F19.10.665).
- C. Edwards, J. Read, and G. D. Sanson. "Characterisingsclerophylly: some mechanical properties of leaves from heath andforest".   _Oecologia_ 123.2 (2000), pp. 158-167.  doi: [10.1007/s004420051001](https://doi.org/10.1007%2Fs004420051001).
- C. Edwards, G. D. Sanson, N. Aranwela, and J. Read."Relationships between sclerophylly, leaf biomechanical propertiesand leaf anatomy in some Australian heath and forest species".  _Plant Biosystems - An International Journal Dealing with allAspects of Plant Biology_ 134 (2000), pp. 261-277.  doi: [10.1080/11263500012331350445](https://doi.org/10.1080%2F11263500012331350445).
- D. S. Falster and M. Westoby. "Alternative height strategiesamong 45 dicot rain forest species from tropical Queensland,Australia".   _Journal of Ecology_ 93 (2005), pp. 521-535.  doi: [10.1111/j.0022-0477.2005.00992.x](https://doi.org/10.1111%2Fj.0022-0477.2005.00992.x).
- D. S. Falster and M. Westoby. "Tradeoffs between height growthrate, stem persistence and maximum height among plant species in apost-fire succession".   _Oikos_ 111 (2005), pp. 57-66.  doi: [10.1111/j.0030-1299.2005.13383.x](https://doi.org/10.1111%2Fj.0030-1299.2005.13383.x).
- J. H. Flynn and C. D. Holder. _A guide to useful woods of theworld_. 2nd. Forest Products Society, 2001. ISBN: 1892529157.
- C. R. Fonseca, J. M. Overton, B. Collins, and M. Westoby."Shifts in trait-combinations along rainfall and phosphorusgradients".   _Journal of Ecology_ 88 (2000), pp. 964-977.  doi: [10.1046/j.1365-2745.2000.00506.x](https://doi.org/10.1046%2Fj.1365-2745.2000.00506.x).
- P. I. Forster. "A taxonomic revision of Alyxia (Apocynaceae) inAustralia".   _Australian Systematic Botany_ 5 (1992), pp.547-580.  doi: [10.1071/sb9920547](https://doi.org/10.1071%2Fsb9920547).
- P. I. Forster. "New names and combinations in Marsdenia(Asclepiadaceae: Marsdenieae) from Asia and Malesia (excludingPapusia)".   _Australian Systematic Botany_ 8 (1995), pp.691-701.  doi: [10.1071/sb9950691](https://doi.org/10.1071%2Fsb9950691).
- R. V. Gallagher, M. R. Leishman, J. T. Miller, C. Hui, D. M.Richardson, J. Suda, and P. Trávníček. "Invasiveness in introducedAustralian acacias: The role of species traits and genome size".  _Diversity and Distributions_ 17 (2011), pp. 884-897.  doi: [10.1111/j.1472-4642.2011.00805.x](https://doi.org/10.1111%2Fj.1472-4642.2011.00805.x).
- E. Goble-Garratt, D. Bell, and W. Loneragan. "Floristic andleaf structure patterns along a shallow elevational gradient".  _Australian Journal of Botany_ 29 (1981), p. 329.  doi: [10.1071/bt9810329](https://doi.org/10.1071%2Fbt9810329).
- B. Goldsmith and D. Carter. _The indigenous timbers ofZimbabwe_. 9. 1981, p. 406. ISBN: 0869232940.
- P. K. Groom and B. B. Lamont. "Reproductive ecology ofnon-sprouting and re-sprouting Hakea species (Proteaceae) insouthwestern Australia.". In: _Gondwanan Heritage_. Ed. by J. C.S.D. Hopper M. Harvey and A. George. Surrey Beatty, ChippingNorton, 1996, pp. 239-248.  doi: [unknown](https://doi.org/unknown).
- P. K. Groom and B. B. Lamont. "Phosphorus accumulation inProteaceae seeds: a synthesis".   _Plant and Soil_ 334.1-2(2009), pp. 61-72.  doi: [10.1007/s11104-009-0135-6](https://doi.org/10.1007%2Fs11104-009-0135-6).
- C. L. Gross. "The reproductive ecology of Canavalia rosea(Fabaceae) on Anak Krakatau, Indonesia".   _Australian Journalof Botany_ 41 (1993), p. 591.  doi: [10.1071/bt9930591](https://doi.org/10.1071%2Fbt9930591).
- P. J. Grubb and D. J. Metcalfe. "Adaptation and inertia in theAustralian tropical lowland rain-forest flora: Contradictorytrends in intergeneric and intrageneric comparisons of seed sizein relation to light demand".   _Functional Ecology_ 10 (1996),p. 512.  doi:  [10.2307/2389944](https://doi.org/10.2307%2F2389944).
- P. J. Grubb, R. V. Jackson, I. M. Barberis, J. N. Bee, D. A.Coomes, N. J. Dominy, M. A. S. D. L. Fuente, P. W. Lucas, D. J.Metcalfe, J. Svenning, I. M. Turner, and O. Vargas. "Monocotleaves are eaten less than dicot leaves in tropical lowland rainforests: Correlations with toughness and leaf presentation".  _Annals of Botany_ 101 (2008), pp. 1379-1389.  doi: [10.1093/aob/mcn047](https://doi.org/10.1093%2Faob%2Fmcn047).
- M. T. Harrison, E. J. Edwards, G. D. Farquhar, A. B. Nicotra,and J. R. Evans. "Nitrogen in cell walls of sclerophyllous leavesaccounts for little of the variation in photosyntheticnitrogen-use efficiency".   _Plant, Cell and Environment_ 32(2009), pp. 259-270.  doi: [10.1111/j.1365-3040.2008.01918.x](https://doi.org/10.1111%2Fj.1365-3040.2008.01918.x).
- F. Hassiotou, J. R. Evans, M. Ludwig, and E. J. Veneklaas."Stomatal crypts may facilitate diffusion of CO2 to adaxialmesophyll cells in thick sclerophylls".   _Plant, Cell andEnvironment_ 32 (2009), pp. 1596-1611.  doi: [10.1111/j.1365-3040.2009.02024.x](https://doi.org/10.1111%2Fj.1365-3040.2009.02024.x).
- M. L. Henery and M. Westoby. "Seed mass and seed nutrientcontent as predictors of seed output variation between species".  _Oikos_ 92 (2001), pp. 479-490.  doi: [10.1034/j.1600-0706.2001.920309.x](https://doi.org/10.1034%2Fj.1600-0706.2001.920309.x).
- L. Hong, R. Lemmens, S. Prawirohatmodjo, I. Soerianegara, M.Sosef, and W. W. (Editors). _Plant resources of south east Asia:Timber trees_. Springer-Verlag Berlin and Heidelberg GmbH & Co.KG, 1999, p. World biodiversity Database CD rom series. ISBN:9783540147732.
- L. Hughes and B. Rice. "Unpublished data: Traits data for 643species". 1992.
- K. Hughes. "Unpublished data: Wood and stem density data forAustralian plant species". 2005.
- B. P. M. Hyland, T. Whiffin, D. Christophel, B. Gray, and R. W.Elick. _Australian tropical rain forest plants trees, shrubs andvines_. CSIRO Publishing, 2003. ISBN: 0643068724.
- World Agroforestry Centre (ICRAF). _The wood density database_.2009.  url: [http://www.worldagroforestry.org/output/wood-density-database](http://www.worldagroforestry.org/output/wood-density-database).
- J. Ilic, D. Boland, M. McDonald, D. G, and P. Blakemore. _Woodydensity phase 1 - State of Knowledge. National Carbon AccountingSystem. Technical Report 18_. Tech. rep. Australian GreenhouseOffice, Canberra, Australia., 2000.
- M. Islam, D. W. Turner, and M. A. Adams. "Phosphorusavailability and the growth, mineral composition and nutritivevalue of ephemeral forbs and associated perennials from thePilbara, Western Australia".   _Australian Journal ofExperimental Agriculture_ 39 (1999), pp. 149-159.  doi: [10.1071/EA98133](https://doi.org/10.1071%2FEA98133).
- M. Islam and M. Adams. "Mineral content and nutritive value ofnative grasses and the response to added phosphorus in a Pilbararangeland".   _Tropical Grasslands_ 33 (1999), pp. 193-200.
- G. Jordan. "Unpublished data: Leaf traits for Tasmanianspecies". 2007.
- E. Jurado. "Diaspore weight, dispersal, growth form andperenniality of central Australian plants".   _Journal ofEcology_ 79 (1993), pp. 811-828.  url: [https://www.jstor.org/stable/2260669](https://www.jstor.org/stable/2260669).
- G. Keighery. "Taxonomy of the Calytrix ecalycata complex(Myrtaceae)".   _Nuytsia_ 15 (2004), pp. 261-268.  url: [https://florabase.dpaw.wa.gov.au/nuytsia/article/409](https://florabase.dpaw.wa.gov.au/nuytsia/article/409).
- Royal Botanic Gardens Kew. _Seed Information Database (SID)_.2010.  url:  [http://data.kew.org/sid/](http://data.kew.org/sid/).
- Royal Botanic Gardens Kew. _Seed Information Database (SID)_.2012.  url:  [http://data.kew.org/sid/](http://data.kew.org/sid/).
- K. J. E. Knox and P. J. Clarke. "Fire severity and nutrientavailability do not constrain resprouting in forest shrubs".  _Plant Ecol_ 212 (2011), pp. 1967-1978.  doi: [10.1007/s11258-011-9956-5](https://doi.org/10.1007%2Fs11258-011-9956-5).
- R. Kooyman, M. Rossetto, W. Cornwell, and M. Westoby."Phylogenetic tests of community assembly across regional tocontinental scales in tropical and subtropical rain forests".  _Global Ecology and Biogeography_ 20 (2011), pp. 707-716.  doi: [10.1111/j.1466-8238.2010.00641.x](https://doi.org/10.1111%2Fj.1466-8238.2010.00641.x).
- E. Laliberté, B. L. Turner, T. Costes, S. J. Pearse, K.Wyrwoll, G. Zemunik, and H. Lambers. "Experimental assessment ofnutrient limitation along a 2-million-year dune chronosequence inthe south-western Australia biodiversity hotspot".   _Journal ofEcology_ 100 (2012), pp. 631-642.  doi: [10.1111/j.1365-2745.2012.01962.x](https://doi.org/10.1111%2Fj.1365-2745.2012.01962.x).
- B. B. Lamont, P. K. Groom, and R. M. Cowling. "High leaf massper area of related species assemblages may reflect low rainfalland carbon isotope discrimination rather than low phosphorus andnitrogen concentrations".   _Functional Ecology_ 16 (2002), pp.403-412.  doi: [10.1046/j.1365-2435.2002.00631.x](https://doi.org/10.1046%2Fj.1365-2435.2002.00631.x).
- B. B. Lamont, P. K. Groom, M. Williams, and T. He. "LMA,density and thickness: recognizing different leaf shapes andcorrecting for their nonlaminarity".   _New Phytologist_ 207.4(2015), pp. 942-947.  doi: [10.1111/nph.13465](https://doi.org/10.1111%2Fnph.13465).
- M. J. Lawes, J. J. Midgley, and P. J. Clarke. "Costs andbenefits of relative bark thickness in relation to fire damage: Asavanna/forest contrast".   _Journal of Ecology_ 101 (2012), pp.517-524.  doi: [10.1111/1365-2745.12035](https://doi.org/10.1111%2F1365-2745.12035).
- M. J. Lawes, H. Adie, J. Russell-Smith, B. Murphy, and J. J.Midgley. "How do small savanna trees avoid stem mortality by fire?The roles of stem diameter, height and bark thickness".  _Ecosphere_ 2.4 (2011), p. art42.  doi: [10.1890/es10-00204.1](https://doi.org/10.1890%2Fes10-00204.1).
- J. R. Lawson, K. A. Fryirs, and M. R. Leishman. _Data from:Hydrological conditions explain wood density in riparian plants ofsouth-eastern Australia_. 2015.  doi: [10.5061/dryad.72h45](https://doi.org/10.5061%2Fdryad.72h45).
- E. Laxton. "Relationship between leaf traits, insectcommunities and resource availability". PhD thesis. MacquarieUniversity, 2005.  url: [http://hdl.handle.net/1959.14/483](http://hdl.handle.net/1959.14/483).
- M. R. Leishman and M. Westoby. "Classifying plants into groupson the basis of associations of individual traits-Evidence fromAustralian semi-arid woodlands".   _Journal of Ecology_ 80(1992), p. 417.  doi: [10.2307/2260687](https://doi.org/10.2307%2F2260687).
- Westoby, M., Rice, B., Howell, and J. "Seed Size and PlantGrowth Form as Factors in Dispersal Spectra".   _Ecology_ 71(1990), pp. 1307-1315.  doi: [10.2307/1938268](https://doi.org/10.2307%2F1938268).
- M. R. Leishman, M. Westoby, and E. Jurado. "Correlates of seedsize variation: A comparison among five temperate floras".  _Journal of Ecology_ 83 (1995), pp. 517-529.  doi: [10.2307/2261604](https://doi.org/10.2307%2F2261604).
- I. Davidson. "Unpublished data: SLA data for 76 acacia speciesfrom glasshouse seedlings". 2011.
- R. V. Gallagher, M. R. Leishman, J. T. Miller, C. Hui, D. M.Richardson, J. Suda, and P. Trávníček. "Invasiveness in introducedAustralian acacias: the role of species traits and genome size".  _Diversity and Distributions_ 17 (2011), pp. 884-897.  doi: [10.1111/j.1472-4642.2011.00805.x](https://doi.org/10.1111%2Fj.1472-4642.2011.00805.x).
- R. Lemmens and I. Soerjanegara. _PROSEA, Volume 5/1: TimberTrees - Major Commercial Timbers_. Pudoc/Prosea, 1993. ISBN:9789022010334.
- F. K. Lim, L. J. Pollock, and P. A. Vesk. "The role of plantfunctional traits in shrub distribution around alpine frosthollows".   _Journal of Vegetation Science_ 28.3 (2017). Ed. byF. de Bello, pp. 585-594.  doi: [10.1111/jvs.12517](https://doi.org/10.1111%2Fjvs.12517).
- J. Lord, J. Egan, T. Clifford, E. Jurado, M. Leishman, D.Williams, and M. Westoby. "Larger seeds in tropical floras:Consistent patterns independent of growth form and dispersalmode".   _Journal of Biogeography_ 24 (1997), pp. 205-211.  doi: [10.1046/j.1365-2699.1997.00126.x](https://doi.org/10.1046%2Fj.1365-2699.1997.00126.x).
- I. Lunt and J. Morgan. "Unpublished data: Trait data fromvarious Morgan and Lunt projects 1990-2012". 2012.
- C. H. Lusk, Y. Onoda, R. Kooyman, and A.Guti<U+FFFD>rrez-Gir<U+FFFD>n. "Reconciling species-level vsplastic responses of evergreen leaf structure to light gradients:shade leaves punch above their weight".   _New Phytologist_186.2 (2010), pp. 429-438.  doi: [10.1111/j.1469-8137.2010.03202.x](https://doi.org/10.1111%2Fj.1469-8137.2010.03202.x).
- C. H. Lusk, J. W. G. Kelly, and S. M. Gleason. "Lightrequirements of Australian tropical vs. cool-temperate rainforesttree species show different relationships with seedling growth andfunctional traits".   _Annals of Botany_ 111 (2012), pp.479-488.  doi: [10.1093/aob/mcs289](https://doi.org/10.1093%2Faob%2Fmcs289).
- C. H. Lusk, K. M. Sendall, and P. J. Clarke. "Seedling growthrates and light requirements of subtropical rainforest treesassociated with basaltic and rhyolitic soils".   _AustralianJournal of Botany_ 62.1 (2014), pp. 48-55.  doi: [10.1071/bt13262](https://doi.org/10.1071%2Fbt13262).
- A. Manea. "Unpublished data: Dispersal appendage data foracacia via worldwide wattle". 2011.
- A. Martawijaya, I. Kartasujana, K. Kadir, and S. A. Prawira._Indonesian wood atlas vol. I. and II_. AFPRDC AFRD Department ofForestry,, 1992.
- B. Maslin. _WATTLE Acacias of Australia_. 2012.
- J. McCarthy. "Predicting the diversity and functionalcomposition of woody plant communities under climate change". PhDthesis. University of Queensland, 2017.
- E. Meier. _The wood database_. 2007.  url: [http://www.wood-database.com/](http://www.wood-database.com/).
- J. Morgan. "Unpublished data: Grassy woodland traits". 2011.
- J. Morgan. "Unpublished data: Trait database". 2011.
- J. Morgan. "Unpublished data: Trait data from Australian plantspecies". 2004.
- H. Morgan. "Root system architecture, water use and rainfallrepsonses of perennial species". PhD thesis. Macquarie University,2005.
- National Herbarium of NSW. _Trait measurements for NSWrainforest species from PlantNET_. 2016.  url: [http://plantnet.rbgsyd.nsw.gov.au/](http://plantnet.rbgsyd.nsw.gov.au/).
- Ü. Niinemets, I. J. Wright, and J. R. Evans. "Leaf mesophylldiffusion conductance in 35 Australian sclerophylls covering abroad range of foliage structural and physiological variation".  _Journal of Experimental Botany_ 60 (2009), pp. 2433-2449. doi:  [10.1093/jxb/erp045](https://doi.org/10.1093%2Fjxb%2Ferp045).
- Northern Territory Herbarium. _Flora of the Darwin RegionOnline_. 2014.  url: [http://www.lrm.nt.gov.au/plants-and-animals/herbarium/darwin_flora_online](http://www.lrm.nt.gov.au/plants-and-animals/herbarium/darwin_flora_online).
- A. O'Reilly-Nugent, E. Wandrag, C. Catford, B. Gruber, D.Driscoll, and R. Duncan. _Measuring impact: Joint-speciesmodelling of invaded plant communities_. 2018.
- C. Osborne, A. Salomaa, T. Kluyver, V. Visser, E. Kellogg, O.Morrone, M. Vorontsova, W. Clayton, and D. Simpson. "A globaldatabase of C4 photosynthesis in grasses".   _New Phytologist_114 (2014), pp. 441-446.  doi: [10.1111/nph.12942](https://doi.org/10.1111%2Fnph.12942).
- P. J. Peeters. "Correlations between leaf structural traits andthe densities of herbivorous insect guilds".   _BiologicalJournal of the Linnean Society_ 77 (2002), pp. 43-65.  doi: [10.1046/j.1095-8312.2002.00091.x](https://doi.org/10.1046%2Fj.1095-8312.2002.00091.x).
- B. K. Pekin, R. S. Wittkuhn, M. M. Boer, C. Macfarlane, and P.F. Grierson. "Plant functional traits along environmentalgradients in seasonally dry and fire-prone ecosystem".  _Journal of Vegetation Science_ 22 (2011), pp. 1009-1020.  doi: [10.1111/j.1654-1103.2011.01323.x](https://doi.org/10.1111%2Fj.1654-1103.2011.01323.x).
- L. D. Prior, D. Eamus, and D. M. J. S. Bowman. "Leaf attributesin the seasonally dry tropics: A comparison of four habitats innorthern Australia".   _Functional Ecology_ 17 (2003), pp.504-515.  doi: [10.1046/j.1365-2435.2003.00761.x](https://doi.org/10.1046%2Fj.1365-2435.2003.00761.x).
- L. D. PRIOR, D. M. J. S. BOWMAN, and D. EAMUS. "Seasonaldifferences in leaf attributes in Australian tropical treespecies: family and habitat comparisons".   _Functional Ecology_18.5 (2004), pp. 707-718.  doi: [10.1111/j.0269-8463.2004.00885.x](https://doi.org/10.1111%2Fj.0269-8463.2004.00885.x).
- Oxford Forestry Institute. _Prospect: The wood database_. 2009. url: [http://dps.plants.ox.ac.uk/ofi/prospect/index.htm](http://dps.plants.ox.ac.uk/ofi/prospect/index.htm).
- Royal Botanic Gardens Kew. _Seed Information Database (SID)_.2014.  url:  [http://data.kew.org/sid/](http://data.kew.org/sid/).
- Royal Botanic Gardens Sydney. _PlantNet: NSW flora online_.2014.  url: [http://plantnet.rbgsyd.nsw.gov.au/](http://plantnet.rbgsyd.nsw.gov.au/).
- Royal Botanic Gardens Sydney. _Plantnet_. 2014.  url: [http://plantnet.rbgsyd.nsw.gov.au/search/simple.htm](http://plantnet.rbgsyd.nsw.gov.au/search/simple.htm).
- B. Rice. "Unpublished data: Growth form of Kakadu plantspecies". 1991.
- A. Richards, A. Shapcott, J. Playford, B. Morrison, C.Critchley, and S. Schmidt. "Physiological profiles of restrictedendemic plants and their widespread congenors in the NorthQueensland wet tropics, Australia".   _Biological Conservation_111 (2003), pp. 41-52.  doi: [10.1016/s0006-3207(02)00245-8](https://doi.org/10.1016%2Fs0006-3207%2802%2900245-8).
- I. Wright. "Unpublished data: OzGLOP compilation". 2008.
- B. Roberts. "Unpublished data". 2006.
- B. Rye. "A revision of south-western Australian species ofMicromyrtus (Myrtaceae) with five antisepalous ribs on thehypanthium.".   _Nuytsia_ 15 (2002), pp. 101-122.
- B. Rye. "A partial revision of the south-western Australianspecies of Micromyrtus (Myrtaceae: Chamelaucieae)".   _Nuytsia_16 (2006), pp. 117-147.
- B. Rye. "Reinstatement of the Western Australian genusOxymyrrhine (Myrtaceae: Chamelaucieae) with three new species".  _Nuytsia_ 19 (2009), pp. 149-165.  url: [https://florabase.dpaw.wa.gov.au/science/nuytsia/468.pdf](https://florabase.dpaw.wa.gov.au/science/nuytsia/468.pdf).
- B. L. Rye. "A revision of the Micromyrtus racemosa complex(Myrtaceae: Chamelaucieae) of south-western Australia".  _Nuytsia_ 20 (2010), pp. 37-56.
- B. Rye, P. Wilson, and G. Keighery. "A revision of the speciesof Hypocalymma (Myrtaceae: Chamelaucieae) with smooth orcolliculate seeds".   _Nuytsia_ 23 (2013), pp. 283-312.  url: [https://florabase.dpaw.wa.gov.au/science/nuytsia/672.pdf](https://florabase.dpaw.wa.gov.au/science/nuytsia/672.pdf).
- B. L. Rye. "An update to the taxonomy of some westernAustralian genera of Myrtaceae tribe Chamelaucieae. 1. Calytrix".  _Nuytsia_ 23 (2013), pp. 483-501.  url: [https://florabase.dpaw.wa.gov.au/science/nuytsia/687.pdf](https://florabase.dpaw.wa.gov.au/science/nuytsia/687.pdf).
- B. L. Rye. "A revision of the south-western Australian genusBabingtonia (Myrtaceae: Chamelaucieae).".   _Nuytsia_ 25 (2015),pp. 219-250.  url: [https://florabase.dpaw.wa.gov.au/science/nuytsia/758.pdf](https://florabase.dpaw.wa.gov.au/science/nuytsia/758.pdf).
- H. Jessop J.P. & Toelken, ed. _Flora of South Australia, 4thedition, 4 vols_. cite as: Jessop, J.P. & Toelken, H.R. (eds)(1986). Flora of South Australia, 4th edition, 4 vols. (GovernmentPrinter: Adelaide). - Electronic version curated by J. Kellermann,State Herbarium of South Australia.. 1986.
- S. Schmidt and G. R. Stewart. "d15N values of tropical savannaand monsoon forest species reflect root specialisations and soilnitrogen status".   _Oecologia_ 134 (2003), pp. 569-577.  doi: [10.1007/s00442-002-1150-y](https://doi.org/10.1007%2Fs00442-002-1150-y).
- E. -. Schulze, R. J. Williams, G. D. Farquhar, W. Schulze, J.Langridge, J. M. Miller, and B. H. Walker. "Carbon and nitrogenisotope discrimination and nitrogen nutrition of trees along arainfall gradient in northern Australia".   _Australian Journalof Plant Physiology_ 25.4 (1998), pp. 413-425.  doi: [10.1071/PP97113](https://doi.org/10.1071%2FPP97113).
- A. J. Scott. "Unpublished data: Honours Project at La TrobeUniversity". 2010.
- K. M. Sendall, C. H. Lusk, and P. B. Reich. "Trade-offs injuvenile growth potential vs. shade tolerance among subtropicalrain forest trees on soils of contrasting fertility".  _Functional Ecology_ 30.6 (2015). Ed. by N. Norden, pp. 845-855. doi: [10.1111/1365-2435.12573](https://doi.org/10.1111%2F1365-2435.12573).
- O. D. Seng. _Specific gravity of Indonesian Woods and itssignificance for practical use_. Tech. rep. FPRDC ForestryDepartment, Bogor, Indonesia, 1951.
- S. Soliveres, D. J. Eldridge, F. Hemmings, and F. T. Maestre."Nurse plant effects on plant species richness in drylands: Therole of grazing, rainfall and species specificity".  _Perspectives in Plant Ecology, Evolution and Systematics_ 14(2012), pp. 402-410.  doi: [10.1016/j.ppees.2012.09.003](https://doi.org/10.1016%2Fj.ppees.2012.09.003).
- R. Specht, P. Rayson, and M. Jackman. "Dark Island heath(Ninety-mile plain, South Australia). VI. Pyric succession:Changes in composition, coverage, dry weight, and mineral nutrientstatus".   _Australian Journal of Botany_ 6 (1958), p. 59.  doi: [10.1071/bt9580059](https://doi.org/10.1071%2Fbt9580059).
- I. R. Thompson. "Morphometric analysis and revision of easternAustralian Hovea (Brongniartieae-Fabaceae)".   _AustralianSystematic Botany_ 14 (2001), p. 1.  doi: [10.1071/sb00008](https://doi.org/10.1071%2Fsb00008).
- Tasmanian Herbarium. _Flora of Tasmania Online_. 2009.  url: [www.tmag.tas.gov.au/floratasmania](www.tmag.tas.gov.au/floratasmania).
- D. Y. P. Tng, G. J. Jordan, and D. M. J. S. Bowman. "Planttraits demonstrate that temperate and tropical giant Eucalyptforests are ecologically convergent with rainforest not savanna".  _PLoS ONE_ 8 (2013), p. e84378.  doi: [10.1371/journal.pone.0084378](https://doi.org/10.1371%2Fjournal.pone.0084378).
- H. Toelken. "A revision of the genus Kunzea (Myrtaceae) I. Thewestern Australian section Zeanuk".   _Journal of the AdelaideBotanic Garden_ 17 (1996), pp. 29-106.
- K. W. Tomlinson, L. Poorter, F. J. Sterck, F. Borghetti, D.Ward, S. de Bie, and F. van Langevelde. "Leaf adaptations ofevergreen and deciduous trees of semi-arid and humid savannas onthree continents".   _Journal of Ecology_ 101 (2013), pp.430-440.  doi: [10.1111/1365-2745.12056](https://doi.org/10.1111%2F1365-2745.12056).
- M. Trudgeon and B. Rye. "Astus, a new western Australian genusof Myrtaceae with heterocarpidic fruits".   _Nuytsia_ 14 (2005),pp. 495-512.
- M. Trudgen and B. Rye. "An update to the taxonomy of somewestern Australian genera of Myrtaceae tribe Chamelaucieae. 2.Cyathostemon".   _Nuytsia_ 24 (2014), pp. 7-16.  url: [https://florabase.dpaw.wa.gov.au/science/nuytsia/692.pdf](https://florabase.dpaw.wa.gov.au/science/nuytsia/692.pdf).
- S. E. Venn, K. Green, C. M. Pickering, and J. W. Morgan. "Usingplant functional traits to explain community composition across astrong environmental filter in Australian alpine snowpatches".  _Plant Ecology_ 212 (2011), pp. 1491-1499.  doi: [10.1007/s11258-011-9923-1](https://doi.org/10.1007%2Fs11258-011-9923-1).
- P. A. Vesk, D. I. Warton, and M. Westoby. "Sprouting bysemi-arid plants: Testing a dichotomy and predictive traits".  _Oikos_ 107 (2004), pp. 72-89.  doi: [10.1111/j.0030-1299.2004.13122.x](https://doi.org/10.1111%2Fj.0030-1299.2004.13122.x).
- P. A. Vesk, M. R. Leishman, and M. Westoby. "Simple traits donot predict grazing response in Australian dry shrublands andwoodlands".   _Journal of Applied Ecology_ 41.1 (2004), pp.22-31.  doi: [10.1111/j.1365-2664.2004.00857.x](https://doi.org/10.1111%2Fj.1365-2664.2004.00857.x).
- P. Vesk. "Unpublished data: Leaf traits for flora of FallsCreek Victoria". 2007.
- Western Australian Herbarium. _FloraBase: the westernAustralian flora_. 1998.  url: [https://florabase.dpaw.wa.gov.au/](https://florabase.dpaw.wa.gov.au/).
- Western Australian Herbarium. _FloraBase: the westernAustralian flora_. 2016.  url: [https://florabase.dpaw.wa.gov.au/](https://florabase.dpaw.wa.gov.au/).
- J. Wells. "Unpublished data: Wood density data for global wooddensity database". 2009.
- M. Westoby and I. J. Wright. "The leaf size - twig sizespectrum and its relationship to other important spectra ofvariation among species".   _Oecologia_ 135 (2003), pp. 621-628. doi: [10.1007/s00442-003-1231-6](https://doi.org/10.1007%2Fs00442-003-1231-6).
- Westoby, M., Rice, B., Howell, and J. "Seed Size and PlantGrowth Form as Factors in Dispersal Spectra".   _Ecology_ 71(1990), pp. 1307-1315.  doi: [10.2307/1938268](https://doi.org/10.2307%2F1938268).
- M. Westoby. "Unpublished data: Trait data for plant species atMt Wellington and Sea Acres". Collected while at MacquarieUniversity. 2004.
- S. M. Gleason, D. W. Butler, K. Zieminska, P. Waryszak, and M.Westoby. "Stem xylem conductivity is key to plant water balanceacross Australian angiosperm species".   _Functional Ecology_ 26(2012), pp. 343-352.  doi: [10.1111/j.1365-2435.2012.01962.x](https://doi.org/10.1111%2Fj.1365-2435.2012.01962.x).
- K. Zieminska, D. W. Butler, S. M. Gleason, I. J. Wright, and M.Westoby. "Fibre wall and lumen fractions drive wood densityvariation across 24 Australian angiosperms".   _AoB PLANTS_ 5(2013).  doi: [10.1093/aobpla/plt046](https://doi.org/10.1093%2Faobpla%2Fplt046).
- M. Westoby. "Unpublished data: SLA data for various plantspecies". 1996.
- J. R. Wheeler, N. G. Marchant, and M. Lewington. _Flora of thesouth west: Bunbury, Augusta, Denmark_. Canberra, A.C.T. :Crawley, W.A: Australian Biological Resources Study ; Universityof Western Australia Press, 2002. ISBN: 9780642568168.
- P. G. Wilson and R. Rowe. "A revision of the Indigofereae(Fabaceae) in Australia. 2. Indigofera species with trifoliolateand alternately pinnate leaves".   _Telopea_ 12 (2008), pp.293-307.
- I. J. Wright, H. T. Clifford, R. Kidson, M. L. Reed, B. L.Rice, and M. Westoby. "A survey of seed and seedling characters in1744 Australian dicotyledon species: Cross-species traitcorrelations and correlated trait-shifts within evolutionarylineages".   _Biological Journal of the Linnean Society_ 69(2000), pp. 521-547.  doi: [10.1111/j.1095-8312.2000.tb01222.x](https://doi.org/10.1111%2Fj.1095-8312.2000.tb01222.x).
- I. J. Wright. "Unpublished data: Seed mass reserve data forvarious species in NSW". 2001.
- I. J. Wright and M. Westoby. "Leaves at low versus highrainfall: Coordination of structure, lifespan and physiology".  _New Phytologist_ 155 (2002), pp. 403-416.  doi: [10.1046/j.1469-8137.2002.00479.x](https://doi.org/10.1046%2Fj.1469-8137.2002.00479.x).
- I. J. Wright, P. B. Reich, M. Westoby, D. D. Ackerly, Z.Baruch, F. Bongers, J. Cavender-Bares, T. Chapin, J. H. C.Cornelissen, M. Diemer, J. Flexas, E. Garnier, P. K. Groom, J.Gulias, K. Hikosaka, B. B. Lamont, T. Lee, W. Lee, C. Lusk, J. J.Midgley, M. Navas, Ü. Niinemets, J. Oleksyn, N. Osada, H. Poorter,P. Poot, L. Prior, V. I. Pyankov, C. Roumet, S. C. Thomas, M. G.Tjoelker, E. J. Veneklaas, and R. Villar. "The worldwide leafeconomics spectrum".   _Nature_ 428 (2004), pp. 821-827.  doi: [10.1038/nature02403](https://doi.org/10.1038%2Fnature02403).
- I. J. Wright, D. S. Falster, M. Pickup, and M. Westoby."Cross-species patterns in the coordination between leaf and stemtraits, and their implications for plant hydraulics".  _Physiologia Plantarum_ 127 (2006), pp. 445-456.  doi: [10.1111/j.1399-3054.2006.00699.x](https://doi.org/10.1111%2Fj.1399-3054.2006.00699.x).
- I. Wright. "Unpublished data: Trait data for Northern Territorysavanna species". 2008.
- A. Zanne. "Unpublished data: Hydraulic traits for NSW plantspecies from four field sites". 2007.
- A. E. Zanne, G. Lopez-Gonzalez, D. A. Coomes, J. Ilic, S.Jansen, S. L. Lewis, R. B. Miller, N. G. Swenson, M. C. Wiemann,and J. Chave. _Data from: Towards a worldwide wood economicsspectrum_. 2009.  doi: [10.5061/dryad.234](https://doi.org/10.5061%2Fdryad.234).

## File types

### CSV 

### YAML files {#yaml}


yml: The `yml` file extension (pronounced "YAML") [is a type structured data file](https://en.wikipedia.org/wiki/YAML), that is both human and machine readable. The information in it appears under various labels, which are imported into AusTraits. You can edit it any text editor, or also in Rstudio. Generally, yml is used in situations where a table does not suit because of variable lengths and or nested structures.
This document outlines how to describe metadata for any given study

The metadata is compiled in a `.yml` file, a structured data file where information is presented in a hierarchical format. It has the advantage over a spreadsheet in that the nested “headers” can have variable numbers of categories. The data under each of the hierarchical headings are easily extracted by R.

Assumed character unless specified otherwise

possible types:

- `character`: 
- `numeric`: must have fields `description`, `type`, `units`, `values` with filed `minimum` and `maximum`.
- `categorical`: Unlike character, only specific values are allowed. must have fields `description`, `type`,and `values`, with the latter including a list of possible values.
- `table`:
- `list`:
- `array`:

## Adding custom R code into metadata.yml {#custom_R_code}

Occasionally all the changes we want to make to dataset may not fit into the prescribed workflow used in AusTraits. For example, we assume each trait has a single unit. But there are a few datasets where data on different rows have different units. So we want to make to make some custom modifications to this particular dataset before the common pipeline of operations gets applied. To make this possible, the workflow allows for some custom R code to be run as a first step in the processing pipeline. That pipeline (in the function [`read_data_study`](https://github.com/traitecoevo/austraits/blob/master/R/steps.R#L59)) looks like:


```r
  data <- 
    read_csv(filename_data_raw, col_types = cols()) %>%
    custom_manipulation(metadata[["config"]][["custom_R_code"]])() %>%
    parse_data(dataset_id, metadata) %>%
    ...
```

Note the second line. 

### Example problem 

As an example, `Barlow_1981` has multiple units per traits. Check it out:


```r
library(readr)
library(yaml)
```

Load the data

```r
key <- "Barlow_1981"
data <- read_csv(file.path("data", key, "data.csv"), col_types = cols(.default = "c"))
metadata <- read_yaml(file.path("data", key, "metadata.yml"))
```

Here's the problem - note tat several traits have multiple units used:


```r
table(data$trait, data$units)
```

```
##                       
##                          cm    m   mm
##   dispersal               0    0    0
##   flowering time end      0    0    0
##   flowering time start    0    0    0
##   leaf length maximum  2570    7  836
##   leaf length minimum  2489    7  815
##   leaf type               0    0    0
##   leaf width maximum   1050    0 1986
##   leaf width minimum   1005    0 1974
##   lifeform                0    0    0
##   longevity               0    0    0
##   plant height maximum  831 2371    6
##   plant height minimum  200  862    0
##   seed breadth maximum    5    0   72
##   seed breadth minimum    5    0   72
##   seed length maximum    33    0  718
##   seed length minimum    34    0  733
##   seed shape              0    0    0
##   seed width maximum     22    0  404
##   seed width minimum     22    0  407
```

So you want to write some R code that fixes this and gets the dataset into the processing pipeline, satisfying all the assumptions.

### Developing solutions

We want to write some custom R code that will appear in the `metadata.yml` file for that study, under a title `config` -> `custom_R_code`. E.g. see this example for [data/Barlow_1981/metadata.yml](https://github.com/traitecoevo/austraits/blob/master/data/Barlow_1981/metadata.yml).

Your code should assume a single object called `data`. And apply whatever fixes are needed. Also it should be

- fully self contained (we're not going to use any of the other remake machinery here)
- have semi colons `;` at the end of each line. This will be needed because we're adding the code to the `metadata.yml` file and newlines get lost when reading in the file.

The workflow is to first develop some code that applies a suitable fix. E.g. for Barlow_1981 here is the code we eventually applied to the object `data` loaded above:


```r
metadata[["config"]][["custom_R_code"]] %>% gsub(";", ";\n", .) %>% writeLines()
```

Running this removes the problem with multiple units:

```r
table(data$trait, data$units)
```

```
##                       
##                          cm    m   mm
##   dispersal               0    0    0
##   flowering time end      0    0    0
##   flowering time start    0    0    0
##   leaf length maximum  2570    7  836
##   leaf length minimum  2489    7  815
##   leaf type               0    0    0
##   leaf width maximum   1050    0 1986
##   leaf width minimum   1005    0 1974
##   lifeform                0    0    0
##   longevity               0    0    0
##   plant height maximum  831 2371    6
##   plant height minimum  200  862    0
##   seed breadth maximum    5    0   72
##   seed breadth minimum    5    0   72
##   seed length maximum    33    0  718
##   seed length minimum    34    0  733
##   seed shape              0    0    0
##   seed width maximum     22    0  404
##   seed width minimum     22    0  407
```
(we can ignore the NULLs here, these are when data and units are both NA. Those get pruged furtehr down the pipeline).

Once you have some working code, you then want to add it into your yml file under a group `config` -> `custom_R_code`. 

And then check it works.

Let's assume you added it in, so we'll load the metadata (and also reload the data)

In the build process we use the function `custom_manipulation` to create a function that accepts a data frame and modifies it according to the code in `txt`


```r
custom_manipulation
```

```
function (txt) 
{
    if (!is.null(txt) && !is.na(txt) && nchar(txt) > 0) {
        function(data) {
            eval(parse(text = txt), env = new.env())
        }
    }
    else {
        identity
    }
}
```

So now lets use it to create a function

```r
f <- custom_manipulation(metadata[["config"]][["custom_R_code"]])
f
```

```
## function (data) 
## {
##     eval(parse(text = txt), env = new.env())
## }
## <environment: 0x7f96dae95fd0>
```

And finally we can apply the function to our data:

```r
data2 <- f(data)
```
(If it fails at this point it won't work in the build). 

Now let's compare it to our original data (the columns units and values should now differ)


```r
all.equal(data, data2)
```

```
## [1] "Different number of rows"
```

And also see the units and traits:


```r
table(data2$trait, data2$units)
```

```
##                       
##                          mm
##   dispersal               0
##   flowering_time          0
##   leaf length maximum  3413
##   leaf length minimum  3311
##   leaf type               0
##   leaf width maximum   3036
##   leaf width minimum   2979
##   leaf_phenology          0
##   leaf_type               0
##   life_history            0
##   lifeform                0
##   plant height maximum 3208
##   plant height minimum 1062
##   seed breadth maximum   77
##   seed breadth minimum   77
##   seed length maximum   751
##   seed length minimum   767
##   seed shape              0
##   seed width maximum    426
##   seed width minimum    429
```

Finally, check it works in the context of loading the metadata:


```r
data2 <- custom_manipulation(metadata[["config"]][["custom_R_code"]])(data)
all.equal(data, data2)
```

```
## [1] "Different number of rows"
```

Now you're ready to go. 

