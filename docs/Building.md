Building of the `AusTraits` data compilation
================
Daniel Falster, Rachael Gallagher, Sam Andrew, Dony Indiarto, James
Lawson, Lizzy Wenk
2020-03-11



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

This document provides information on how AusTraits is assembled from
its component parts. Separate documents provide information on:

  - Overview of Austraits: [Falster et al 2020](XXXX)
  - Accessing and using AusTraits: XXXX
  - Building Austraits: [Building](Building.md)
  - Contributing to AusTraits: [Contributing](Contributing.md)
  - Working with GitHub repository:
    [Working\_with\_github](Working_with_github.md)
  - Tips & Tricks for AusTraits developes: [TipsTricks](TipTricks.md)
  - Full list of trait definitions:
    [Trait\_definitions](Trait_definitions.md)

# Overview & Mission

This document describes the compiling of AusTraits - a database of
traits for the Australian Flora. It was generated using AusTraits
**version 0.9.1.9000**, which harmonises data on 202 traits from 175
different sources, including field campaigns, published literature,
taxonomic monographs, and individual species descriptions. Traits link
to ecological strategy variation and vary in scope from physiological
measures of performance (e.g. photosynthetic gas exchange, water-use
efficiency) to morphological parameters (e.g. leaf size, seed size,
maximum height). AusTraits contains curated and harmonised species- and
genus-level observations coupled to, where available, contextual
information on site properties.

We envision AusTraits as an on-going collaborative community resource
that:

1.  Increases our collective understanding the Australian flora;
2.  Facilitates accumulating and sharing of trait data;
3.  Aspires to fully transparent and reproducible research of highest
    standard, and
4.  Builds a sense of community among contributors and users.

Prior to the development of AusTraits, data on Australian plant traits
existed as a series of largely disconnected datasets collected by
individual laboratories or initiatives. Our goal has been to harmonise
these different sources. Moreoever, we aimed to do this in an
open-source way, so as to accelerate discoveries about Australian
plants.

We therefore set out to build a resource with the following features:

  - The workflow for harmonising different datasets is made fully open
    and reproducible;
  - All data are shared under a standard open license permitting reuse;
  - Successive versions of the data remain available to ensure
    reproducibility of research;
  - Contributions towards AusTraits are recognised via invitations to
    co-author data papers, i.e. releases of the data resource, and
    subsequent citation.
  - Users of AusTraits are encouraged to contribute towards the further
    improvement of the database.

A key goal for us was to make the process for harmonising different
datasets as transparent posbble. Our workflow is therefore
fully-reproducible and open, meanign it exposes the decisions made in
the processing of data into a harmonised and curated dataset (Figure 1);
and can also be reruen by others (see section ON [compiling
AusTraits](#compiling)).

![](Workflow.png)

# Structure of AusTraits

AusTraits is essentailly a series of linked components, which cross link
against each other::

    austraits
    ├── traits
    ├── sites
    ├── methods
    ├── excluded_data
    ├── taxonomy
    ├── definitions
    ├── contributors
    ├── sources
    └── build_info

These include all the data and contextual information submitted with
each contributed dataset. It is essential that users of AusTraits data
are confident the data have the meaning they expect it to and were
collected using methods they trust. As such, each dataset within
Austraits must include descriptions of the study, sites, and methods
used as well as the data itself.

## Components

The core components are defined as follows.

### traits

**Description:** A table containing measurements of plant
traits.

**Content:**

| key             | value                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| :-------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| dataset\_id     | Primary identifier for each study contributed into AusTraits; most often these are scientific papers, books, or online resources. By default should be name of first author and year of publication, e.g. `Falster_2005`.                                                                                                                                                                                                                                                  |
| species\_name   | Name of species after aligning taxonomy with standard sources.                                                                                                                                                                                                                                                                                                                                                                                                             |
| site\_name      | Name of site where species was sampled. Cross-references between similar columns in context and data.                                                                                                                                                                                                                                                                                                                                                                      |
| observation\_id | A unique identifier for the observation, useful for joining traits coming from the same observation\_id. Observation ids are assigned automatically, based on `dataset_id` and row number in the file `data.csv`.                                                                                                                                                                                                                                                          |
| trait\_name     | Name of trait sampled. Allowable values specified in the table `traits`.                                                                                                                                                                                                                                                                                                                                                                                                   |
| value           | Value of trait sampled. In the master dataset these are all stored as characters, then converted to numeric later where appropriate.                                                                                                                                                                                                                                                                                                                                       |
| unit            | Units of the sampled trait value after aligning with AusTraits standards.                                                                                                                                                                                                                                                                                                                                                                                                  |
| value\_type     | A categorical variable describing the type of trait value recorded                                                                                                                                                                                                                                                                                                                                                                                                         |
| replicates      | Number of replicate measurements that comprise the data points for the trait for each measurement. A numeric value (or range) is ideal and appropriate if the value type is a `mean`, `median`, `min` or `max`. For these value types, if replication is unknown the entry should be `unknown`. If the value type is `raw_value` the replicate value should be `1`. If the value type is `expert_mean`, `expert_min`, or `expert_max` the replicate value should be `.na`. |
| original\_name  | Taxonomic name given to species in the original data supplied by the authors                                                                                                                                                                                                                                                                                                                                                                                               |

### sites

**Description:** A table containing observations of site characteristics
associated with information in `data`. Cross referencing between the two
dataframes is possible using combinations of the variables `dataset_id`,
`site_name`.

**Content:**

| key            | value                                                                                                                                                                                                                     |
| :------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| dataset\_id    | Primary identifier for each study contributed into AusTraits; most often these are scientific papers, books, or online resources. By default should be name of first author and year of publication, e.g. `Falster_2005`. |
| site\_name     | Name of site where species was sampled. Cross-references between similar columns in context and data.                                                                                                                     |
| site\_property | The site characteristic being recorded. Name should include units of measurement, e.g. `longitude (deg)`. Ideally we have at least these variables for each site - `longitude (deg)`, `latitude (deg)`, `description`.    |
| value          | Value of trait sampled. In the master dataset these are all stored as characters, then converted to numeric later where appropriate.                                                                                      |

### methods

**Description:** A table containing details on methods with which data
were collected, including time frame and
source.

**Content:**

| key                         | value                                                                                                                                                                                                                                                                                                                                  |
| :-------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| dataset\_id                 | Primary identifier for each study contributed into AusTraits; most often these are scientific papers, books, or online resources. By default should be name of first author and year of publication, e.g. `Falster_2005`.                                                                                                              |
| trait\_name                 | Name of trait sampled. Allowable values specified in the table `traits`.                                                                                                                                                                                                                                                               |
| methods                     | A textual description of the methods used to collect the trait data. Whenever available, methods are taken near-verbatim from referenced souce. Methods can include descriptions such as ‘measured on herbarium specimens’,‘data from the literature’, or a detailed description of the field or lab methods used to collect the data. |
| year\_collected\_start      | The year data collection commenced                                                                                                                                                                                                                                                                                                     |
| year\_collected\_end        | The year data collection was completed                                                                                                                                                                                                                                                                                                 |
| description                 | A 1-2 sentence description of the purpose of the study.                                                                                                                                                                                                                                                                                |
| collection\_type            | A field to indicate where the plants on which traits were measured were collected - in the `field`, `lab`, `glasshouse`, `herbarium specimens`, or `literature`. The latter should only be used when the data were sourced from the literature and the collection type is unknown.                                                     |
| sample\_age\_class          | A field to indicate if the study was completed on `adult` or `juvenile` plants                                                                                                                                                                                                                                                         |
| sampling\_strategy          | A written description of how study sites were selected and how study individuals were selected. When available, this information is lifted verbatim from a published manuscript. For herbarium studies, this field ideally indicates which herbaria were ‘sampled’ to measure a specific trait                                         |
| source\_primary\_citation   | Citation for primary source. This detail is genearted from the primary source in the metadata                                                                                                                                                                                                                                          |
| source\_primary\_key        | Citation key for primary source in `sources`. The key is typically of format Surname\_year                                                                                                                                                                                                                                             |
| source\_secondary\_citation | Citation for secondary source. This detail is genearted from the secondary source in the metadata                                                                                                                                                                                                                                      |
| source\_secondary\_key      | Citation key for secondary source in `sources`. The key is typically of format Surname\_year                                                                                                                                                                                                                                           |

### excluded\_data

**Description:** A table of data that did not pass quality test and so
were excluded from the master dataset.

**Content:** Structure is identical to that presented in the `data`
table, only with an extra column called `error` indicating why the
record was excluded

### taxonomy

**Description:** A table containing details on species taxonomy
associated with information in `data`. Cross referencing between the two
dataframes is possible using the variable `species_name`. We have
attempted to align species names with known taxonomic units, focussing
primarily on the [`The Plant List` (TPL)](http://www.theplantlist.org/)
– a global working list of all known plant species. In addition we
have tried to align these names with the [`Australian Plant Census`
(APC)](https://biodiversity.org.au/nsl/services/apc) and the
[`Australian Plant Names Index`
(APNI)](https://biodiversity.org.au/nsl/services/APNI).

**Content:**

| key           | value                                                          |
| :------------ | :------------------------------------------------------------- |
| species\_name | Name of species after aligning taxonomy with standard sources. |
| family        | Family of aligned species\_name                                |
| authority     | Authority for aligned species\_name                            |
| TPL\_ID       | ID of species in the TPL                                       |
| status        | Status of species in TPL                                       |
| APC\_name     | Name of species in APC                                         |
| APC\_ID       | ID of species in the APC                                       |
| APNI\_ID      | ID of species in the APNI                                      |

### definitions

**Description:** A copy of the definitions for all tables and terms.
Information included here was used to process data and generate any
documentation for the study.

**Content:** A structured yaml file, represented as a list in R. See
file `config/definitions.yaml` for more details.

### contributors

**Description:** A list of people contributing to each
study.

**Content:**

| key         | value                                                                                                                                                                                                                     |
| :---------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| dataset\_id | Primary identifier for each study contributed into AusTraits; most often these are scientific papers, books, or online resources. By default should be name of first author and year of publication, e.g. `Falster_2005`. |
| name        | Name of contributor                                                                                                                                                                                                       |
| institution | Last known institution or affiliation                                                                                                                                                                                     |
| role        | Their role in the study                                                                                                                                                                                                   |

### sources

**Description:** Bibtex entries for all primary and secondary sources in
the compilation.

### build\_info

**Description:** Description of the computing environment used to create
this version of the dataset, including version number, git commit and R
session\_info.

## Dataset ID

The core organising unit behind AusTraits is the `dataset_id`. Records
are organsiingn as coming from a particular study, defined by the
`dataset_id`. Our preferred fromat for `dataset_id` is surname of the
first author of any corrpeosnding publication, followed by the year, as
`surname_year`. E.g. `Falster_2005`. Whereever there are multiple
studies with the same id, we add a suffix `a`, `b` etc.
E.g.`Falster_2005a`, `Falster_2005b`.

## Observation IDs

As well as a `dataset_id`, each trait measurement has an associated
`observation_id`. Observation IDs bind together related measurements
within any dataset, and thereby allow transfromation between long
(e.g. with variables `trait_name` and `value`) and wide (e.g. with
traits as columns) formats.

Generally, `observation_id` has the format `dataset_id_XX` where `XX` is
a unique number within each dataset. For example, if multiple traits
were collected on the same individual, the `observation_id` allows us to
gather these together. For floras, which report a species averages, the
`observation_id` is determined via the species name. Importantly,

For datasets that arrive in wide format we assume each row has a unique
`observation_id`. For datasets that arrive in long format, the
`observation_id` is assigned based on a specified grouping variable.
This variable can be specified in the `metadata.yml` file under the
section `variable_match`. If missing, `observation_id` is assigned based
on `species_name`.

## Site Names

As well as `dataset_id` and `observation_id`, where approrpiate, trait
values are associated with a `site_name`. Unique combinations of
`dataset_id` and `site_name` can be used to retrieve site details,
stored in the table `sites`.

## Values and Value types

Each record in the table of trait data has an associated `value` and
`value_type`.

Traits are either `numeric` or `categorical`. For traits with numerical
values, the recorded value has been converted into standardised units
and we have check that the value can be converted into a number and lies
within the allowable range. For categorical variables, we only include
records that are defined in the definitions (see [trait definitions
below](#traits)). Moreover, we use a format whereby

  - we use `_` for multi-word terms, e.g. `semi_deciduous`
  - use a space for situations where there are two possible values for
    that trait, e.g. `annual biennial` for something which is either
    annual or biennial

Each trait measurement also an associated `value_type`, which gives `A
categorical variable describing the type of trait value recorded`.
Possible values
are:

| key              | value                                                                                                                                                                                                                                                                                                                                                                                               |
| :--------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| raw\_value       | Value is a direct measurement                                                                                                                                                                                                                                                                                                                                                                       |
| site\_min        | Value is the minimum of measurements on multiple individuals of the species at a single site                                                                                                                                                                                                                                                                                                        |
| site\_mean       | Value is the mean or median of measurements on multiple individuals of the species at a single site                                                                                                                                                                                                                                                                                                 |
| site\_max        | Value is the maximum of measurements on multiple individuals of the species at a single site                                                                                                                                                                                                                                                                                                        |
| multisite\_min   | Value is the minimum of measurements on multiple individuals of the species across multiple sites                                                                                                                                                                                                                                                                                                   |
| multisite\_mean  | Value is the mean or median of measurements on multiple individuals of the species across multiple sites                                                                                                                                                                                                                                                                                            |
| multisite\_max   | Value is the maximum of measurements on multiple individuals of the species across multiple sites                                                                                                                                                                                                                                                                                                   |
| expert\_min      | Value is the minimum observed for a species across its entire global range, as estimated by an expert based on their knowledge of the species. The value has not been measured directly. Data from herabarium studies that represent a species’ entire range fit in this category.                                                                                                                  |
| expert\_mean     | Value is the average observed for a species across its entire global range, as estimated by an expert based on their knowledge of the species. The value has not been measured directly. Data from herabarium studies that represent a species’ entire range fit in this category. Categorical variable values obtained from a reference book or identified by an expert also have this value type. |
| expert\_max      | Value is the maximum observed for a species across its entire global range, as estimated by an expert based on their knowledge of the species. The value has not been measured directly. Data from herabarium studies that represent a species’ entire range fit in this category.                                                                                                                  |
| experiment\_min  | Value is the minimum of measurements from an experimental study either in the field or a glasshouse                                                                                                                                                                                                                                                                                                 |
| experiment\_mean | Value is the mean or median of measurements from an experimental study either in the field or a glasshouse                                                                                                                                                                                                                                                                                          |
| experiment\_max  | Value is the maximum of measurements from an experimental study either in the field or a glasshouse                                                                                                                                                                                                                                                                                                 |
| individual\_mean | Value is a mean of replicate measurements on an individual (usually for experimental ecophysiology studies)                                                                                                                                                                                                                                                                                         |
| unknown          | Value type is not currently known                                                                                                                                                                                                                                                                                                                                                                   |

AusTraits does not include intra-individual observations. When multiple
measurements per individual are submitted to AusTraits, we take the mean
of the values and record the value\_type as `individual_mean`.

## Taxonomy

Version 0.9.1.9000 of AusTraits contains records for 22191 different
species. We have attempted to align species names with known taxonomic
units, focussing primarily on the [`The Plant List`
(TPL)](http://www.theplantlist.org/) – a global working list of all
known plant species. In addition we have tried to align these names with
the [`Australian Plant Census`
(APC)](https://biodiversity.org.au/nsl/services/apc) and the
[`Australian Plant Names Index`
(APNI)](https://biodiversity.org.au/nsl/services/APNI). The `APNI_ID`
can also be used to access relevant records for the species in the
[`Atlas of Living Australia`](https://bie.ala.org.au/) (ALA).

Links to species records in these systems can be accessed via the
relevant IDs as in these examples.

  - The Plant List:
    <http://www.theplantlist.org/tpl1.1/record/kew-450649> where
    `kew-450649` is the `TPL_ID`
  - Australian Plant Census:
    <https://biodiversity.org.au/nsl/services/node/apc/2908862> where
    `2908862` is the `APC_ID`
  - Australian Plant Names Index:
    <http://id.biodiversity.org.au/node/apni/2899106> where `2899106` is
    the `APNI_ID`
  - Atlas of Living Australia:
    <https://bie.ala.org.au/species/http://id.biodiversity.org.au/node/apni/2899106>
    where `2899106` is the `APNI_ID`.

## Sources

For each dataset in the compilation there is the option to list primary
and secondary citation.s The primary citation The original study in
which data were collected while the secondary citation is A subsequent
study where data were compiled or re-analysed and then made available.
These references are included in two places:

1.  Within the table [methods](#methods), where we provide a formatted
    version of each.
2.  In the element [sources](#sources), where we provided bibtex
    versions of all sources which can be imported into your reference
    library. The keys for these references are listed within the
    <span id="methods">methods</span>.

## Trait definitions

Allowable traits and values are defined in the definitions file. Each
trait is labelled as either `numeric` or `categorical`. An exa,ple of
each type is as follows. For the full list, see Appendix section [Trait
definition](#trait_defs).

**specific\_leaf\_area**

  - label: Leaf area per unit leaf dry mass (specific leaf area, SLA)
  - description: Leaf area per unit leaf dry mass; SLA
  - number of records: 23664
  - number of studies: 69
  - type: numeric
  - units: mm2/mg
  - allowable range: 0.4 - 500 mm2/mg

**woodiness**

  - label: Woodiness
  - description: A plant’s degree of lignification in stems
  - number of records: 6582
  - number of studies: 6
  - type: categorical
  - allowable values:
      - *herbaceous*: Plant with non-lignified stems
      - *semi\_woody*: Plant with partially lignified stems
      - *woody*: Plant that produces secondary xylem, have lignin

# Compiling AusTraits

In this section we describe how to build the harmonised datatset, before
descrbing the structure of the raw components in more detail. By
“compiling” we mean transofmring data from all the different studies
into a harmonised common format. As described above and depicted in
Figure 1, AusTraits is built so that you can rebuild the database from
its parts at any time. This means that decisions made along the way, in
how data are transfromed of encoded, can be inspected and modified. And
new data is easily incorpoated.

## Gihtub repoisitory

All the raw materials and code needed are stored in our github
repository
[`austriats.build`](https://github.com/traitecoevo/austraits.build/).
Github provides a commercial web platform for sharing, visualizing, and
managing data or code under version control. It includes ability to
browse the “history” of the repository, “issue” tracking, and ability to
host “releases”. It also enables multiple contributors to work
simulaenously on the same project. While traditonally used for storing
code, Github is also being used to store data, especially where there
are multiple contirbuors ([Perkel 2016](http://doi.org/10.1038/538127a),
[Falster et al 2019](http://doi.org/10.1093/gigascience/giz035)).

The first step to compile AsTraits is to download a copy of the
[austriats.build](https://github.com/traitecoevo/austraits.build/)
repository from Github. Then open up the Rstudio project, or open R into
the right directory.

Further details on working with our Github reporsiotry are available in
the document XXXX

## Install dependencies via `devtools`

To check you have the right packages installed, you can use the
[`devtools`](https://devtools.r-lib.org) package to run:

``` r
# install.packages("devtools)  # install devtools if needed
devtools::install_deps()
```

This command checks that the required packages, listed in the file
`DESCRIPTION` are available in your local machine.

## Compile via `remake`

One of the packages that will be installed with the above is
[`remake`](https://github.com/richfitz/remake). This package manages the
compiling, and also helps steramline the amount of recompiling needed
whne new sources are added.

Running the following command will rebuild AusTraits and save the
assmebled dataset into a file `export/data/austraits.rds`.

Remake can also load the compiled dataset directly into R by calling:

# Structure and format of raw data

## File structure

The main directory for the [`austraits.build`
reprository](https://github.com/traitecoevo/austraits.build/) repository
contains the following files and folders, with purpose as indicated.

    austraits
    ├── austraits.build.Rproj  # Rstudio project 
    ├── config                 # configuration and definition files
    ├── data                   # raw data files
    ├── DESCRIPTION            # R package description
    ├── Dockerfile             # Creates container for docker
    ├── docs                   # documentation
    ├── export                 # for output
    ├── R                      # R functions used during build
    ├── README.md              # landing page
    ├── remake.yml             # instructions for build
    ├── scripts                # 
    └── tests                  # defines tests applied to datasets

## Configuration

The folder `config` contains three files which govern the building of
the dataset.

    config
    ├── definitions.yml
    ├── species_list.csv
    └── unit_conversions.csv

The file `definitions.yml` defines the structure of the database and all
terms. The tables and definitions provided in the section above on
[Structure of AusTraits](#austraits_structure%5D) are defined in this
file, as are trait defitnions. A `.yml` file is a structured data file
where information is presented in a hierarchical format (see [appendix
for details](#yaml)). You will need to edit this file if you are
changing or adding trait definitions.

The file `species_list.csv` is our master list of known species. Each
species is listed once, with known IDs (see [section above](#taxonomic)
for
details).

| species\_name             | family      | authority                     | TPL\_ID     | status   | APC\_name             | APC\_ID | APNI\_ID |
| :------------------------ | :---------- | :---------------------------- | :---------- | :------- | :-------------------- | :------ | :------- |
| Abelmoschus               | Malvaceae   | NA                            | NA          | NA       | NA                    | NA      | NA       |
| Abelmoschus ficulneus     | Malvaceae   | (L.) Wight & Arn.             | kew-2609577 | Accepted | Abelmoschus ficulneus | 2897916 | 55929    |
| Abelmoschus manihot       | Malvaceae   | (L.) Medik.                   | kew-2609589 | Accepted | Abelmoschus manihot   | 2901085 | 55937    |
| Abelmoschus moschatus     | Malvaceae   | Medik.                        | kew-2609599 | Accepted | Abelmoschus moschatus | 2900572 | 55953    |
| Abroma augusta            | Malvaceae   | (L.) L.f.                     | kew-2610167 | Accepted | Abroma molle          | 2904866 | 199881   |
| Abrophyllum ornans        | Rousseaceae | (F.Muell.) Hook.f.            | kew-2610292 | Accepted | Abrophyllum ornans    | 2893678 | 56085    |
| Abrotanella forsterioides | Compositae  | (Hook.f.) Benth.              | gcc-99065   | Accepted | unknown               | unknown | unknown  |
| Abrotanella nivigena      | Compositae  | (F.Muell.) F.Muell. ex Benth. | gcc-31570   | Accepted | Abrotanella nivigena  | 2900512 | 56120    |
| Abrotanella scapigera     | Compositae  | NA                            | NA          | NA       | NA                    | NA      | NA       |
| Abrus precatorius         | Leguminosae | L.                            | ild-2477    | Accepted | Abrus precatorius     | 2919311 | 56149    |

The file `unit_conversions.csv` defines the unit converstions that are
used when converting traits to common units, e.g.

| unit\_from   | unit\_to | function    |
| :----------- | :------- | :---------- |
| % dry weight | mg/g     | x\*10       |
| %            | mg/g     | x\*10       |
| %            | g/g      | x\*0.01     |
| cm2/g        | mm2/mg   | x\*0.1      |
| cm2          | mm2      | x\*100      |
| g            | mg       | x\*1000     |
| mm           | m        | x\*0.001    |
| %            | mg/g     | x\*10       |
| g/m2         | mm2/mg   | 1/x\*1000/1 |
| mmol/g       | mg/g     | x\*14.0067  |

## R

## Data

The folder `data` contains the raw data from individual studies included
in AusTraits. Data from each study is organised into a separate folder,
with two files:

  - `data.csv`: a table containing the actual trait data.
  - `metadata.yml`: contains information about the study, maps trait
    names and units onto standard types, and lists any substitutions
    applied to the data in processing.

The folder `data` thus contains a long list of folders, one for each
study and each containing two files:

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

where `Angevin_2010`, `Barlow_1981`, `Bean_1997`, `Westoby_2014` are
each a unique `dataset_id` in the final dataset.

### Data.csv

The file `data.csv` file contains raw measurements and can be in either
long or wide format.

Required columns include the species name, the trait name (column in
long format, header in wide format), units (column in long format, part
of header in wide format), site (if applicable), and trait values.

If multiple trait measurements were made on the same individual or are
the mean of a species’ measurements from the same site, they should be
kept linked. If the data is in wide format, each row should represent
measurements made on a single individual or a single species-by-site
mean, with different trait values as consecutive columns. If the data is
in long format, an observation ID (or other identifier) must be assigned
to identify which measurements are linked to a unique individual or
site.

We aim to keep the data file in rawest form possible (i.e. as few
changes as possible) but it must be a single csv file. Additional custom
R code may be required to make the file exactly compatible with the
AusTraits format, but these changes should be executed as AusTraits is
compiled and should be in the `metadata.yml` file under
`config/custom_R_code` (see below). Any files used to create the
submitted `data.csv` file (e.g. Excel …) should be archived in a
subfolder within the study folder named `raw`.

### Metadata.yml

The metadata is compiled in a `.yml` file, a structured data file where
information is presented in a hierarchical format (see [Appendix for
details](#yaml)). There are 9 values at the top hierarchical level:
source, people, dataset, sites, config, traits, substitutions,
taxonomic\_updates, questions. These are each described below.

As a start, you may want to checkout some examples from [existing
studies in
Austraits](https://github.com/traitecoevo/austraits/tree/master/data),
e.g.
[Angevin\_2010](https://github.com/traitecoevo/austraits/blob/master/data/Angevin_2010/metadata.yml)
or
[Wright\_2004](https://github.com/traitecoevo/austraits/blob/master/data/Wright_2004/metadata.yml).

#### Source

This section provides citation details for the original source(s) for
the data, whether it is a published journal article, book, website, or
thesis. In general we aim to reference the primary source. References
are written in structured yml format, under the category `source` and
then sub-groupings `primary` and `secondary`. General guidelines for
describing a source

  - maximum of one primary and secondary source allowed
  - elements are names as in [bibtex
    format](https://en.wikipedia.org/wiki/BibTeX)
  - keys should be named in the format `Surname_year` and should be
    identical to the name given to the dataset folder.
  - a secondary source may be needed if the main collector is not an
    author on the paper where data was released, or data were otherwise
    released via a subsequent study.
  - if your data is from an unpublished study, only include the elements
    that are applicable

Following are some examples for different types of source.

A journal article:

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

A book:

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

An online resource:

    source:
      primary: key: WAH_1998
        bibtype: Misc
        author: Western Australian Herbarium
        year: 1998
        title: FloraBase--the Western Australian Flora
        publisher: Department of Parks and Wildlife
        url: https://florabase.dpaw.wa.gov.au/
      secondary: .na

An unpublished resource:

    source:
      primary:
        key: Duncan_1998
        bibtype: Unpublished
        author: David H. Duncan
        year: 1998
        title: Leaf anatomy of Australian plant species
        note: Collected while at Macquarie University
      secondary: .na

Note that in these first examples `secondary` is set as `.na`. If a
secondary source is included it may look like:

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

This section provides a list of the key contributors to the study, their
respective institutions and roles in the study. Roles are defined as
follows:

| key         | value                                                           |
| :---------- | :-------------------------------------------------------------- |
| collector   | The person (people) leading data collection (up to 2 permitted) |
| contributor | Person responsible for entering data into AusTraits             |
| lab\_leader | Leader of lab group at time of collection                       |
| assistant   | Anyone else who assisted in collection of the data              |
| contact     | The person to contact with questions about the data set         |

An example is as follows:

    people:
    - name: Daniel Falster
      institution: Macquarie University
      role: collector, contact, contributor
    - name: Mark Westoby
      institution: Macquarie University
      role: lab_leader

Note that only the AusTraits custodians have the contributors e-mail
addresses on file. This information will not be directly available to
AusTraits users or new contributors via Github.

#### Dataset

This section details about the study where the data were collected

The following elements are included under the element `dataset`:

  - **year\_collected\_start**: The year data collection commenced
  - **year\_collected\_end**: The year data collection was completed
  - **description**: A 1-2 sentence description of the purpose of the
    study.
  - **collection\_type**: A field to indicate where the plants on which
    traits were measured were collected - in the `field`, `lab`,
    `glasshouse`, `herbarium specimens`, or `literature`. The latter
    should only be used when the data were sourced from the literature
    and the collection type is unknown.
  - **sample\_age\_class**: A field to indicate if the study was
    completed on `adult` or `juvenile` plants
  - **sampling\_strategy**: A written description of how study sites
    were selected and how study individuals were selected. When
    available, this information is lifted verbatim from a published
    manuscript. For herbarium studies, this field ideally indicates
    which herbaria were ‘sampled’ to measure a specific trait
  - **original\_file**: The name of the file initially submitted to
    AusTraits
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

  - **data\_is\_long\_format**: Indicates if the data spreadsheet has a
    vertical (long) or horizontal (wide) configuration with `yes` or
    `no` terminology
  - **variable\_match**: Identifies which information is in each column
    in the file `data.csv`, excluding the actual trait data. One element
    within `variable_match` must be `species_name`. Datasets with
    `data_is_long_format` set to `yes` must also identify which column
    includes data on `value` and `trait_name`. Other allowed values
    include `site_name` and `observation_id`, if these columns are
    present in the `data.csv` file.
  - **custom\_R\_code**: A field where additional R code can be
    included. This allows for custom manipulation of the data in the
    submitted spreadsheet into a different format for easy integration
    with AusTraits. `.na` indicates no custom R code was used.

An example is

    config:
      data_is_long_format: yes
      variable_match:
        species_name: Taxon
        value: trait value
        trait_name: trait
      custom_R_code: mutate(data, `trait value` = ifelse(trait == 'flowering time', convert_month_range_vec_to_binary(`trait value`), `trait value`))

A common use of the `custom_R_code` is to automate the conversion of a
verbal description of flowering or fruiting periods into the supported
trait values, as occurs in this example. It might also be used if values
for a single trait are expressed across multiple columns and need to be
merged. See the `Catford_2014` as an example of this. Additional
examples on adding `custom_R_code` are provided [in the
appendix](#custom_R_code).

#### Traits

This section A translation table mapping traits and units in the
original study onto corresponding variables in AusTraits. Also specified
here are methods used to collect the data.

For each trait submitted to Austraits, there is the following
information:

  - **var\_in**: Names of trait in the original data submitted
  - **unit\_in**: Units of trait in the original data submitted
  - **trait\_name**: Name of trait sampled. Allowable values specified
    in the table `traits`.
  - **value\_type**: A categorical variable describing the type of trait
    value recorded
  - **replicates**: Number of replicate measurements that comprise the
    data points for the trait for each measurement. A numeric value (or
    range) is ideal and appropriate if the value type is a `mean`,
    `median`, `min` or `max`. For these value types, if replication is
    unknown the entry should be `unknown`. If the value type is
    `raw_value` the replicate value should be `1`. If the value type is
    `expert_mean`, `expert_min`, or `expert_max` the replicate value
    should be `.na`.
  - **methods**: A textual description of the methods used to collect
    the trait data. Whenever available, methods are taken near-verbatim
    from referenced souce. Methods can include descriptions such as
    ‘measured on herbarium specimens’,‘data from the literature’, or a
    detailed description of the field or lab methods used to collect the
    data.

Values under `trait_name` must be allowable values, as described under
[the section trait definitions](#trait_defs). Similarly, values under
`value_type` must be allowable values, as described under [the section
Value types](#value_types).

An example is as follows:

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

#### Substitutions

This section provides a list of any “find and replace” substitutions
needed to get the data to get it into the right format

Substitutions are required whenever the exact word(s) used to describe a
categorical trait value in AusTraits are different from the vocabulary
used by the author in the `data.csv` file. It is preferable that authors
make changes using `substitutions` rather than changing the `data.csv`
file. See the section [trait definitions](#trait_defs) for a list of
supported values for each trait.

Each substitution is documented using the following elements:

  - **trait\_name**: Name of variable to search
  - **find**: Value to find
  - **replace**: Replacement value

An example is as follows:

    substitutions:
    - trait_name: life_history
      find: p
      replace: perennial
    - trait_name: plant_growth_form
      find: s
      replace: shrub
    - ...

#### Taxonomic updates

This section provides a list of taxonomic name changes needed to align
with current taxonomy Taxonomic updates are required to align species
names with an accepted species list (for details, see details on
[Taxonony](#taxonomy).

Each substitution is documented using the following elements:

  - **find**: Taxonomic name given to species in the original data
    supplied by the authors
  - **replace**: Name of species after aligning taxonomy with standard
    sources.
  - **reason**: Records why the change was implemented, e.g. `typos`,
    `taxonomic synonyms`, and `standardising spellings`

An example is as follows:

    taxonomic_updates:
    - find: Carissa lanceolata
      replace: Carissa spinarum
      reason: Synonym reported by TaxonStand (2018-09-19)
    - find: Melaleuca pallida
      replace: Callistemon pallidus
      reason: Synonym reported by TaxonStand (2018-09-19)

#### Questions

This section provides a place to record any queries we have about the
dataset (recorded as a named array), including notes on any additonal
traits that may have been collected in the study but have not been
icnoproated into austraits.

An example is as follows:

    questions:
      questions for author: Triglochin procera has very different seed masses in the main traits spreadsheet and the field seeds worksheet. Which is correct? There are a number of species with values in the field leaves worksheet that are absent in the main traits worksheet - we have included this data into Austraits; please advise if this was inappropriate.
      austraits: need to map aquatic_terrestrial onto an actual trait once one is created.

# Adding data to Austraits

Within the compiled matrix `austraits` are the elements listed in
[Elements of AusTraits](#elements). The actual trait data can be found
in the dataframe `austraits$traits`.

## Overview

This sections explains how to format files to contribute a new study to
AusTraits. It is important that all steps are followed so that our
automated workflow proceeds without problems.

The main steps are:

1.  Clone the `austraits.build` repository from github and create a new
    branch in the repo with the name of your dataset\_id (for more info,
    see the document [working with github](Working_with_github.Rmd).
2.  Create a new folder within the folder `data` with a name of the
    `dataset_id`, e.g. `Gallagher_2014`
3.  Prepare the files `data.csv` and `metadata.yml` and place them
    within the folder, as per instructions [below](#create_data).
4.  Add the new study into the build framework and rebuild Austraits, as
    described under [adding data to Austraits](#adding) below.
5.  Run tests on the contributed data and correct the `data.csv` and
    `metadata.yml`files as necessary. See the section
    [tests](#running_tests) for details.
6.  Generate and proofread a report on the data. In particular, check
    that numeric trait values fall within a logical range relative to
    other studies and that individual trait observations are not
    unnecessarily excluded because their trait values are unsupported.
    See the section on generating [reports](#reports) for details.
7.  Return to step 3 if changes are made to the `data.csv` or
    `metadata.yml` files.
8.  Push to GitHub.

It may help to download one of the [existing
datasets](https://github.com/traitecoevo/austraits/tree/master/data) and
use it as a template for your own files and a guide on required content.
You should look at the files in the [config
folder](https://github.com/traitecoevo/austraits/tree/master/config), in
particular the `definitions` files for the list of traits we cover and
the supported trait values for each trait. Or read through information
on the \[\] supported traits and trait values\](XXXX.)

Once you have prepared your `data.csv` and `metadata.yml` files within a
folder in the `data` directory, you can incorporate the new data into
Austraits by running:

``` r
austraits_rebuild_remake_setup()
```

This step updates the file `remake.yml` with appropriate rules for the
new dataset; similarly if you remove datasets, do the same. (At this
stage, [remake](https://github.com/richfitz/remake) offers no looping
constructs so for now we generate the remake file using
[whisker](https://github.com/edwindj/whisker).)

You can then rebuild Austraits, including your dataset.

## Constructing the `data.csv` file

<!-- Lizzy - add details -->

## Constructing the `metadata.yml` file

One way to construct the `metadata.yml` file is to use one of the
existing files and modify yours to follow the same format. As a start,
checkout some examples from [existing studies in
Austraits](https://github.com/traitecoevo/austraits/tree/master/data),
e.g.
[Angevin\_2010](https://github.com/traitecoevo/austraits/blob/master/data/Angevin_2010/metadata.yml)
or
[Wright\_2004](https://github.com/traitecoevo/austraits/blob/master/data/Wright_2004/metadata.yml).

Note, when editing the `metadata.yml`, edits should be made in a proper
text editor (Microsoft word tends to stuff up the formatting). For
example, Rstudio works.

To assist you in constructing the `metadata.yml` file, we have developed
functions to help fill in the different sections of the file. If you
wish to include additional elements, you can afterwards edit the file
further.

To use the functions, make sure you first run the following, to make the
functions available

### Creating a template

The first function creates a basic template for your the `metadata.yml`
file for your study. Assuming you have already created a file `data.csv`
in the folder `data/your_dataset_id`, run

The function will ask a series of questions and then create a relatively
empty file `data/your_dataset_id/metadata.yml`. The key questions are:

  - Is the data long vs wide? A wide dataset has each
    variable(i.e. trait ) as a column. A long dataset has each variable
    as a row and column as a species. *Select column for ‘species\_name’
    *Select column for ‘trait\_name’

### Adding a source

Three functions are available to help entering citation details for the
source data.

The function `metadata_create_template` creates a template for the
primary source with default fields for a journal article, which you can
then edit manually.

Alternatively, if you have a `doi` for your study, use the function:

``` r
metadata_add_source_doi(dataset_id, doi)
```

and the different elements within source will automatically be
generated. By default, details are added as a primary source. To
override this, specify the type

``` r
metadata_add_source_doi(dataset_id, doi, type="secondary")
```

Alternatively, if you have reference details saved in a bibtex file
called `myref.bib` you can use the function

``` r
metadata_add_source_doi(dataset_id, file = "myref.bib")
```

(These options require the package
[rcrossref](https://github.com/ropensci/rcrossref) and
[RefManageR](https://github.com/ropensci/RefManageR/) to be installed.)

### Adding people

The function `metadata_create_template` creates a template for entering
details about people, which you can then edit manually.

### Custom R code

Occasionally all the changes we want to make to dataset may not fit into
the prescribed workflow used in Austraits. For example, we assume each
trait has a single unit. But there are a few datasets where data on
different rows have different units. So we want to make to make some
custom modifications to this particular dataset before the common
pipeline of operations gets applied. To make this possible, the workflow
allows for some custom R code to be run as a first step in the
processing pipeline. That pipeline (in the function
[`load_study`](https://github.com/traitecoevo/austraits/blob/master/R/steps.R))
looks like this:

``` r
  data <- 
    read_csv(filename_data_raw, col_types = cols()) %>%
    custom_manipulation(metadata[["config"]][["custom_R_code"]])() %>%
    parse_data(dataset_id, metadata) %>%
    ...
```

Note the second line. After loading the csv file, we can apply some
Custom R code small manipulations to the dataframe before processing it.
Custom R code is valid R code, but written inside the `metadata.yml`
file. While developing this, you’ll want to test your code. This can be
achieved by running the function

which returns a data frame, showing how the datasets looks after being
manipulated.mes

### Definting traits

Add
traits

<!-- TODO: You will be asked to indicate the columns you wish to keep as distinct traits -->

### Site detials

Add sites details

This function assumes you have site details stored in wide format, in
R:

<!-- TODO: need to indicate that the following code needs to be run with the assignment site_data for the previous code to work. -->

    ## # A tibble: 2 x 6
    ##   site_name        description                                            `elevation (m)` `latitude (deg)` `longitude (deg)` `rainfall (mm)`
    ##   <chr>            <chr>                                                            <dbl>            <dbl>             <dbl>           <dbl>
    ## 1 Atherton         Tropical rain forest vegetation.                                   800            -17.1              146.            2000
    ## 2 Cape Tribulation Complex mesophyll vine forest in tropical rain forest.              25            -16.1              145.            3500

If your data is in a file, you’ll need to read it in first.

### Using substitutions

Substitutions can be added by running:

``` r
metadata_add_substitution(dataset_id, trait_name, find, replace)
```

where `find` is the trait value used in the data.csv file and `replace`
is the trait value supported by Austraits.

### Update taxonomy

We’ve implemented code to semi-automate the checking of names using the
R package
[Taxonstand](https://cran.r-project.org/web/packages/Taxonstand/index.html)
(for more documentation see
[here](https://www.rdocumentation.org/packages/Taxonstand/versions/2.1/topics/TPL)).
To generate a suggested name change for a specific study run:

``` r
metadata_check_taxa("Westoby_2014")
```

If TaxonStand finds taxonomic changes to make it will add the relevant
lines of code directly to the metadata.yml file.

TaxonStand has been configured in the above function to only permit
relatively certain changes (e.g. with a minor change to spelling or
known synonym).

There are additional arguments you can add for the function
`metadata_check_taxa` including: - `update` where the default is TRUE,
meaning changes found will be added to the `metadata.yml` file - `typos`
where the default is FALSE, meaning typos will not be corrected -
`diffchar` which indicates the number of characters that can be
different for a typo-match. Here the default is two.

Therefore, if you want the function `metadata_check_taxa` to correct 1
and 2 character typos, run the function as follows:

``` r
metadata_check_taxa("Westoby_2014", typos=TRUE)
```

If TaxonStand fails to find a suitable alignment, and you have
identified one yourself, you can add it to the metadata by running

``` r
metadata_add_taxnomic_change(study, find, replace, reason)
```

## Running tests

You can also run some automated tests to ensure the dataset meets
required setup. The tests run through a collection of pre-specified
checks on the files for each study. The output alerts you to possible
issues needing to be fixed, by comparing the data in the files with
expected structure and allowed values, as specified in the definitions.

To run the tests, the variable `dataset_ids` must be defined in the
global namespace, containing a vector of ids to check. For example

``` r
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

To enable better quality checks we have code to generate a report on the
data in each study.

(Reports are written in
[Rmarkdown](https://rstudio.github.io/rmarkdown/) and generated via the
[knitr](https://cran.r-project.org/web/packages/knitr/) package. The
template is stored in the folder `vignettes` called
`report_study.html`).

To generate a report for a particular study:

``` r
austraits <- remake::make("austraits")
source("R/report_utils.R")
build_study_report("Wright_2002")
```

**Guidelines for writing report code**

  - use [knitr chunk
    options](https://rmarkdown.rstudio.com/lesson-3.html) to customise
    when code is shown and how output is displayed.
  - use [tidyverse style and
    format](http://htmlpreview.github.io/?https://github.com/nicercode/2018_BEES_regression/blob/master/tidyverse.html)
  - use [kableExtra for styling
    tables](https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html)

**Maps:** We use the package
[leaflet](https://cran.r-project.org/web/packages/leaflet/index.html) to
generate interactive maps via the JavaScript ‘Leaflet’ framework and
based on the [Open street map](https://www.openstreetmap.org/).

## Pushing to GitHub

By far our preferred way of contributing is for you to fork the database
in github, add your dataset then send us a [pull
request](https://help.github.com/articles/using-pull-requests/). If this
is not possible, you could email the relevant files (see above) to
Rachael Gallagher.

# Appendices

## File types

### CSV

### YAML files

yml: The `yml` file extension (pronounced “YAML”) [is a type structured
data file](https://en.wikipedia.org/wiki/YAML), that is both human and
machine readable. The information in it appears under various labels,
which are imported into AusTraits. You can edit it any text editor, or
also in Rstudio. Generally, yml is used in situations where a table does
not suit because of variable lengths and or nested structures. This
document outlines how to describe metadata for any given study

The metadata is compiled in a `.yml` file, a structured data file where
information is presented in a hierarchical format. It has the advantage
over a spreadsheet in that the nested “headers” can have variable
numbers of categories. The data under each of the hierarchical headings
are easily extracted by R.

Assumed character unless specified otherwise

possible types:

  - `character`:
  - `numeric`: must have fields `description`, `type`, `units`, `values`
    with filed `minimum` and `maximum`.
  - `categorical`: Unlike character, only specific values are allowed.
    must have fields `description`, `type`,and `values`, with the latter
    including a list of possible values.
  - `table`:
  - `list`:
  - `array`:

## Adding custom R code into metadata.yml

Occasionally all the changes we want to make to dataset may not fit into
the prescribed workflow used in AusTraits. For example, we assume each
trait has a single unit. But there are a few datasets where data on
different rows have different units. So we want to make to make some
custom modifications to this particular dataset before the common
pipeline of operations gets applied. To make this possible, the workflow
allows for some custom R code to be run as a first step in the
processing pipeline. That pipeline (in the function
[`read_data_study`](https://github.com/traitecoevo/austraits/blob/master/R/steps.R#L59))
looks like:

``` r
  data <- 
    read_csv(filename_data_raw, col_types = cols()) %>%
    custom_manipulation(metadata[["config"]][["custom_R_code"]])() %>%
    parse_data(dataset_id, metadata) %>%
    ...
```

Note the second line.

### Example problem

As an example, `Barlow_1981` has multiple units per traits. Check it
out:

Load the data

Here’s the problem - note tat several traits have multiple units used:

``` 
                     cm    m   mm
```

dispersal 0 0 0 flowering time end 0 0 0 flowering time start 0 0 0 leaf
length maximum 2570 7 836 leaf length minimum 2489 7 815 leaf type 0 0 0
leaf width maximum 1050 0 1986 leaf width minimum 1005 0 1974 lifeform 0
0 0 longevity 0 0 0 plant height maximum 831 2371 6 plant height minimum
200 862 0 seed breadth maximum 5 0 72 seed breadth minimum 5 0 72 seed
length maximum 33 0 718 seed length minimum 34 0 733 seed shape 0 0 0
seed width maximum 22 0 404 seed width minimum 22 0 407

So you want to write some R code that fixes this and gets the dataset
into the processing pipeline, satisfying all the assumptions.

### Developing solutions

We want to write some custom R code that will appear in the
`metadata.yml` file for that study, under a title `config` -\>
`custom_R_code`. E.g. see this example for
[data/Barlow\_1981/metadata.yml](https://github.com/traitecoevo/austraits/blob/master/data/Barlow_1981/metadata.yml).

Your code should assume a single object called `data`. And apply
whatever fixes are needed. Also it should be

  - fully self contained (we’re not going to use any of the other remake
    machinery here)
  - have semi colons `;` at the end of each line. This will be needed
    because we’re adding the code to the `metadata.yml` file and
    newlines get lost when reading in the file.

The workflow is to first develop some code that applies a suitable fix.
E.g. for Barlow\_1981 here is the code we eventually applied to the
object `data` loaded above:

Running this removes the problem with multiple units:

``` r
table(data$trait, data$units)
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

(we can ignore the NULLs here, these are when data and units are both
NA. Those get pruged furtehr down the pipeline).

Once you have some working code, you then want to add it into your yml
file under a group `config` -\> `custom_R_code`.

And then check it works.

Let’s assume you added it in, so we’ll load the metadata (and also
reload the data)

In the build process we use the function `custom_manipulation` to create
a function that accepts a data frame and modifies it according to the
code in `txt`

``` r
custom_manipulation
```

function (txt) { if (\!is.null(txt) && \!is.na(txt) && nchar(txt) \> 0)
{ function(data) { eval(parse(text = txt), env = new.env()) } } else {
identity } }

So now lets use it to create a function

``` r
f <- custom_manipulation(metadata[["config"]][["custom_R_code"]])
f
```

    ## function (data) 
    ## {
    ##     eval(parse(text = txt), env = new.env())
    ## }
    ## <environment: 0x7fd9313da080>

And finally we can apply the function to our data:

``` r
data2 <- f(data)
```

(If it fails at this point it won’t work in the build).

Now let’s compare it to our original data (the columns units and values
should now differ)

``` r
all.equal(data, data2)
```

    ## [1] "Different number of rows"

And also see the units and traits:

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

Finally, check it works in the context of loading the metadata:

    ## [1] "Different number of rows"

Now you’re ready to go.
