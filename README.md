# Austraits: A compilation of traits for Australian plant species

## About

The Austraits database (Austraits)  is an open-source platform for ongoing compilation and distribution of data on the traits of Australian plant species. So far, data have been assembled from over 100 individual primary and secondary sources, describing more than 100 plant traits and over 18k species.  The project is being led by Dr Rachael Gallagher and Dr Daniel Falster at Macquarie University, Australia.

## Using Austraits

We are yet to determine a suitable license for the data in Austraits so data cannot as yet be used without further permissions. Our aim is to release an open database, once we have cleared this with our contributors.

## Building the database from source

Austraits can be rebuilt from source (raw data files) using our scripted workflow in R. The idea of this repository is to provide a fully-transparent process of combining data from multiple sources.

First download the code and raw data, either from Ecological Archives or from github as either [zip file](https://github.com/traitecoevo/austraits/archive/master.zip), or by cloning the Austraits repository:

```
git clone git@github.com:traitecoevo/austraits.git
```

Then open R and set the downloaded folder as your working directory. Beyond base R, building of Austraits requires the package ['remake'](https://github.com/richfitz/remake). To install remake from within R, run:

```
# installs the package devtools
install.packages("devtools")
# use devtools to install remake
devtools::install_github("richfitz/remake")
```

A number of other packages are also required. These can be installed using remake:

```
remake::install_missing_packages()
```

Then build the dataset

```
# build the dataset
remake::make()
````

A copy of the dataset has been saved in the folder `export` as an `rds` (compressed data for R) file.

To load the dataset into R:

```
austraits <- readRDS('export/austraits.rds')
```
or, alternatively you can skip the export step:

```
austraits <- remake::make('austraits')
```

The database contains the following elements

- `data`: amalgamated dataset (table), with columns as defined in `dictionary`
- `metadata`: a table of metadata
- `context`: a table of contextual site variables, where available

These elements are available at both of the above links as a series of CSV and text files.

## Contributing data to Austraits

We welcome further contributions to Austraits. If you would like to contribute data, the minimal requirements are

1. Data were collected for Australian plant species growing in Australia
2. You collected data on one of the traits list in the [trait definitions table](config/definitions.yml).
3. You are willing to release the data under an open license for reuse by the scientific community.

See [these instructions](vignettes/CONTRIBUTING.md) on how to prepare data.

Once sufficient additional data has been contributed, we plan to submit an update to the first data paper, inviting as co-authors anyone who has contributed data.

## Acknowledgements

We are extremely grateful to everyone who has contributed data. We would also like to acknowledge the following funding sources for supporting the data compilation:

- an MQRF grant to RV Gallagher
- a Science and Industry Endowment Fund Grant to Falster (RP04-174).
