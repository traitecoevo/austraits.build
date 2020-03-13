# Austraits: A compilation of traits for Australian plant species

## About

The Austraits database (Austraits)  is an open-source platform for ongoing compilation and distribution of data on the traits of Australian plant species. So far, data have been assembled from over 100 individual primary and secondary sources, describing more than 100 plant traits and over 18k species.  The project is being led by Dr Rachael Gallagher and Dr Daniel Falster at Macquarie University, Australia.

## Using Austraits

We are yet to determine a suitable license for the data in Austraits so data cannot as yet be used without further permissions. Our aim is to release an open database, once we have cleared this with our contributors.

In addition to any general license applying to the dataset, any usage requires acknowledgement of the following institutions permitting inclusion of specific resources in this compilation:  

- Barlow_1981: Australian Biological Resources Study.
- BRAIN_2007: Brisbane Rainforest Action and Information Network
- Hyland_2003, CPRR_2002: CSIRO's Centre for Australian National Biodiversity Research
- NTH_2014: Northern Territory Herbarium
- RBGK_2014, Kew_2019_1, Kew_2019_2, Kew_2019_3, Kew_2019_4, Kew_2019_5, Kew_2019_6: Kew Botanic Gardens
- NHNSW_2016, RBGSYD_2014_2, RBGSYD_2014: PLantNet, National Herbarium of NSW
SAH_2014: State Herbarium of South Australia

## Building the database from source

Austraits can be rebuilt from source (raw data files) using our scripted workflow in R. The idea of this repository is to provide a fully-transparent process of combining data from multiple sources.

First download the code and raw data from github as either [zip file](https://github.com/traitecoevo/austraits/archive/master.zip), or by cloning the Austraits repository:

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

## Docker container for reproducible compute environment 

As the R compute environment and packages change over time, we use a Docker container for all our builds to ensure reproducibility. 

If you have [Docker installed](https://hub.docker.com), you can recreate the compute environment as follows. For more instructions on running docker, see the info from the R docker project [rocker](https://hub.docker.com/r/rocker/rstudio).

First fetch the container:

```
docker pull traitecoevo/austraits.build:1.0
```

Then launch it via:

```
docker run --user root -v $(pwd):/home/rstudio/ -p 8787:8787 -e DISABLE_AUTH=true traitecoevo/austraits.build:1.0
```

Adding a `-d` into the command above will cause the image to run in the background. 

The code above initialises a docker container, which runs an rstudio session, which is accessed by pointing your browser to [localhost:8787](http://localhost:8787). 

Note, this container does not contain the actual github repo, only the software environment. If you run the above command from within your downloaded repo, it will map the working directory as the current working directory inside the docker container.


The recipe used to build the docker container is included in the Dockerfile in this repo. Our image builds off [`rocker/verse:3.6.1` container](https://hub.docker.com/r/rocker/verse) via the following command, in a terminal contained within the downloaded repo:

```
docker build -t traitecoevo/austraits.build:1.0 .
```

Images are pushed to dockerhub ([here](https://cloud.docker.com/u/traitecoevo/repository/docker/traitecoevo/austraits.build)):

```
docker push traitecoevo/austraits.build:1.0
```

## Using AusTraits

To load the dataset into R:

```
austraits <- readRDS('export/austraits.rds')
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
