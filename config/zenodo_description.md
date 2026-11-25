AusTraits is a transformative database, containing measurements on the traits of Australia's plant taxa, standardised from hundreds of disconnected primary sources. So far, data have been assembled from > 300 distinct sources, describing > 500 plant traits and > 34,000 taxa.

To handle the harmonising of diverse data sources, we use a reproducible workflow to implement the various changes required for each source to reformat it suitable for incorporation in AusTraits. Such changes include restructuring datasets, renaming variables, changing variable units, changing taxon names. While this repository contains the harmonised data, the raw data and code used to build the resource are also available on the project's GitHub repository, <https://github.com/traitecoevo/austraits.build/>.

Further information on the project is available at the project website [austraits.org](https://austraits.org) and in the associated publication (see below).

**CONTRIBUTORS**

The project is jointly led by Prof Daniel Falster (UNSW Sydney), Prof Rachael Gallagher (Western Sydney University), Dr Elizabeth Wenk (UNSW Sydney), and Dr Hervé Sauquet (Royal Botanic Gardens and Domain Trust Sydney), with input from > 300 contributors from > 100 institutions (see full list above). The project was initiated by Prof Rachael Gallagher and Prof Ian Wright while at Macquarie University.

We are grateful to the following institutions for contributing data: the Council of Heads of Australasian Herbaria (CHAH; Australasian Virtual Herbarium); the Australian Government Department of Climate Change, Energy, the Environment and Water (DCCEEW), including the Australian Biological Resources Study; the Australian National Botanic Gardens and the Centre for Australian National Biodiversity Research; the National Herbarium of New South Wales (Botanic Gardens of Sydney); the Northern Territory Herbarium; the Queensland Herbarium; the State Herbarium of South Australia; the Tasmanian Herbarium; the Western Australian Herbarium; the Royal Botanic Gardens Victoria and the Victorian Department of Environment, Land, Water and Planning; the Royal Botanic Gardens, Kew; and the Brisbane Rainforest Action and Information Network.

AusTraits has been supported by investment from the Australian Research Data Commons (ARDC), via their "Transformative data collections" (https://doi.org/10.47486/TD044) and "Data Partnerships" (https://doi.org/10.47486/DP720, https://doi.org/10.47486/DP720A) programs; and grants from the Australian Research Council (FT160100113, DE170100208, FT100100910) and Macquarie University. The ARDC is enabled by National Collaborative Research Investment Strategy (NCRIS).

**ACCESSING AND USE OF DATA**

The compiled AusTraits database is released under an open source licence (CC-BY), enabling re-use by the community.

A requirement of use is that users cite the AusTraits resource paper, which includes all contributors as co-authors:

> Falster, Gallagher et al (2021) *AusTraits, a curated plant trait database for the Australian flora*. Scientific Data 8: 254, <https://doi.org/10.1038/s41597-021-01006-6>

In addition, we encourage users to cite the original data sources, wherever possible.

Note that under the license data may be redistributed, provided the attribution is maintained.

AusTraits is available in two forms, each in several formats: the full database, with every record and its context, and the observed means, one value per taxon and trait.

The full database:

- austraits-{{version}}.zip: data in plain text format (.csv, .bib, .yml files). Suitable for anyone, including those using Python.
- austraits-{{version}}.rds: data as compressed R object. Suitable for users of R (see below).
- austraits-{{version}}-flattened.rds: contains a flattened version of the dataset for direct loading in R; all data tables are joined into a wider format
- austraits-{{version}}-flattened.parquet: contains a flattened version of the dataset in parquet format; all data tables are joined into a wider format

The observed means (the mean, or for categorical traits the most frequent value, of each trait for each taxon, computed from the records):

- austraits-{{version}}-observed-means.zip: in plain text format (.csv), with the dictionary describing their columns
- austraits-{{version}}-observed-means.rds: as compressed R object
- austraits-{{version}}-observed-means.parquet: in parquet format
- austraits-{{version}}-observed-means-dictionary.csv: describes their columns

For R users, either the [traits.use](https://github.com/traitecoevo/traits.use) package or the [austraits R package](https://traitecoevo.github.io/austraits/) can be used to download AusTraits releases and to load and query the data.

**OTHER WAYS TO ACCESS AUSTRAITS**

This record holds the full releases. The same data can also be reached in other ways (see <https://austraits.org/#access>):

- the [AusTraits data portal](https://austraits.org/portal), to browse, filter and download data in the browser, without code;
- [tutorials](https://traitecoevo.github.io/traits.build-book/content/AusTraits_tutorial.html) with worked examples of exploring and analysing the data;
- taxon-level trait summaries on species pages of the [Atlas of Living Australia](https://www.ala.org.au).

**STRUCTURE OF AUSTRAITS**

The compiled AusTraits database contains a series of relational tables and files. These elements include all the data, contextual information submitted with each contributed datasets, database schema, and trait definitions. The flattened files (austraits-{{version}}-flattened.rds and .parquet) hold the same data as a single table, with all the tables joined onto each trait record. The file dictionary.html provides the same information in textual format. Similar information is available at <https://traitecoevo.github.io/traits.build-book/>.

**CONTRIBUTING**

We envision AusTraits as an on-going collaborative community resource that:

1. Increases our collective understanding of the Australian flora;
2. Facilitates accumulation and sharing of trait data;
3. Builds a sense of community among contributors and users; and
4. Aspires to fully transparent and reproducible research of the highest standard.

As a community resource, we are very keen for people to contribute. Assembly of the database is managed on GitHub at <https://github.com/traitecoevo/austraits.build/>.

Here are some of the ways you can contribute:

**Using and citing the data**: Use AusTraits in your research and cite it (see above), along with the original data sources.

**Reporting Errors**: If you notice a possible error in AusTraits, please [post an issue on GitHub](https://github.com/traitecoevo/austraits.build/issues).

**Refining documentation:** We welcome additions and edits that make using the existing data or adding new data easier for the community.

**Contributing new data**: We gladly accept new data contributions to AusTraits. See full instructions on how to contribute at <https://github.com/traitecoevo/austraits.build/>.
