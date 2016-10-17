---
title: "Metadata documents for austraits"
output: github_document
---

This document outlines how to describe metadata for any given study

Is a yaml file.

# Description of main components

## Sources

# Sources

In general we aim to reference the primary source. References are written in structured yml format, under the category `source` and then under titles `primary` and `secondary`. Here are some examples for different types of source:

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

A journal article:

```
source:
  primary:
    key: Falster_2005
    bibtype: Article
    author: Daniel S. Falster, Mark Westoby
    year: 2005
    title: Alternative height strategies among 45 dicot rain forest species from tropical
      Queensland, Australia
    journal: Journal of Ecology
    volume: 93
    pages: 521--535
    publisher: Wiley-Blackwell
    doi: 10.1111/j.0022-0477.2005.00992.x
    url: http://dx.doi.org/10.1111/j.0022-0477.2005.00992.x
  secondary: .na
```

An online resource:

```
source:
  primary:
    key: WAH_1998
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
    author: Jerome Chave, David Coomes, Steven Jansen, Simon L. Lewis, Nathan G. Swenson,
      Amy E. Zanne
    year: 2009
    title: Towards a worldwide wood economics spectrum
    journal: Ecology Letters
    volume: 12
    pages: 351--366
    publisher: Wiley-Blackwell
    doi: 10.1111/j.1461-0248.2009.01285.x
    url: http://dx.doi.org/10.1111/j.1461-0248.2009.01285.x
  secondary:
    key: Zanne_2009
    bibtype: Misc
    author: Amy E. Zanne, G. Lopez-Gonzalez, David A. Coomes, Jugo Ilic, Steven Jansen,
      Simon L. Lewis, Regis B. Miller, Nathan G. Swenson, Michael C. Wiemann, Jerome
      Chave
    year: 2009
    title: 'Data from: Towards a worldwide wood economics spectrum'
    volume: .na
    pages: .na
    publisher: Dryad Digital Repository
    doi: 10.5061/dryad.234
    url: https://doi.org/10.5061/dryad.234
 ```

General guidelines for describing a source

- elements are names as in [bibtex format](https://en.wikipedia.org/wiki/BibTeX)
- maximum of one primary and secondary source allowed
- a secondary source may be needed if the main collector is not an author on the paper where data was released, or data were otherwise released via a subsequent study.
- keys should be named in the format `Surname_year`.


## people

## dataset

## config

## traits

## substitutions

# Suggestions for developing

The elements `traits` and `substitutions` are designed to covert into tables. You can see how they look as tables with code like the following:

```
source("R/support.R")

metadata <- read_yaml("data/dataset_002/metadata.yml")

list_to_df(metadata[["traits"]])

list_to_df(metadata[["substitutions"]])
```

Similarly we can convert a table to yaml as follows:

```
x <- list_to_df(metadata[["traits"]])
mylist <- df_to_list(x)
write_yaml(mylist, "tmp.yml")
```
