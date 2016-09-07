---
title: "Writing the metadata.yml"
output: github_document
---

Place to document structure of metadata file.

Is a yaml file.

# Description of main components

## source

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
