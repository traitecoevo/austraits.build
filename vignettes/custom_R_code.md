---
title: "Adding custom R code into metadata.yml"
output: github_document
---

Occasionally all the changes we want to make to dataset may not fit into the presrcibed workflow used in Austraits. For example, we assume each trait has a single unit. But there are a few datasets where data on different rows have different units. So we want to make to make some custom modifcations to this particualr dataset before the common pipeline of operations gets applied. To make this possible, the workflow allows for some custom R code to be run as a first step in the processing pipeline. That pipepline (in the function [`read_data_study`](https://github.com/traitecoevo/austraits/blob/master/R/steps.R#L59)) looks like:


```r
data <- read_csv(filename_data_raw)
data <- custom_manipulation(metadata[["config"]][["custom_R_code"]])(data)
data <- parse_data(dataset_id, data, metadata)
data <- add_all_columns(data, definitions_data)
data <- drop_unsupported(data, definitions_traits, categorical_trait_constraints)
data <- convert_units(data, definitions_traits, unit_conversion_functions)
```

Note the second line. 

# Example problem 

As an example, `dataset_093` has multplie units per traits. Check it out:


```r
library(readr)
library(yaml)
```

Load the data

```r
key <- "dataset_093"
data <- read_csv(file.path("../data", key, "data.csv"))
```

Here's the problem - note tat several traits have multiple units used:


```r
table(data$trait, data$units)
```

```
##                       
##                          cm    m   mm NULL
##   dispersal               0    0    0    0
##   flowering time end      0    0    0    0
##   flowering time start    0    0    0    0
##   leaf length maximum  2570    7  836    0
##   leaf length minimum  2489    7  815    0
##   leaf type               0    0    0    0
##   leaf width maximum   1050    0 1986    0
##   leaf width minimum   1005    0 1974    0
##   lifeform                0    0    0    0
##   longevity               0    0    0    0
##   plant height maximum  839 2367   13    0
##   plant height minimum  200  862    0  339
##   seed breadth maximum    5    0   72 1130
##   seed breadth minimum    5    0   72 1132
##   seed length maximum    33    0  718    0
##   seed length minimum    34    0  733    0
##   seed shape              0    0    0    0
##   seed width maximum     22    0  404    0
##   seed width minimum     22    0  407    0
```

So you want to write some R code that fixes this and gets the dataset into the porcessing pipeline, satisfying all the assumptions.

# Developing solutions

We want to write some custom R code that will appear in tje `metadata.yml` file for that study, under a title `config` -> `custom_R_code`. E.g. see this example for [data/dataset_093/metadata.yml](https://github.com/traitecoevo/austraits/blob/0f142e32dc6e8fc1b5f2d42257ea60289f212ace/data/dataset_093/metadata.yml#L32).

Your code should assume a single object called `data`. And apply whatever fixes are needed. Also it should be

- fully slef contained (we're not going to use any of the other remake machinery here)
- have semi colons `;` at the end of each line. This will be needed because we're adding the code to the `metadata.yml` file and newlines get lost when reading in the file.

The workflow is to first develop some code that applies a suitable fix. E.g. for dataset_093 here is the code we eventually applied to the object `data` loaded above:


```r
traits <- c("leaf length maximum", "leaf length minimum", "leaf width maximum", "leaf width minimum",
            "plant height maximum", "plant height minimum", "seed breadth maximum", "seed breadth minimum",
            "seed length maximum", "seed length minimum", "seed width maximum", "seed width minimum");
a <- list(cm=0.1, m=0.001);
for(t in traits){
  for(v in names(a)) {
    i <- which(data[["trait"]]==t & data[["units"]] == v);
    data[["value"]][i] <- as.character(as.numeric(data[["value"]][i]) * a[[v]]);
    data[["units"]][i] <- "mm";
    };
  };
```

Runnign this removes the problem with mutliple units:

```r
table(data$trait, data$units)
```

```
##                       
##                          mm NULL
##   dispersal               0    0
##   flowering time end      0    0
##   flowering time start    0    0
##   leaf length maximum  3413    0
##   leaf length minimum  3311    0
##   leaf type               0    0
##   leaf width maximum   3036    0
##   leaf width minimum   2979    0
##   lifeform                0    0
##   longevity               0    0
##   plant height maximum 3219    0
##   plant height minimum 1062  339
##   seed breadth maximum   77 1130
##   seed breadth minimum   77 1132
##   seed length maximum   751    0
##   seed length minimum   767    0
##   seed shape              0    0
##   seed width maximum    426    0
##   seed width minimum    429    0
```
(we can ignore the NULLs here, these are when data and units are both NA. Those get pruged furtehr down the pipeline).

Once you have some working code, you then want to add it into your yml file under a group `config` -> `custom_R_code`. E.g. see this example for [data/dataset_093/metadata.yml](https://github.com/traitecoevo/austraits/blob/0f142e32dc6e8fc1b5f2d42257ea60289f212ace/data/dataset_093/metadata.yml#L32).

And then check it works.

Let's assume you added it in, so we'll load the metadata (and also reload the data)


```r
data <- read_csv(file.path("../data", key, "data.csv"))
```

Here's the R code

```r
txt <- '
    traits <- c("leaf length maximum", "leaf length minimum", "leaf width maximum", "leaf width minimum",
            "plant height maximum", "plant height minimum", "seed breadth maximum", "seed breadth minimum",
            "seed length maximum", "seed length minimum", "seed width maximum", "seed width minimum");
    a <- list(cm=0.1, m=0.001);
    for(t in traits){
      for(v in names(a)) {
        i <- which(data[["trait"]]==t & data[["units"]] == v);
        data[["value"]][i] <- as.character(as.numeric(data[["value"]][i]) * a[[v]]);
        data[["units"]][i] <- "mm";
        };
      };
    data'
```

In the build process we use the function `custom_manipulation` to create a function that accepts a dataframe and modifies it according to the code in `txt`


```r
custom_manipulation <- function(txt) {
  if (!is.null(txt) && !is.na(txt)  && nchar(txt) > 0) {
    function(data) {eval(parse(text=txt), env=new.env())}
  } else {
    identity
  }
}
```

So now lets use it to create a function

```r
f <- custom_manipulation(txt)
f
```

```
## function(data) {eval(parse(text=txt), env=new.env())}
## <environment: 0x7f891e499860>
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
## [1] "Component \"units\": 11517 string mismatches"
## [2] "Component \"value\": 11513 string mismatches"
```

And also see the units and traits:


```r
table(data2$trait, data2$units)
```

```
##                       
##                          mm NULL
##   dispersal               0    0
##   flowering time end      0    0
##   flowering time start    0    0
##   leaf length maximum  3413    0
##   leaf length minimum  3311    0
##   leaf type               0    0
##   leaf width maximum   3036    0
##   leaf width minimum   2979    0
##   lifeform                0    0
##   longevity               0    0
##   plant height maximum 3219    0
##   plant height minimum 1062  339
##   seed breadth maximum   77 1130
##   seed breadth minimum   77 1132
##   seed length maximum   751    0
##   seed length minimum   767    0
##   seed shape              0    0
##   seed width maximum    426    0
##   seed width minimum    429    0
```

Finally, check it works in the conext of laoding the metadata:


```r
data <- read_csv(file.path("../data", key, "data.csv"))
metadata <- yaml::yaml.load_file(file.path("../data", key, "metadata.yml"))
data2 <- custom_manipulation(metadata[["config"]][["custom_R_code"]])(data)
all.equal(data, data2)
```

```
## [1] "Component \"units\": 11517 string mismatches"
## [2] "Component \"value\": 11513 string mismatches"
```

Now you're ready to go. 
