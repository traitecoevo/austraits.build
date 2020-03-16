---
title: Tips & Tricks for developers of `AusTraits`
author: Daniel Falster
date: "2020-03-16"
header-includes:
   - \usepackage{pmboxdraw}
output:
  github_document:
    html_preview: false
    toc: yes
    toc_depth: 3
editor_options:
  chunk_output_type: console
---

<!-- TipsTricks.md is generated from TipsTricks.Rmd Please edit that file -->



Separate documents provide information on: 

- Overview of Austraits from [Falster et al 2020](XXXX)
- [Building Austraits](docs/Building.md)
- [Contributing](docs/Contributing.md)
- [Working with our GitHub repository](docs/Working_with_github.md)
- [Tips & Tricks for AusTraits developers](docs/TipTricks.md), and
- [Full list of trait definitions](docs/Trait_definitions.md)


# Working with our Github repository

## Commit messages

Informative commit messages are ideal. Where possible, these should link to 


## Pull requests

To makes changes you'll need to send a pull request (PR) on GitHub. For an intro to PRs [see here](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests). In short, 

1. Create a Git branch for your new work, either within the AusTraits repo (if you are an approved contributor) or as a [fork of the repo](https://help.github.com/en/github/getting-started-with-github/fork-a-repo). 
2. Make commits and push these up onto the branch. 
2. Make sure everything runs fine before you send a pull request.
3. When you're ready to merge in the new features, 

Before you make a substantial pull request, you should always [file an issue](https://github.com/traitecoevo/austraits.build/issues) and make sure someone from the team agrees that it’s worth pursuing. a problem. If you’ve found a bug, create an associated issue and illustrate the bug with a minimal [reprex](https://www.tidyverse.org/help/#reprex) illustrating the issue.


## Merging a pull request

There are multiple ways to merge a pull request, including using GitHub's built-in options for merging and squashing. When merging a PR, we ideally want

- a single commit
- attributing the work to the original author
- to run various checks along the way 

There's two ways to do this. For both you need to be an approved maintainer. 

### Merging in your own PR

You can merge in your own PR after you've had someone else review it. 

1. Send the PR
2. Tag someone to review
3. Once ready, merge into main choosing "Squash & Merge", using an informative commit message.


### Merging someone else's PR

When merging in someone else's PR, the buitlin  options aren't ideal. as they either take all of the commits on a branch (ugh, messy), OR make the commit under the name of the person merging the request.

The workflow below describes how to merge a pull request from the command line, with a single commit & attributing the work to the original author. Lets assume a branch of name `Smith_1995`.

First from the master branch in the repo, run the following:

```
git merge --squash origin/Smith_1995
```

Then in R



Now back in the terminal

```
git add .
git commit
```

Add a commit message, referencing relevant pull requests and issues, e.g.

```
Smith_1995: Import new data

For #224, closes #286
```

And finally, amend the commit author, to reference the person who did all the work!
```
git commit --amend --author "XXX <XXX@gmail.com>"
```

# Making a new release & version updating

Notes on release process

http://r-pkgs.had.co.nz/release.html

## Releasing a new version

Increment version number



# Extracting data from PDF tables

If you encounter a PDF table of data and need to extract values, this can be achieved with the [`tabula-java` tool](https://github.com/tabulapdf/tabula-java/). There's actually an R wrapper (called [`tabulizer`](https://github.com/ropensci/tabulizer)), but we haven't succeeded in getting this running. However, it's easy enough to run the java tool at the command line on OSX.

1. [Download latest release of `tabula-java`](https://github.com/tabulapdf/tabula-java/releases) and save the file in your path

2. Run
```
java -jar tabula-1.0.3-jar-with-dependencies.jar my_table.pdf -o my_data.csv
```
This should output the data from the table in `my_table.pdf` into the csv `my_data.csv`

3. Clean up in Excel. check especially for correct locations of white spaces.

