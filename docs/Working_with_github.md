Working with the `AusTraits` GitHub repo
================
Daniel Falster
2020-03-11



<!-- Working_with_github.md is generated from Working_with_github.Rmd Please edit that file -->

<!-- hack to get indentation on 3rd level of floating TOC in html; see
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

This document outlines our processes for handling various activities in
GitHub.

# Preparing a new dataset

# Commit messages

Informative commit messages are ideal. Where possible, these should link
to

# Pull requests

To makes changes you’ll need to send a pull request (PR) on GitHub. For
an intro to PRs [see
here](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests).
In short,

1.  Create a Git branch for your new work, either within the AusTraits
    repo (if you are an approved contributor) or as a [fork of the
    repo](https://help.github.com/en/github/getting-started-with-github/fork-a-repo).
2.  Make commits and push these up onto the branch.
3.  Make sure everything runs fine before you send a pull request.
4.  When you’re ready to merge in the new features,

Before you make a substantial pull request, you should always [file an
issue](https://github.com/traitecoevo/austraits.build/issues) and make
sure someone from the team agrees that it’s worth pursuing. a problem.
If you’ve found a bug, create an associated issue and illustrate the bug
with a minimal [reprex](https://www.tidyverse.org/help/#reprex)
illustrating the issue.

## Merging a pull request

There are multiple ways to merge a pull request, including using
GitHub’s built-in options for merging and squashing. When merging a
PR, we ideally want

  - a single commit
  - attributing the work to the original author
  - to run various checks along the way

There’s two ways to do this. For both you need to be an approved
maintainer.

### Merging in your own PR

You can merge in your own PR after you’ve had someone else review it.

1.  Send the PR
2.  Tag someone to review
3.  Once ready, merge into main choosing “Squash & Merge”, using an
    informative commit message.

### Merging someone else’s PR

When merging in someone else’s PR, the buitlin options aren’t ideal. as
they either take all of the commits on a branch (ugh, messy), OR make
the commit under the name of the person merging the request.

The workflow below describes how to merge a pull request from the
command line, with a single commit & attributing the work to the
original author. Lets assume a branch of name `Smith_1995`.

First from the master branch in the repo, run the following:

    git merge --squash origin/Smith_1995

Then in R

Now back in the terminal

    git add .
    git commit

Add a commit message, referencing relevant pull requests and issues,
e.g.

    Smith_1995: Import new data
    
    For #224, closes #286

And finally, amend the commit author, to reference the person who did
all the work\!

    git commit --amend --author "XXX <XXX@gmail.com>"

# Making a new release & version updating

Notes on release process

<http://r-pkgs.had.co.nz/release.html>

## Releasing a version

Increment version number

Type: Compendium Package: austraits.build Title: Raw data and code used
to build the AusTraits compilation Version: 0.9.1.9001 <Authors@R>
(parsed): \* Daniel Falster <daniel.falster@unsw.edu.au> \[cre\]
(<https://orcid.org/0000-0002-9814-092X>) \* Rachael Gallagher \[ctb,
cur\] (<https://orcid.org/0000-0002-4680-8115>) \* Elizabeth Wenk \[cur,
ctb\] \* Stuart Allen \[ctb\] \* Sam Andrew \[ctb, cur\] \* Caitlin
Baxter \[cur\] \* James Lawson \[ctb\] \* Ian Wright \[ctb\] Maintainer:
Daniel Falster <daniel.falster@unsw.edu.au> Description: The Austraits
database (AusTraits) is an open-source platform for ongoing compilation
and distribution of data on the traits of Australian plant species. Data
have been assembled from diverse sources, describing more than 100 plant
traits and over 18k species. This repository contains all the
information needed to build the resource from different sources,
i.e. harmonise the different datasets into a common structure, with
common variable names, units and structure. Also included are the
functions we use to help with adding new studies. Note the compiled
database is available from <https://github.com/traitecoevo/austraits>.
License: What license it uses BugReports:
<https://github.com/traitecoevo/austraits.build/issues> Depends: R (\>=
3.6.1), base, crayon, dplyr, purrr, readr, RefManageR, stringr, tidyr,
tibble Imports: desc, git2r, remake, yaml, Taxonstand Suggests: whisker,
testhat, crayon, forcats, ggbeeswarm, googledrive, gridExtra, jurien,
kableExtra, knitr, leaflet, rcrossref, rlang, rmarkdown, rprojroot,
rqdatatable, scales, Taxonstand, whisker, zip Remotes: richfitz/remake
Encoding: UTF-8 LazyData: true

XXXXX
