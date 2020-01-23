


Notes on release process

http://r-pkgs.had.co.nz/release.html

## Releasing a version

Increment version number

```{r}
desc::desc_bump_version("dev")
```

## Merge pull request

This workflow describes how to merge a pull request from the command line, with

- a single commit
- attributing the work to the original author
- run various checks along the way 


First from the master branch in the repo, run the following:

```
git merge --squash origin/Jin_0000
```


Then in R

```{r}
dataset_ids <- "Wills_2018"

# Update remake file
austraits_rebuild_remake_setup()

# check data builds
remake::make(dataset_ids)

# Check taxnonomy has been updated
metadata_check_taxa(dataset_ids)

# run tests on dataset
austraits_run_tests()

# read and write yaml to prevent future reformatting 
f <- file.path("data", dataset_ids, "metadata.yml")
read_yaml(f) %>% write_yaml(f)

# rebuild
remake::make(dataset_ids)
```

Now back in the terminal

```
git add .
git commit
```

Add a commit message, referencing relevant pull requests and issues, e.g.

```
Jin_0000: Import new data

For #224, closes #286
```

And finally, amend the commit author, to reference the person who did all the work!
```
git commit --amend --author "XXX <XXX@gmail.com>"
```
