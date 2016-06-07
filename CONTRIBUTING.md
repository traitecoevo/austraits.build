
Contributing to Austraits
====================

## Reporting errors

If you notice a possible error in Austraits, please [post an issue here](https://github.com/traitecoevo/austraits/issues), describing the error and possible fix in detail. If you can, please provide code illustrating the problem.

## Contributing new data

We gladly accept new data contributions to Austraits. In the future we hope to publish a data paper, including contributors as co-authors on the article.

If you would like to contribute data, the requirements are:

1. Data were collected for Australian plant species growing in Australia
2. You collected data on one of the traits list in the [trait definitions table](config/definitions_traits.csv).
3. You are willing to release the data under an open license for reuse by the scientific community.
4. **That you make it is as easy as possible for us to incorporate your data by carefully following the instructions below.**

By far our preferred way of contributing is for you to fork the database in github, add your dataset then send us a [pull request](https://help.github.com/articles/using-pull-requests/). If this is not possible, you could email the relevant files (see above) to Rachael Gallagher.

### Preparing data

To contribute, please follow the steps below. It is important that all steps are followed so that our automated workflow proceeds without problems.

1. Create a new folder with name corresponding to the paper or study of the dataset, e.g. `Gallagher_2014` (do not include `etal` or similar).
2. Prepare the following files:
	* `data.csv`: a table of data in comma-separated values format, with data for each individual plant on a single row
	* `metadata.csv`: description of the metadata.
  * `context.csv`: description of site (contextual) variables.

It may help to download one of the [existing datasets](https://github.com/traitecoevo/austraits/tree/master/data) and use it as a template for your own files and a guide on required content. You should look at the files in the [config folder](https://github.com/traitecoevo/austraits/tree/master/config), in particular the `definitions` files for the list of traits we cover.

### Adding data to Austraits

Once you have prepared your data files, add the relevant folder into the `data` directory. You can then rebuild the dataset, including your dataset.

To do so you will need to rerun the `bootstrap.R` script, which will update the `remake_data.yml` and `remake_reports.yml` files with appropriate rules for the new dataset (similarly if you remove datasets, do the same). (At this stage, [remake](https://github.com/richfitz/remake) offers no looping constructs (on purpose) so for now at least we generate the remakefile using [whisker](https://github.com/edwindj/whisker).)

### Tests

You can also run some automated tests to ensure the dataset meets required setup.

## Other contributions

If you would like to value-add to Austraits in some other way, please get in contact by [posting an issue](https://github.com/traitecoevo/austraits/issues) with an idea or offer of time.
