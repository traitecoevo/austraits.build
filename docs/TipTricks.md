Tips & Tricks for developers of `AusTraits`
================
Daniel Falster
2020-03-11



<!-- TipsTricks.md is generated from TipsTricks.Rmd Please edit that file -->

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

Separate documents provide information on:

  - Overview of Austraits: [Falster et al 2020](XXXX)
  - Accessing and using AusTraits: XXXX
  - Building Austraits: [Building](Building.md)
  - Contributing to AusTraits: [Contributing](Contributing.md)
  - Working with GitHub repository:
    [Working\_with\_github](Working_with_github.md)
  - Tips & Tricks for AusTraits developes: [TipsTricks](TipTricks.md)
  - Full list of trait definitions:
    [Trait\_definitions](Trait_definitions.md)

## Extracting data from PDF tables

If you encounter a PDF table of data and need to extract values, this
can be achieved with the [`tabula-java`
tool](https://github.com/tabulapdf/tabula-java/). There’s actually an R
wrapper (called [`tabulizer`](https://github.com/ropensci/tabulizer)),
but we haven’t succeeded in getting this running. However, it’s easy
enough to run the java tool at the command line on OSX.

1.  [Download latest release of
    `tabula-java`](https://github.com/tabulapdf/tabula-java/releases)
    and save the file in your
    path

2.  Run

<!-- end list -->

    java -jar tabula-1.0.3-jar-with-dependencies.jar my_table.pdf -o my_data.csv

This should output the data from the table in `my_table.pdf` into the
csv `my_data.csv`

3.  Clean up in Excel. check especially for correct locations of white
    spaces.
