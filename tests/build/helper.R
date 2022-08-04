requireNamespace("testthat", quietly = TRUE)




library(austraits.build)
source("../testthat/functions.R")

root.dir <- rprojroot::find_package_root_file()
# source(file.path(root.dir, "config/custom.R"))

schema <- load_schema()
definitions <- load_schema(file.path(root.dir, "config/traits.yml"), "traits")
unit_conversions <- austraits.build:::make_unit_conversion_functions(file.path(root.dir,"config/unit_conversions.csv"))

taxon_list <- read_csv_char(file.path(root.dir,"config/taxon_list.csv"))
