requireNamespace("testthat", quietly = TRUE)




library(traits.build)
source("../testthat/functions.R")

root.dir <- rprojroot::find_package_root_file()
source(file.path(root.dir, "R/custom_R_code.R"))

schema <- get_schema()
resource_metadata <- get_schema(file.path(root.dir, "config/metadata.yml"), "metadata")
definitions <- get_schema(file.path(root.dir, "config/traits.yml"), "traits")
unit_conversions <- traits.build:::get_unit_conversions(file.path(root.dir,"config/unit_conversions.csv"))

taxon_list <- read_csv_char(file.path(root.dir,"config/taxon_list.csv"))
