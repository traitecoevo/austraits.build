# Chinnock_2007 (spreadsheet) vs Chinnock_2007_2 (OCR + parser), traits both hold, processed by traits.build
suppressPackageStartupMessages({library(traits.build); library(dplyr); library(readr); library(tidyr); library(stringr)})
schema <- get_schema(); resource_metadata <- get_schema("config/metadata.yml", "metadata")
definitions <- get_schema("config/traits.yml", "traits"); unit_conversions <- get_unit_conversions("config/unit_conversions.csv")
proc <- function(id) { cfg <- dataset_configure(file.path("data", id, "metadata.yml"), definitions)
  dataset_process(file.path("data", id, "data.csv"), cfg, schema, resource_metadata, unit_conversions)$traits }
a <- proc("Chinnock_2007"); b <- proc("Chinnock_2007_2")
saveRDS(list(a = a, b = b), "ignore/jobs/erem1/compare.rds")
cat("Chinnock_2007:", nrow(a), "rows,", n_distinct(a$taxon_name), "taxa,", n_distinct(a$trait_name), "traits\n")
print(sort(unique(a$trait_name)))
cat("shared traits:", paste(intersect(unique(a$trait_name), unique(b$trait_name)), collapse = ", "), "\n")
cat("taxa in both:", length(intersect(unique(a$taxon_name), unique(b$taxon_name))), "\n")
print(a %>% count(trait_name, value_type), n = 60)
