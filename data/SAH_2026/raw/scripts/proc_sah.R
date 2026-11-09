# Process SAH_2014 / 2022 / 2023 / 2026 with traits.build and save each traits table for the comparison (ignore/jobs/sa2, 2026-11-06).
suppressPackageStartupMessages({library(traits.build); library(dplyr)})
schema <- get_schema(); resource_metadata <- get_schema("config/metadata.yml", "metadata")
definitions <- get_schema("config/traits.yml", "traits"); unit_conversions <- get_unit_conversions("config/unit_conversions.csv")
for (id in c("SAH_2014", "SAH_2022", "SAH_2023", "SAH_2026")) {
  cfg <- dataset_configure(file.path("data", id, "metadata.yml"), definitions)
  x <- dataset_process(file.path("data", id, "data.csv"), cfg, schema, resource_metadata, unit_conversions)
  cat(id, nrow(x$traits), "rows", n_distinct(x$traits$taxon_name), "taxa", n_distinct(x$traits$trait_name), "traits\n")
  saveRDS(x$traits, file.path("ignore/jobs/sa2", paste0("traits_", id, ".rds")))
}
