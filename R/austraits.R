
## Functions for extracting bits from austraits

extract_dataset <- function(austraits, dataset_id) {

  ret <- list()
  for(v in c("data", "context"))
    ret[[v]] <- austraits[[v]][austraits[[v]][["dataset_id"]] == dataset_id,]
  # NB: can't use dplyr::filter in the above as it doesn't behave when the variable name is the same as acolumn name

  ret[["species_list"]] <- austraits[["species_list"]] %>% filter(species_name %in% ret[["data"]][["species_name"]])
  ret[["metadata"]] <- austraits[["metadata"]][[dataset_id]]
  ret
}

trait_type  <- function(trait_name, definitions) {
  extract_list_element(trait_name, definitions$traits$values, "type")
}

trait_is_numeric <- function(trait_name, definitions) {
  trait_type(trait_name, definitions) == "numeric"
}

trait_is_categorical <- function(trait_name, definitions) {
  !trait_is_numeric(trait_name, definitions)
}

export_to_plain_text <- function(austraits, path) {
  dir.create(path, FALSE, TRUE)
  for(v in c("data","context","species_list"))
    write_csv(austraits[[v]], sprintf("%s/%s.csv", path, v))
  write_yaml(austraits[["metadata"]],  sprintf("%s/%s.yml", path, v))
}


compare_versions <- function (v1, v2, path = "export/tmp", dataset_id=NULL) {
  unlink(path, TRUE, TRUE)
  dir.create(path, FALSE, TRUE)

  v1 <- readRDS(v1)
  v2 <- readRDS(v2)

  if(!is.null(dataset_id)){
    v1 <- v1 %>% extract_dataset(dataset_id)
    v2 <- v2 %>% extract_dataset(dataset_id)
  }

  v1 %>% export_to_plain_text(path)
  repo <- git2r::init(path)
  git2r::add(repo, "*")
  v2 %>% export_to_plain_text(path)
  # Call git -C export/tmp diff --word-diff-regex="[^[:space:],]+"
#  system2(sprintf("git -C %s diff --word-diff-regex='[^[:space:],]+')", path))
}

#compare_versions("export/austraits-0.rds", "export/austraits.rds", "export/blackman", dataset_id="Leishman_1992")

#compare_versions("export/austraits-0.rds", "export/austraits.rds")
