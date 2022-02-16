### function has been migrated to and modified in steps.R
export_to_plain_text <- function(austraits, path) {
  dir.create(path, FALSE, TRUE)
  for(v in c("traits", "sites", "contexts", "methods", "excluded_data", "taxa", "taxonomic_updates"))
    readr::write_csv(austraits[[v]], sprintf("%s/%s.csv", path, v))
  write_yaml(austraits[["definitions"]],  sprintf("%s/definitions.yml", path))
  RefManageR::WriteBib(austraits$sources, sprintf("%s/sources", path))
}

### Doesnt appear to be migrated to austraits package nor is it used in austraits.build ###
compare_versions <- function (v1, v2, path = "export/tmp", dataset_id=NULL, trait_name = NULL) {
  unlink(path, TRUE, TRUE)
  dir.create(path, FALSE, TRUE)

  v1 <- readRDS(v1)
  v2 <- readRDS(v2)

  if(!is.null(dataset_id)){
    v1 <- v1 %>% austraits::extract_dataset(dataset_id)
    v2 <- v2 %>% austraits::extract_dataset(dataset_id)
  }

  if(!is.null(trait_name)){
    v1 <- v1 %>% austraits::extract_trait(trait_name)
    v2 <- v2 %>% austraits::extract_trait(trait_name)
  }


  v1 %>% export_to_plain_text(path)
  repo <- git2r::init(path)
  git2r::add(repo, "*")
  v2 %>% export_to_plain_text(path)

  message(paste0("Comparison saved in ", path, ". Run ` git -C ", path, " diff --word-diff-regex='[^[:space:],]+' ` in terminal to view differences"))
}