
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


  v1 %>% export_version_plaintext(path)
  repo <- git2r::init(path)
  git2r::add(repo, "*")
  v2 %>% export_version_plaintext(path)

  message(paste0("Comparison saved in ", path, ". Run ` git -C ", path, " diff --word-diff-regex='[^[:space:],]+' ` in terminal to view differences"))
}
