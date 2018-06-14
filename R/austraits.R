require(tidyverse)
require(ggplot2)
require(forcats)
require(scales)
require(ggbeeswarm)
require(forcats)

## Functions for extracting bits from austraits

extract_dataset <- function(austraits, dataset_id) {

  ret <- list()
  for(v in c("data", "context", "excluded"))
    ret[[v]] <- austraits[[v]][austraits[[v]][["dataset_id"]] %in% dataset_id,]
  # NB: can't use dplyr::filter in the above as it doesn't behave when the variable name is the same as a column name
  ret[["species_list"]] <- austraits[["species_list"]] %>% filter(species_name %in% ret[["data"]][["species_name"]])
  ret[["metadata"]] <- austraits[["metadata"]][dataset_id]
  ret
}

spread_traits_data <- function(data) {

  vars <- c("value", "unit", "value_type", "replicates", "precision", "methodology_ids")
  ret <- list()
  for(v in vars) {
    ret[[v]] <-
      data %>% rename(to_spread = !!v) %>%
        select(dataset_id, species_name, site_name, observation_id, trait_name, to_spread, original_name) %>%
        spread(trait_name, to_spread)
  }
  ret
}


extract_trait <- function(austraits, trait_name) {

  ret <- austraits
  
  # NB: can't use dplyr::filter in the above as it doesn't behave when the variable name is the same as a column name
  ret[["data"]] <- austraits[["data"]][austraits[["data"]][["trait_name"]] %in% trait_name,]
  ids <- ret[["data"]][["dataset_id"]] %>% unique() %>% sort()
  ret[["context"]] <- austraits[["context"]][austraits[["context"]][["dataset_id"]] %in% ids,]
  ret[["species_list"]] <- austraits[["species_list"]] %>% filter(species_name %in% ret[["data"]][["species_name"]])
  ret[["metadata"]] <- austraits[["metadata"]][ids]
  ret[["excluded"]] <- austraits[["excluded"]][austraits[["excluded"]][["trait_name"]] %in% trait_name,]

  # if nuemric, convert to numeric  
  if(!is.na(ret[["data"]][["unit"]][1])){
    ret[["data"]][["value"]] <- as.numeric(ret[["data"]][["value"]])
  }

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
  for(v in c("data","context","species_list", "excluded"))
    write_csv(austraits[[v]], sprintf("%s/%s.csv", path, v))
  write_yaml(austraits[["metadata"]],  sprintf("%s/metadata.yml", path))
}


compare_versions <- function (v1, v2, path = "export/tmp", dataset_id=NULL, trait_name = NULL) {
  unlink(path, TRUE, TRUE)
  dir.create(path, FALSE, TRUE)

  v1 <- readRDS(v1)
  v2 <- readRDS(v2)

  if(!is.null(dataset_id)){
    v1 <- v1 %>% extract_dataset(dataset_id)
    v2 <- v2 %>% extract_dataset(dataset_id)
  }

  if(!is.null(trait_name)){
    v1 <- v1 %>% extract_trait(trait_name)
    v2 <- v2 %>% extract_trait(trait_name)
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


trait_distribution_by_datasetid <- function(...){
  trait_distribution_plot(y_axis_category = "dataset_id", ...)
}

trait_distribution_by_family <- function(...){
  trait_distribution_plot(y_axis_category = "family", ...)
}


trait_distribution_plot_numerical <- function(austraits, plant_trait_name, y_axis_category, highlight=NA, subset = TRUE){
  
  if(subset)
    austraits <- extract_trait(austraits, plant_trait_name)
  
  data <- extract_trait(austraits, plant_trait_name)$data %>% 
    mutate(log_value = log10(value)) %>%
    left_join(., select(austraits$species, 'species_name', 'family'), by = "species_name")
   
  if(!y_axis_category %in% names(data)){
   stop("incorrect grouping variable")
  }
  
  data$Group =  fct_reorder(data[[y_axis_category]], data$log_value, fun = mean, na.rm=TRUE)

  n <- length(levels(data$Group))
  y.text <- ifelse(n > 20, 0.5, 1)
  p <- 
      ggplot(data, aes(x = value, y = Group, colour = value_type)) +
      geom_quasirandom(groupOnX=FALSE) + 
      theme_bw() +
      labs(title = paste0(plant_trait_name,' - distribution by ', y_axis_category)) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
            axis.text.x=element_text(size=rel(1.5)),
            axis.text.y=element_text(size=rel(y.text))
            )
  vals <- austraits$definitions$traits$values[[plant_trait_name]]$values 
  
  range <- (vals$maximum/vals$minimum)
  
  if(range > 20) {
    p <- p + 
      scale_x_log10( name=paste('Value (', data$unit[1], ')'),#log transformation function
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))
  }
  
  
  if(!is.na(highlight) & highlight %in% data$Group) {
    
    a <- ifelse(levels(data$Group) == highlight, "blue", "black")
    p <- p + 
      theme(axis.text.y = element_text(colour = a))
  }
  
  p
}



trait_distribution_plot_categorical <- function(austraits, plant_trait_name, y_axis_category, highlight=NA, subset = TRUE) {
  
  if(subset)
    austraits <- extract_trait(austraits, plant_trait_name)
  
  data <- extract_trait(austraits, plant_trait_name)$data %>% 
    left_join(., select(austraits$species, 'species_name', 'family'), by = "species_name")
  
  if(!y_axis_category %in% names(data)){
    stop("incorrect grouping variable")
  }
  
  data$Group = data[[y_axis_category]] %>% as.factor()
  
  df <- data %>% group_by(Group, value) %>% summarise(n=n())
  
  n <- length(levels(data$Group))
  y.text <- ifelse(n > 20, 0.5, 1)
  p <- 
    ggplot(df, aes(x=Group, y=n, fill = value)) +
    geom_bar(position = "stack", stat="identity") +
    coord_flip() + 
    theme_bw() +
    labs(title = paste0(plant_trait_name,' - distribution by ', y_axis_category)) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
          axis.text.x=element_text(size=rel(1.5)),
          axis.text.y=element_text(size=rel(y.text))
    )
  
  if(!is.na(highlight) & highlight %in% data$Group) {
    
    a <- ifelse(levels(data$Group) == highlight, "blue", "black")
    p <- p + 
      theme(axis.text.y = element_text(colour = a))
  }
  
  p
}