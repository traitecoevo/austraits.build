library(tidyverse)

## Functions for extracting bits from Austraits

extract_dataset <- function(austraits, dataset_id) {

  ret <- list()
  for(v in c("data", "context", "details", "excluded_data"))
    ret[[v]] <- austraits[[v]][ austraits[[v]][["dataset_id"]] %in% dataset_id,]
  # NB: can't use dplyr::filter in the above as it doesn't behave when the variable name is the same as a column name
  ret[["species_list"]] <- austraits[["species_list"]] %>% filter(species_name %in% ret[["data"]][["species_name"]])
  ret[["metadata"]] <- austraits[["metadata"]][dataset_id]
  ret
}

spread_traits_data <- function(data) {

  vars <- c("value", "unit", "value_type", "replicates")
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

  # if numeric, convert to numeric
  if(!is.na(ret[["data"]][["unit"]][1])){
    ret[["data"]][["value"]] <- as.numeric(ret[["data"]][["value"]])
  }

  ret
}

trait_type  <- function(trait_name, definitions) {
  extract_list_element(trait_name, definitions$traits$elements, "type")
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


trait_distribution_plot_numerical <- function(austraits, plant_trait_name, y_axis_category, highlight=NA, hide_ids = FALSE) {

  # plant_trait_name <- "plant_height"
  # y_axis_category <- "dataset_id"
  # highlight= "Blackman_2014"
  
  # Subset data to this trait
  austraits_trait <- extract_trait(austraits, plant_trait_name)

  data <- austraits_trait$data %>%
    mutate(log_value = log10(value)) %>%
    left_join(., select(austraits_trait$species, 'species_name', 'family'),
              by = "species_name")

  # Define grouping variables and derivatives
  if(!y_axis_category %in% names(data)){
   stop("incorrect grouping variable")
  }

  # define grouping variable, ordered by group-level by mean values
  data$Group = forcats::fct_reorder(data[[y_axis_category]], data$log_value,
                           fun = mean, na.rm=TRUE)

  n_group <- levels(data$Group) %>% length()

  # set colour to be alternating
  data$colour = ifelse(data$Group %in% levels(data$Group)[seq(1, n_group, by=2)],
                       "a", "b")


  # set colour of group to highlight
  if(!is.na(highlight) & highlight %in% data$Group) {
    data <- mutate(data, colour = ifelse(Group %in% highlight, "c", colour))
  }

  # Check range on x-axis
  vals <- austraits_trait$definitions$traits$elements[[plant_trait_name]]$values
  range <- (vals$maximum/vals$minimum)

  # Check range on y-axis
  y.text <- ifelse(n_group > 20, 0.75, 1)
  heights = c(1, max(1, n_group/7))

  # Top plot - plain histogram of data
  p1 <-
    ggplot(data, aes(x=value)) +
    geom_histogram(aes(y = ..density..), color="darkgrey", fill="darkgrey", bins=50) +
    geom_density(color="black") +
    xlab("") + ylab("All data") +
    theme_bw()  +
    theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_blank(),
        panel.background = element_blank()
        )
  # Second plot -- dots by groups, using ggbeeswarm package
  p2 <-
      ggplot(data, aes(x = value, y = Group, colour = colour)) +
      ggbeeswarm::geom_quasirandom(groupOnX=FALSE) +
      ylab(paste("By ", y_axis_category)) +
      theme_bw() +
      theme(legend.position = "bottom",
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x=element_text(size=rel(1.25)),
            axis.text.y=element_text(size=rel(y.text))
            ) +
      guides(colour=FALSE)
  
  if(hide_ids) {
    p2 <- p2 + theme(axis.text.y = element_blank())
  }

  # Define scale on x-axis and transform to log if required
  if(range > 20) {
    #log transformation
    p1 <- p1 +
      scale_x_log10( name="",
                     breaks = scales::trans_breaks("log10", function(x) 10^x),
                     labels = scales::trans_format("log10", scales::math_format(10^.x)),
                     limits=c(vals$minimum, vals$maximum))
    p2 <- p2 +
      scale_x_log10(name=paste(plant_trait_name, ' (', data$unit[1], ')'),
                     breaks = scales::trans_breaks("log10", function(x) 10^x),
                     labels = scales::trans_format("log10", scales::math_format(10^.x)),
                     limits=c(vals$minimum, vals$maximum))
  } else {
    p1 <- p1 + scale_x_continuous(limits=c(vals$minimum, vals$maximum))
    p2 <- p2 + scale_x_continuous(limits=c(vals$minimum, vals$maximum)) +
          xlab(paste(plant_trait_name, ' (', data$unit[1], ')'))

  }

  # combine plots
  f <- function(x) {suppressWarnings(ggplot_gtable(ggplot_build(x)))}
  p1 <- f(p1)
  p2 <- f(p2)
  # Fix width of second plot to be same as bottom using ggplot_table
  p1$widths[2:3] <- p2$widths[2:3]
  gridExtra::grid.arrange(p1, p2, nrow=2, widths=c(1), heights=heights)
}
