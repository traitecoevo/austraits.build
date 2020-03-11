## Functions for extracting bits from Austraits

extract_dataset <- function(austraits, dataset_id) {

  ret <- list()
  for(v in c("traits", "sites", "methods", "excluded_data"))
    ret[[v]] <- austraits[[v]][ austraits[[v]][["dataset_id"]] %in% dataset_id,]
  # NB: can't use dplyr::filter in the above as it doesn't behave when the variable name is the same as a column name
  ret[["taxonomy"]] <- austraits[["taxonomy"]] %>% filter(species_name %in% ret[["traits"]][["species_name"]])
  ret[["definitions"]] <- austraits[["definitions"]]

  ret
}

spread_trait_data <- function(data) {

  vars <- c("value", "unit", "value_type", "replicates")
  ret <- list()
  for(v in vars) {
    ret[[v]] <- data %>% 
        rename(to_spread = !!v) %>%
        select(dataset_id, species_name, site_name, observation_id, trait_name, to_spread, original_name) %>%
        spread(trait_name, to_spread)
  }

  ret
}

gather_trait_data <- function(data, definitions) {

  id_variables <- c("dataset_id", "species_name", "site_name", "observation_id", "trait_name", "value", "unit", "value_type", "replicates", "original_name")
  
  traits <- names(data$value)[!(names(data$value) %in% id_variables)]
  
  vars <- names(data)

  gather_f <- function(df, v) {
    df[[v]] %>% gather(one_of(traits), key = "trait_name", value = !!v)
  }

  ret <- gather_f(data, vars[1])

  for(v in vars[-c(1)])
    ret <- ret %>% 
              left_join(
                        gather_f(data, v), 
                        by = setdiff(id_variables, vars)
                        )

  ret <- ret %>% 
    mutate(value = clean_NA(value)) %>%
    filter(!is.na(value)) %>%
    arrange(observation_id, trait_name) %>%
    select(id_variables)
}

# ensure NA appears as a real NA and not character
clean_NA <- function(x) ifelse(x == "NA", NA_character_, x)
  
bind_trait_values <- function(data) {

  bind_x <- function(x) paste0(x, collapse = "--")

  bind_values_worker <- function(.data) {
    # If more than one value per group need to combine
    if(nrow(.data) > 1) {
      return(
            .data %>% 
              mutate(
                 value = bind_x(value),
                 value_type = bind_x(value_type),
                 replicates = bind_x(replicates)
                 ) %>%
              filter(row_number()==1) 
            )
    }
    .data
  }

  data  %>% 
    group_by(observation_id, trait_name) %>% 
    bind_values_worker() %>% 
    ungroup() %>% 
    arrange(observation_id, trait_name, value_type)
}

separate_trait_values <- function(data, definitions) {

  separate_x <- function(x) strsplit(x, "--")[[1]]

  separate_values_worker <- function(df) {

    df[rep(1, df$n_vals[1]),] %>%
      mutate(
           value = separate_x(value[1]),
           value_type = separate_x(value_type[1]),
           replicates = separate_x(replicates[1])
           )
  }


  # record the number of values in each row of data
  data$n_vals <- 1 + str_count(data$value_type, "--")

  # separate out those rows requiring no modification
  out_1 <- data %>% 
    filter(n_vals == 1)

  # separate out those rows requiring modification & modify
  out_2 <- data %>% 
    filter(n_vals > 1) %>% 
    split(paste(.$observation_id, .$trait_name)) %>%    
    lapply(separate_values_worker) %>% 
    bind_rows()

  # join it all back together, clean up and sort as in original
  bind_rows(out_1, out_2) %>% 
    select(-n_vals) %>% 
    mutate(replicates = clean_NA(replicates),
           value_type = factor(
                          clean_NA(value_type), 
                          levels = names(definitions$value_type$values))
           ) %>% 
    arrange(observation_id, trait_name, value_type)
}

extract_trait <- function(austraits, trait_name) {

  ret <- austraits

  # NB: can't use dplyr::filter in the above as it doesn't behave when the variable name is the same as a column name
  ret[["traits"]] <- austraits[["traits"]][ austraits[["traits"]][["trait_name"]] %in% trait_name,]
  ids <- ret[["traits"]][["dataset_id"]] %>% unique() %>% sort()
  ret[["sites"]] <- austraits[["sites"]][austraits[["sites"]][["dataset_id"]] %in% ids,]
  ret[["taxonomy"]] <- austraits[["taxonomy"]] %>% filter(species_name %in% ret[["traits"]][["species_name"]])
  ret[["excluded_data"]] <- austraits[["excluded_data"]][austraits[["excluded_data"]][["trait_name"]] %in% trait_name,]

  # if numeric, convert to numeric
  if(!is.na(ret[["traits"]][["unit"]][1])){
    ret[["traits"]][["value"]] <- as.numeric(ret[["traits"]][["value"]])
  }

  ret[["methods"]] <- austraits[["methods"]] %>% filter(dataset_id %in%  ret[["traits"]][["dataset_id"]] )

  keys <- union(ret$methods$source_primary_key, 
                ret$methods$source_secondary_key) %>% 
          unique() %>% na.omit() %>% as.character()
                
  ret[["sources"]] <- austraits$sources[keys]

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


## move suspected duplicates from the `traits` frame of austraits
## to excluded_data. 

remove_suspected_duplicates <- function(austraits, 
                                        priority_sources = NULL
                                        ) {
  
  # copy traits and create a new variable with year of dataset_id
  # we will preference studies with a lower value
  if(is.null(priority_sources))
    priority_sources <- 
      c(
      "Kew_2019_1", "Kew_2019_2", "Kew_2019_3", "Kew_2019_4", "Kew_2019_5", "Kew_2019_6",
      "ANBG_2019", "GrassBase_2014", "CPBR_2002", "NTH_2014","RBGK_2014", 
      "NHNSW_2016", "RBGSYD_0000", "RBGSYD_2014", "TMAG_2009", "WAH_1998", "WAH_2016",
      "Brock_1993", "Barlow_1981", "Hyland_2003"    
      )
  
  tmp <- 
    austraits$traits %>% 
    # Extract year from dataset_id, so that we can keep the older record
    mutate(
      priority_source = (dataset_id %in% priority_sources),
      year =  str_split(dataset_id, "_") %>% 
                    lapply(function(i) i[2]) %>% unlist() %>% 
                    gsub("0000", "9999", .)
          ) %>%
  # sort to align suspected duplicates
  arrange(trait_name, species_name, value, desc(priority_source), year) %>%
  # detect duplicates based on combination of variables
  mutate(
         to_check = paste(trait_name, species_name, value), 
         is_duplicate = duplicated(to_check)
         )

  # update `traits` with unique values only
  austraits$traits <- tmp %>% 
    filter(!is_duplicate) %>% 
    # remove temporary variables
    select(-year, -to_check, -is_duplicate) %>%
    # original sorting
    arrange(observation_id, trait_name, value_type)

  # add suspected duplicates to bottom of excluded_data, noting 
  # observartion_id of the matching data in the error column
  austraits$excluded_data <- austraits$excluded_data %>% 
    bind_rows(
      tmp %>% 
        filter(is_duplicate) %>% 
        mutate(error = paste("suspected duplicate with ", 
                             # find observatio_id for matching variable
                             tmp$observation_id[match(to_check, tmp$to_check)])
              ) %>% 
        # remove temporary variables
        select(-year, -to_check, -is_duplicate)
      ) %>%
    # original sorting
    arrange(observation_id, trait_name, value_type)

  austraits
}

export_to_plain_text <- function(austraits, path) {
  dir.create(path, FALSE, TRUE)
  for(v in c("traits","sites", "methods", "excluded_data", "taxonomy"))
    write_csv(austraits[[v]], sprintf("%s/%s.csv", path, v))
  write_yaml(austraits[["definitions"]],  sprintf("%s/definitions.yml", path))
  RefManageR::WriteBib(austraits$sources, sprintf("%s/sources", path))
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

  message(paste0("Comparison saved in ", path, ". Run ` git -C ", path, " diff --word-diff-regex='[^[:space:],]+' ` in terminal to view differences"))
}

#compare_versions("export/austraits-0.rds", "export/austraits.rds", "export/blackman", dataset_id="Leishman_1992")

#compare_versions("export/austraits-0.rds", "export/austraits.rds")



compare_versions_df <- function (df1, df2, path = "export/tmp") {
  unlink(path, TRUE, TRUE)
  dir.create(path, FALSE, TRUE)


  df1 %>% write_csv(sprintf("%s/df.csv", path))
 
  repo <- git2r::init(path)
  git2r::add(repo, "*")

  df2 %>% write_csv(sprintf("%s/df.csv", path))

  message(paste0("Comparison saved in ", path, ". Run ` git -C ", path, " diff --word-diff-regex='[^[:space:],]+' ` in terminal to view differences"))
}

trait_distribution_plot_numerical <- function(austraits, plant_trait_name, y_axis_category, highlight=NA, hide_ids = FALSE) {

  # plant_trait_name <- "plant_height"
  # y_axis_category <- "dataset_id"
  # highlight= "Blackman_2014"
  
  # Subset data to this trait
  austraits_trait <- extract_trait(austraits, plant_trait_name)

  my_shapes = c("_min" = 60, "_mean" = 16, "_max" =62, "unknown" = 18)
  
  as_shape <- function(value_type) {
    p <- rep("unknown", length(value_type))
    
    p[grepl("mean", value_type)] <- "_mean" #16
    p[grepl("min", value_type)] <- "_min" #60
    p[grepl("max", value_type)] <- "_max" #62
    factor(p, levels=names(my_shapes))
  }
  
  data <- austraits_trait$traits %>%
    mutate(log_value = log10(value),
           shapes = as_shape(value_type)) %>%
    left_join(., select(austraits_trait$taxonomy, 'species_name', 'family'),
              by = "species_name")

  # Define grouping variables and derivatives
  if(!y_axis_category %in% names(data)){
   stop("incorrect grouping variable")
  }

  # define grouping variable, ordered by group-level by mean values
  data$Group = forcats::fct_reorder(data[[y_axis_category]], data$log_value, na.rm=TRUE)

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
      ggplot(data, aes(x = value, y = Group, colour = colour, shape = shapes)) +
      ggbeeswarm::geom_quasirandom(groupOnX=FALSE) +
      ylab(paste("By ", y_axis_category)) +
      # inclusion of custom shapes: for min, mean, unknown
      # NB: this single line of code makes function about 4-5 slower for some reason
      scale_shape_manual(values = my_shapes) +
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
  if(vals$minimum !=0 & range > 20) {
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
