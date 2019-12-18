load_study <- function(filename_data_raw,
                       filename_metadata,
                       definitions,
                       unit_conversion_functions,
                       taxonomy_known
                       ) {

  dataset_id <- basename(dirname(filename_data_raw))

  # read metadata
  metadata <- read_yaml(filename_metadata)

  # load and clean trait data
  traits <- read_csv(filename_data_raw, col_types = cols(), guess_max = 100000) %>%
    custom_manipulation(metadata[["config"]][["custom_R_code"]])() %>%
    parse_data(dataset_id, metadata) %>%
    add_all_columns(definitions, "traits") %>%
    flag_unsupported_traits(definitions) %>%
    convert_units(definitions, unit_conversion_functions) %>%
    flag_unsupported_values(definitions) %>%
    update_taxonomy(metadata) %>%
    mutate(
      # For cells with multiple values (separated by a space), sort these alphabetically
      value =  ifelse(is.na(error), split_then_sort(value),value),
      value_type = factor(value_type, levels = names(definitions$value_type$values))
      ) %>% 
    arrange(observation_id, trait_name, value_type) 

  # read contextual (site) data
  if(length(unlist(metadata$sites)) > 1){
    # extract contextual data from metadata
    format_sites <- function(v, my_list) {
      my_list[[v]] %>%
      list1_to_df() %>%
      rename(site_property="key") %>%
      mutate(dataset_id=dataset_id, site_name = v)
    }

    sites <- lapply(metadata$sites, lapply, as.character) %>%
      lapply(names(.), format_sites, .) %>%
      dplyr::bind_rows()

  } else {
    sites <- tibble(dataset_id = character(), site_name = character())
  }
  sites <- add_all_columns(sites, definitions, "sites")

  # record contributors
  contributors <- 
    metadata$people %>%
    list_to_df() %>% 
    mutate(dataset_id = dataset_id) %>% 
    select(dataset_id = dataset_id, everything()) %>% 
    filter(!is.na(name))

  # record methods on study from metadata
  source_primary <- convert_list_to_bib(metadata$source$primary)
  source_secondary <- convert_list_to_bib(metadata$source$secondary)
 
  methods <-   
    full_join( by = "dataset_id",
      # methods used to collect each trait  
      metadata[["traits"]] %>%
        list_to_df() %>%
        filter(!is.na(trait_name)) %>% 
        mutate(dataset_id = dataset_id) %>%
        select(dataset_id, trait_name, methods) 
      ,
      # study methods
      metadata$dataset %>% 
        list1_to_df() %>% 
        spread(key, value) %>%
        select(one_of(names(metadata$dataset))) %>% 
          mutate(dataset_id = dataset_id) %>%
          select(-original_file, -notes)
      )  %>%
      full_join( by = "dataset_id",
        #references
        tibble(
          dataset_id = dataset_id,
          source_primary_citation = bib_print(source_primary),
          source_primary_key = source_primary$key,
          source_secondary_citation = ifelse(!is.null(source_secondary), bib_print(source_secondary), NA_character_),
          source_secondary_key = ifelse(!is.null(source_secondary), source_secondary$key, NA_character_)
          )
      )

  # Retrieve taxonomic details
  taxonomy <- 
      left_join(by = "species_name",
                tibble(species_name =  unique(traits$species_name)),
                taxonomy_known
                ) %>%
      arrange(species_name) %>%
      mutate(genus = 
         stringr::str_split(species_name, " " ) %>% map_chr(1)
         ) %>%
      select(species_name, genus, family, everything())

  list(dataset_id = dataset_id,
       traits       = traits %>% filter(is.na(error)) %>% select(-error),
       sites    = sites %>% select(-error),
       methods    = methods,
       excluded_data = traits %>% filter(!is.na(error)) %>% select(error, everything()),
       taxonomy = taxonomy,
       definitions = definitions,
       contributors = contributors,
       sources =  c(source_primary, source_secondary)
       )
}

## Creates a function that applies custom data manipulations as needed
## If the metadata field custom_R_code is not empty, apply code
## specified there. Otherwise we apply the identity function to
## indicate no manipulations will be  done.
## The code custom_R_code assumes a single input -- a  data.frame
## called `data` and returns a data.frame
custom_manipulation <- function(txt) {
  if (!is.null(txt) && !is.na(txt)  && nchar(txt) > 0) {
    function(data) {eval(parse(text=txt), env=new.env())}
  } else {
    identity
  }
}


## Remove any disallowed traits, as defined in definitions
flag_unsupported_traits <- function(data, definitions) {
  
  # create error column if not already present
  if(is.null(data[["error"]]))
    data[["error"]] <- NA_character_

  # exclude traits not in definitions
  i <- data$trait_name %in% names(definitions$traits$elements)
  mutate(data, error = ifelse(!i, "Unsupported trait", error))
}

# checks if values in vector x are in y
# values in x may contain multiple values separated by `sep`
# so first split these
check_all_values_in <- function(x, y, sep=" "){
  x %>% str_split(sep) %>% sapply(function(xi) all(xi %in% y))
}


# formats bibentry according to desired style using RefManageR
# not using print.BibEntry as this message to screen
bib_print <- function(bib, .opts = list(first.inits = TRUE, max.names = 1000, style="markdown") ) {

  # set format
  oldopts <- RefManageR::BibOptions(.opts)
  on.exit(RefManageR::BibOptions(oldopts))
  bib %>% 
    RefManageR:::format.BibEntry(.sort = F) %>%
    # HACK: remove some of formatting introduced in line above
    # would be nicer if we could apply csl style
    gsub("[] ", "", ., fixed = TRUE) %>% 
    gsub("\\n", "", .) %>% 
    gsub("DOI:", " doi: ", ., fixed = TRUE) %>% 
    gsub("URL:", " url: ", ., fixed = TRUE) %>% 
    ifelse(tolower(bib$bibtype) == "article",  gsub("In:", " ", .), .)
}

# convert a list of elements into a valid bibEntry
convert_list_to_bib <- function(ref) {
  if(is.null(ref)) return(NULL)

  if(is.na(ref[1])) return(NULL)


  # Replace , with and to get correct handling of authors
  ref$author <- gsub(",", " and ", ref$author)

  # Ensures capitalisation of title retained as is
  if(!is.null(ref$title))
    ref$title = sprintf("{%s}", ref$title)

  RefManageR::as.BibEntry(ref)
}

convert_bib_to_list <- function(bib) {

  # Read in file, convert to list, set key
    bib <- bib %>% unlist()
  
    if(!is.null(bib$author))
      bib$author <- paste(bib$author, collapse=" and ")
    if(!is.null(bib$editor))
      bib$editor <- paste(bib$editor, collapse=" and ")

    bib
}

## Flag any values outside allowable range
flag_unsupported_values <- function(data, definitions) {

  # NA values
  i <-   is.na(data[["value"]])
  data <- mutate(data, error = ifelse(i, "Missing value", error))
  
  # only check traits not already flagged as errors
  traits <- (filter(data, is.na(error)) %>% pull(trait_name) %>% unique())

  for(trait in traits ) {
   
    # General categorical traits
    if(trait_is_categorical(trait, definitions)) {

      i <-  is.na(data[["error"]]) &
            data[["trait_name"]] == trait &
            !is.null(definitions$traits$elements[[trait]]$values) &
            !check_all_values_in(data$value, names(definitions$traits$elements[[trait]]$values))
      data <- mutate(data, error = ifelse(i, "Unsupported trait value", error))
    }

    # specific tests for flowering, fruiting time
    if(trait %in% c("flowering_time", "fruiting_time") ) {

      ii <- data[["trait_name"]] == trait

      # Contains non-number
      i <-  ii & is.na(data[["error"]]) & grepl("\\D", data[["value"]])
      data <- mutate(data, error = ifelse(i, "Time contains non-number", error))

      # Only 0-1
      i <-  ii & is.na(data[["error"]]) & grepl("[2-9]+", data[["value"]])
      data <- mutate(data, error = ifelse(i, "Time can only contain 0 & 1s", error))

      # Must be length 12
      i <-  ii & is.na(data[["error"]]) & str_length(data[["value"]]) != 12
      data <- mutate(data, error = ifelse(i, "Time must be length 12", error))
    }

    # Numerical traits out of range
    if(trait_is_numeric(trait, definitions) ) {

      x <- suppressWarnings(as.numeric(data[["value"]]))
      i <-  is.na(data[["error"]]) & data[["trait_name"]] == trait & is.na(x)
      data <- mutate(data, error = ifelse(i, "Value does not convert to numeric", error))
 
      i <-  is.na(data[["error"]]) & data[["trait_name"]] == trait &
        (x < definitions$traits$elements[[trait]]$values$minimum | x > definitions$traits$elements[[trait]]$values$maximum)
      data <- mutate(data, error = ifelse(i, "Value out of allowable range", error))
    }
  }

  data
}

make_unit_conversion_functions <- function(filename) {
  x <- read_csv(filename, col_types = cols())

  # make functions from text
  fs <- lapply(x[["function"]], function(x) {
                                  my_f <- function(x) {}
                                  body(my_f) <- parse(text = x)
                                  my_f})
  names(fs) <- unit_conversion_name(x[["unit_from"]], x[["unit_to"]])
  fs
}

unit_conversion_name <- function(from, to) {sprintf("%s-%s", from, to)}

## Convert units to desired type
convert_units <- function(data, definitions, unit_conversion_functions) {

  # List of original variable names
  vars <- names(data)

  # Look up ideal units, determine whether to convert
  data <- data %>%
    mutate(
      i = match(trait_name, names(definitions$traits$elements)),
      to = extract_list_element(i, definitions$traits$elements, "units"),
      ucn = unit_conversion_name(unit, to),
      type = extract_list_element(i, definitions$traits$elements, "type"),
      to_convert =  ifelse(is.na(error), (type == "numeric" & unit != to ), FALSE))

  # Identify anything problematic in conversions and drop
  j <- is.na(data[["to_convert"]]) |
        data[["to_convert"]] & !data[["ucn"]] %in% names(unit_conversion_functions)

  data <- mutate(data, 
            error = ifelse(j, "Missing unit conversion", error),
            to_convert = ifelse(j, FALSE, to_convert))

  f <- function(value, name) {
    as.character(unit_conversion_functions[[name]](as.numeric(value)))
  }

  # Split by unique unit conversions, to allow for as few calls as possible
  data %>%
    group_by(ucn, to_convert) %>%
    mutate(
      value = ifelse(to_convert, f(value, ucn[1]), value),
      unit = ifelse(to_convert, to, unit)) %>%
    ungroup() %>%
    select(one_of(vars))
}

# Add or remove columns of data as needed so that all sets have
# the same columns.
add_all_columns <- function(data, definitions, group) {

  vars <- names(definitions[["austraits"]][["elements"]][[group]][["elements"]])
  missing <- setdiff(vars, names(data))

  for(v in missing)
    data <- mutate(data, !!v := NA)

  data %>%
    select(one_of(vars)) %>%
    mutate(error = NA_character_)
}

# processes a single dataset
parse_data <- function(data, dataset_id, metadata) {

  # get config data for dataset
  data_is_long_format <- metadata[["config"]][["data_is_long_format"]]

  # Step 1. create dataframe with data for vars that we want to keep, and set to correct names
  # all names in "variable_match" must exist in dataset, if not then we need to stop and fix the problem
  var_in <- unlist(metadata[["config"]][["variable_match"]])
  var_out <- names(metadata[["config"]][["variable_match"]])
  if (any(!var_in %in% colnames(data))) {
    stop(paste0("\nVariable '", setdiff(var_in, colnames(data)), "' from data.csv not found in configVarnames"))
  }

  df <- data %>%
        select(one_of(var_in)) %>%
        rename_columns(var_in, var_out) %>% 
        mutate(dataset_id = dataset_id)

  # Add unique observation ids 
  # function builds id -- determine number of 00s needed based on number of records
  make_id <- function(n, dataset_id) 
              sprintf(paste0("%s_%0", ceiling(log10(n)), "d"), 
                              dataset_id, seq_len(n))

  if(!data_is_long_format) {
    # For wide datasets rows are assumed to be natural grouping
    df <- df %>% 
            mutate(observation_id = make_id(nrow(.), dataset_id))
  } else {
    
    # For long datasets, create unique identifier from species_name, site, and observation_id (if specified)
    df[["observation_id_tmp"]] <- gsub(" ", "-", df[["species_name"]])
      
    if(!is.null(df[["site_name"]][1]))
      df[["observation_id_tmp"]] <- paste0(df[["observation_id_tmp"]],"_", df[["site_name"]])

    if(!is.null(df[["observation_id"]])) {
      df[["observation_id_tmp"]] <- paste0(df[["observation_id_tmp"]],"_", df[["observation_id"]])
      df[["observation_id"]] <- NULL
    }

    df <- df %>%
              left_join(by = "observation_id_tmp",
                        tibble(observation_id_tmp = df[["observation_id_tmp"]] %>% unique() %>% sort(), 
                               observation_id = make_id(length(observation_id_tmp), dataset_id))
                        ) %>%
              select(-observation_id_tmp)
  }
  
  # Step 2. Add trait information, with correct names

  cfgChar <-
    metadata[["traits"]] %>%
    list_to_df() %>%
    filter(!is.na(trait_name))  # remove any rows without a matching trait record
   
  # check that the trait names as specified in config actually exist in data
  # if not then we need to stop and fix this problem
  # NOTE - only need to do this step for wide (non-vertical) data
  if (data_is_long_format == FALSE & any(! cfgChar[["var_in"]] %in% colnames(data))) {
    stop(paste(dataset_id, ": missing traits: ", setdiff(cfgChar[["var_in"]], colnames(data))))
  }

  ## if needed, change from wide to long style
  if (data_is_long_format == FALSE) {
    # if the dataset is "wide" then process each variable in turn, to create the "long" dataset -
    # say the original dataset has 20 rows of data and 5 traits, then we will end up with 100 rows
    out <- list()
    for (i in seq_len(nrow(cfgChar))) {
      # create a temporary dataframe which is a copy of df
      # df is our data frame containing all the columns we want EXCEPT for the trait data itself
      out[[i]] <- df
      # to x we append columns of data for trait_name, unit and value (the latter is retrieved from the data)
      out[[i]][["trait_name"]] <- cfgChar[["var_in"]][i]
      out[[i]][["value"]] <- data[[cfgChar[["var_in"]][i]]] %>% as.character()
    }
    out <- dplyr::bind_rows(out)
  } else {
    out <- df
    out[["value"]] <- out[["value"]] %>%  as.character()
  }

  # Ensure all lower case
  out[["value"]] <- tolower(out[["value"]])

  # Add information on trait type, precision, if not already present
  vars <- c("value_type", "replicates")
  i <- match(out[["trait_name"]], cfgChar[["var_in"]])
  if(length(i) >0 ) {
    j <- !is.na(i)
    for(v in vars) {
      out[[v]] <- NA
      out[[v]][j] <- cfgChar[[v]][i[j]]
    }
  }

  # Now process any name changes as per metadata[["traits"]]
  out[["unit"]] <- NA_character_
  i <- match(out[["trait_name"]], cfgChar[["var_in"]])
  if(length(i) >0 ) {
    j <- !is.na(i)
    out[["unit"]][j] <- cfgChar[["unit_in"]][i[j]]
    out[["trait_name"]][j] <- cfgChar[["trait_name"]][i[j]]
  }

  # Implement any value changes as per substitutions
  if(!is.na(metadata[["substitutions"]][1])) {
    cfgLookup <-  list_to_df(metadata[["substitutions"]]) %>%
      mutate(
             find=tolower(find),
             replace=tolower(replace)
             )

    for(i in seq_len(nrow(cfgLookup))) {
      j <- which(out[["trait_name"]] == cfgLookup[["trait_name"]][i] &
                  out[["value"]] == cfgLookup[["find"]][i])
      if( length(j) > 0 ) {
        out[["value"]][j] <- cfgLookup[["replace"]][i]
      }
    }
  }

  out
}


## Enforce some standards on species names
standardise_names <- function(x) {

  f <- function(x, find, replace) {
    gsub(find, replace, x, perl=TRUE)
  }

  x %>%
    ## Capitalise first letter
    f("^([a-z])", "\\U\\1") %>%

    ## sp. not sp or spp
    f("\\ssp(\\s|$)", " sp.\\1") %>%
    f("\\sspp(\\s|$)", " sp.\\1") %>%

    ## subsp. not ssp, ssp., subsp or sub sp.
    f("\\sssp(\\s|$)", " subsp.\\1") %>%
    f("\\sssp.(\\s|$)", " subsp.\\1") %>%
    f("\\ssubsp(\\s|$)", " subsp.\\1") %>%
    f("\\ssub sp.(\\s|$)", " subsp.\\1") %>%

    ## lower case after subsp.
    f("\\ssubsp.\\s([A-Z])", " subsp. \\L\\1") %>%

    ## var. not var
    f("\\svar(\\s|$)", " var.\\1") %>%

    ## aff. not affin, aff, affn
    f("\\saffin(\\s|$)", " aff.\\1") %>%
    f("\\saff(\\s|$)", " aff.\\1") %>%
    f("\\saffn(\\s|$)", " aff.\\1") %>%

    ## remove double space
    f("[\\s]+", " ") %>%

    ## remove " ms" if present
    f(" ms", "")

}

update_taxonomy  <- function(study_data, metadata){

  out <- study_data

  # copy original species name to a new column
  out[["original_name"]] = out[["species_name"]]

  # Now make any replacements specified in metadata yaml
  ## Read metadata table, quit if empty
  cfgLookup <-  list_to_df(metadata[["taxonomic_updates"]])
  if(is.na(cfgLookup) || nrow(cfgLookup) == 0) {
    return(out)
  }

  ## Makes replacements, row by row
  for(i in seq_len(nrow(cfgLookup))) {
    j <- which(out[["species_name"]] == cfgLookup[["find"]][i])
    if( length(j) > 0 )
      out[["species_name"]][j] <- cfgLookup[["replace"]][i]
  }

  out[["species_name"]] <- standardise_names(out[["species_name"]])

  ## Return updated table
  out
}

combine_austraits <- function(..., d=list(...), definitions) {

  combine <- function(name, d) {
    dplyr::bind_rows(lapply(d, "[[", name))
  }

  # combine sources and remove duplicates
  sources <- d %>% lapply("[[", "sources") 
  keys <- sources %>% lapply(names)  %>% unlist() %>% unique() %>% sort()
  sources <- sources %>% reduce(c) %>% .[keys]
  
  # drop null datasets
  d[sapply(d, is.null)] <- NULL

  names(d) <- sapply(d, "[[", "dataset_id")

  # taxonomy 
  taxonomy <- combine("taxonomy", d) %>% 
                arrange(species_name) %>% 
                filter(!duplicated(.))

  # retrieve families from list of known genera - prioritise genera with accepted names
  genera_accepted <- taxonomy %>% 
      filter(!is.na(family) & status == "Accepted") %>%
      select(genus, family) %>% 
      distinct()

  genera_unresolved <- taxonomy %>% 
      filter(!is.na(family) & status == "Unresolved" & !genus %in% genera_accepted$genus) %>%
      select(genus, family) %>% 
      distinct()

  genera_known <- bind_rows(genera_accepted, genera_unresolved)


  genera_known <- 
      rlang::set_names(genera_known$family, genera_known$genus)

  # fill families where unknown
  taxonomy <- taxonomy %>% 
      mutate(
          family = ifelse(
              is.na(family), genera_known[genus], family)
          )

  ret <- list(traits=combine("traits", d),
              sites=combine("sites", d),
              methods=combine("methods", d),
              excluded_data = combine("excluded_data", d),
              taxonomy=taxonomy,
              definitions = definitions,
              contributors=combine("contributors", d),
              sources = sources,
              build_info = list(
                      version=definitions$austraits$elements$version$value,
                      git_SHA=get_SHA_link(),
                      session_info = sessionInfo()
                      )
              )
  ret
}
