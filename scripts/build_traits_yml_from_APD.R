
# XXX need to change triple quotes (''') to single quotes ('). During read/write, '{count}/{count}' becomes '''{count}/{count}'''
# XXX flowering_time, fruiting_time, foliage_time and recruitment_time are categorical traits but without any set categorical trait values. 
# XXX       Therefore an empty value for allowed_values_levels is created that breaks AusTraits

library(purrr)
library(dplyr)
library(readr)
library(stringr)
library(traits.build)


path_APD <- "https://raw.githubusercontent.com/traitecoevo/APD/master"
path_APD2 <- "https://raw.githubusercontent.com/traitecoevo/APD/master/data"

trait_groups <- readr::read_csv(file.path(path_APD2, "APD_trait_hierarchy.csv"), show_col_types = FALSE) %>%
  dplyr::select(trait_groupings = label, hierarchy) %>%
  dplyr::filter(!is.na(hierarchy)) %>%
  dplyr::mutate(trait_group = stringr::str_replace_all(hierarchy, " \\<", ";")) %>%
  dplyr::select(-hierarchy)

APD <- 
  read_csv(file.path(path_APD, "APD_traits.csv"), show_col_types = FALSE) %>%
  mutate(
    description = ifelse(is.na(description), description_encoded, paste0(description_encoded, ";", description)),
    units = ifelse(str_starts(units, fixed("{")), sprintf("'%s'", units),units),
    Entity_URI = paste0("https://w3id.org/APD/traits/", identifier),
    trait_type = ifelse(grepl("continuous variable", trait_type), "numeric", "categorical"),
    allowed_values_levels = ifelse(trait_type == "categorical", "add_in", NA),
    across(c(structure_measured, characteristic_measured, keywords, trait_groupings),
           ~ stringr::str_replace_all(.x, " \\[[:graph:]+\\]", ""))
  ) %>%
  dplyr::left_join(trait_groups, by = "trait_groupings") %>%
  select(trait,
         label,
         description,
         comments,
         type = trait_type,
         units,
         allowed_values_min,
         allowed_values_max,
         allowed_values_levels,
         structure_measured,
         keywords,
         trait_group,
         characteristic_measured,
         entity_URI = Entity
  ) %>%
  arrange(entity_URI)

value_levels <- read_csv(file.path(path_APD,"APD_categorical_values.csv"), show_col_types = FALSE) %>%
  mutate(
    description = ifelse(is.na(categorical_trait_synonyms),
                         categorical_trait_description,
                         paste0(categorical_trait_description, " (Synonyms, ", categorical_trait_synonyms, ")"))
  ) %>%
  select(trait_cat = trait,
         label = allowed_values_levels,
         description
  )

# why do we start with traits.yml? Doesn't work if new traits without entity_URI, since that comes from APD first
traits <- get_schema("config/traits.yml", I("traits"))



vars <- c("entity_URI", "label", "description", "comments", "type", "units", "allowed_values_min", "allowed_values_max", "allowed_values_levels", "structure_measured", "keywords", "trait_group", "characteristic_measured")

traits <- list()
traits$description <- "Names and details of plant traits included in the AusTraits compilation"
traits$type <- "list"

traits$elements <-  
  APD %>%
  mutate(units = str_replace_all(units, "'", "")) %>%
  split(APD$entity_URI) %>% # turn into list, 1 element for each row
  # reprocess rows into a list
  map(~ .x %>% 
        # variables in specified order
        select(all_of(vars)) %>%
        # remove NAs
        select(where(~ !is.na(.x))) %>%
        # as a list
        as.list()
  )

# rename with trait name
all.equal(names(traits$elements), APD$entity_URI)
names(traits$elements) <- APD$trait


# for categorical traits, add allowable values
for(trait in names(traits$elements)) {
  if(traits$elements[[trait]][["type"]] == "categorical") {  
    
    allowed_values_levels<-      
      value_levels %>% 
      filter(trait_cat == trait) %>% 
      select(label, description) %>%
      arrange(label) %>%
      pivot_wider(names_from = label, values_from = description)
    
    if(nrow(allowed_values_levels) > 0) {      
      traits$elements[[trait]][["allowed_values_levels"]] <- 
        allowed_values_levels %>% as.list()
    }
  }
}

# This code is not being used, because it makes the traits.yml file too long
# Retaining code for possible use when traits.yml is read into 
# the AusTraits Data Portal and we want to be able to separate the information
# in these fields into the individual values.  
#
# for(trait in names(traits$elements)) {
#  for(v in c("keywords", "structure_measured", "characteristic_measured", "trait_group"))
#    traits$elements[[trait]][[v]] <- traits$elements[[trait]][[v]] 
#    %>% str_split(";") %>% map(~ str_trim(.x)) %>%  unlist()
#}

# Delete allowed_values_levels for items where no values are available or suitable
for(v in c("recruitment_time", "fruiting_time", "flowering_time", "foliage_time")) {
  traits$elements[[v]]$allowed_values_levels <- NULL
}

yaml::write_yaml(list(traits = traits), "config/traits.yml")
