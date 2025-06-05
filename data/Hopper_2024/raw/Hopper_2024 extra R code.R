read_csv("data/Hopper_2024/raw/Hopper et al TPB Flora Appendix 1.csv") -> Appendix1

Appendix1_edited <- Appendix1 %>%
  mutate(
    Jan_flower = ifelse(str_detect(str_to_lower(Jan), "x"),"Y","N"),
    Feb_flower = ifelse(str_detect(str_to_lower(Feb), "x"),"Y","N"),
    Mar_flower = ifelse(str_detect(str_to_lower(Mar), "x"),"Y","N"),
    Apr_flower = ifelse(str_detect(str_to_lower(Apr), "x"),"Y","N"),
    May_flower = ifelse(str_detect(str_to_lower(May), "x"),"Y","N"),
    Jun_flower = ifelse(str_detect(str_to_lower(Jun), "x"),"Y","N"),
    Jul_flower = ifelse(str_detect(str_to_lower(Jul), "x"),"Y","N"),
    Aug_flower = ifelse(str_detect(str_to_lower(Aug), "x"),"Y","N"),
    Sep_flower = ifelse(str_detect(str_to_lower(Sep), "x"),"Y","N"),
    Oct_flower = ifelse(str_detect(str_to_lower(Oct), "x"),"Y","N"),
    Nov_flower = ifelse(str_detect(str_to_lower(Nov), "x"),"Y","N"),
    Dec_flower = ifelse(str_detect(str_to_lower(Dec), "x"),"Y","N"),
    across(c(Jan_flower, Feb_flower, Mar_flower, Apr_flower, May_flower, Jun_flower, Jul_flower, Aug_flower, Sep_flower, Oct_flower, Nov_flower, Dec_flower), ~ifelse(is.na(.x), "N", .x)),
    flowering_time = paste0(Jan_flower, Feb_flower, Mar_flower, Apr_flower, May_flower, Jun_flower, Jul_flower, Aug_flower, Sep_flower, Oct_flower, Nov_flower, Dec_flower),
    flowering_time = ifelse(flowering_time == "NNNNNNNNNNNN", NA, flowering_time),Jan_fruit = ifelse(str_detect(str_to_lower(Jan), "x"),"Y","N"),
    Feb_fruit = ifelse(str_detect(str_to_lower(Feb), "x"),"Y","N"),
    Mar_fruit = ifelse(str_detect(str_to_lower(Mar), "x"),"Y","N"),
    Apr_fruit = ifelse(str_detect(str_to_lower(Apr), "x"),"Y","N"),
    May_fruit = ifelse(str_detect(str_to_lower(May), "x"),"Y","N"),
    Jun_fruit = ifelse(str_detect(str_to_lower(Jun), "x"),"Y","N"),
    Jul_fruit = ifelse(str_detect(str_to_lower(Jul), "x"),"Y","N"),
    Aug_fruit = ifelse(str_detect(str_to_lower(Aug), "x"),"Y","N"),
    Sep_fruit = ifelse(str_detect(str_to_lower(Sep), "x"),"Y","N"),
    Oct_fruit = ifelse(str_detect(str_to_lower(Oct), "x"),"Y","N"),
    Nov_fruit = ifelse(str_detect(str_to_lower(Nov), "x"),"Y","N"),
    Dec_fruit = ifelse(str_detect(str_to_lower(Dec), "x"),"Y","N"),
    across(c(Jan_fruit, Feb_fruit, Mar_fruit, Apr_fruit, May_fruit, Jun_fruit, Jul_fruit, Aug_fruit, Sep_fruit, Oct_fruit, Nov_fruit, Dec_fruit), ~ifelse(is.na(.x), "N", .x)),
    fruiting_time = paste0(Jan_fruit, Feb_fruit, Mar_fruit, Apr_fruit, May_fruit, Jun_fruit, Jul_fruit, Aug_fruit, Sep_fruit, Oct_fruit, Nov_fruit, Dec_fruit),
    fruiting_time = ifelse(fruiting_time == "NNNNNNNNNNNN", NA, fruiting_time),
    Geographic_range_relative_to_TPBNR = case_when(
      `Geographic range west relative to TPBNR` == "western" ~ "west_of",
      `Geographic range centered relative to TPBNR` == "centered" ~ "centered_on",
      `Geographic range east relative to TPBNR` == "eastern" ~ "east_of"
    ),
    species = str_replace(species, "^\\*[ ]","")
  ) %>%
select(-contains(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Geographic range "))) 

APCalign::align_taxa(unique(Appendix1_edited$species), resources = resources) %>% 
  select(species = original_name, taxon_name = aligned_name) %>% 
  write_csv("data/Hopper_2024/raw/Appendix1_aligned_names.csv")

Appendix1_aligned <- read_csv("data/Hopper_2024/raw/Appendix1_aligned_names.csv")

Appendix1_edited <- Appendix1_edited %>% left_join(Appendix1_aligned)

read_csv("data/Hopper_2024/raw/Hopper et al TPB Granite flora Appendix 2.csv") -> Appendix2

Appendix2_edited <- Appendix2 %>%
  mutate(
    plant_growth_substrate = case_when(
      Te == "te" ~ "terrestrial",
      Aq == "aq" ~ "aquatic",
      .default = NA
    ),
    life_history = case_when(
      Pn == "pn" ~ "perennial",
      An == "an" ~ "annual",
      .default = NA
    ),
    woodiness = case_when(
      Wo == "wo" ~ "woody",
      He == "he" ~ "herbaceous",
      .default = NA
    ),
    plant_growth_form = case_when(
      Ge == "ge" ~ "geophyte",
      Fe == "fe" ~ "fern",
      Vi == "vi" ~ "climber"
    )
  ) %>%
  select(species = `Plant taxa`, fire_response = `fire response r/s`, plant_growth_substrate, life_history, woodiness, plant_growth_form) %>%
  mutate(species = str_replace(species, "^\\*[ ]",""))

APCalign::align_taxa(unique(Appendix2_edited$species), resources = resources) %>% 
  select(species = original_name, taxon_name = aligned_name) %>% 
  write_csv("data/Hopper_2024/raw/Appendix2_aligned_names.csv")

Appendix2_aligned <- read_csv("data/Hopper_2024/raw/Appendix2_aligned_names.csv")

Appendix2_edited <- Appendix2_edited %>% left_join(Appendix2_aligned)

Appendix1_edited %>% full_join(Appendix2_edited %>% rename(species2 = species)) %>% write_csv("data/Hopper_2024/data.csv", na = "")
