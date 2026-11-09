suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(stringr); library(purrr)})
x <- readRDS("ignore/jobs/erem1/compare.rds"); a <- x$a; b <- x$b
shared <- intersect(unique(a$trait_name), unique(b$trait_name)); tx <- intersect(unique(a$taxon_name), unique(b$taxon_name))
num <- c("leaf_length", "leaf_width", "plant_height", "seed_length", "seed_width")
# numeric: Chinnock_2007's min / max against Chinnock_2007_2's own (not inferred) min / max (mean when a single value)
an <- a %>% filter(trait_name %in% num, taxon_name %in% tx) %>% mutate(v = as.numeric(value)) %>% group_by(taxon_name, trait_name, value_type) %>% summarise(a = min(v), .groups = "drop")
bn <- b %>% filter(trait_name %in% num, taxon_name %in% tx) %>% mutate(v = as.numeric(value), vt = recode(value_type, mean = "single"))
bmin <- bn %>% group_by(taxon_name, trait_name) %>% summarise(b_min = suppressWarnings(min(v[vt %in% c("minimum", "single")])), b_max = suppressWarnings(max(v[vt %in% c("maximum", "single")])), .groups = "drop")
cmp <- an %>% pivot_wider(names_from = value_type, values_from = a) %>% rename(a_min = minimum, a_max = maximum) %>% left_join(bmin, by = c("taxon_name", "trait_name")) %>%
  mutate(b_min = ifelse(is.finite(b_min), b_min, NA), b_max = ifelse(is.finite(b_max), b_max, NA),
         status = case_when(is.na(b_min) & is.na(b_max) ~ "missing in _2",
                            (is.na(a_min) | a_min == b_min) & (is.na(a_max) | a_max == b_max) ~ "same",
                            TRUE ~ "differs"))
write_csv(cmp, "ignore/jobs/erem1/compare_numeric.csv")
print(cmp %>% count(trait_name, status) %>% pivot_wider(names_from = status, values_from = n, values_fill = 0))
# categorical
cat_tr <- setdiff(shared, num)
ac <- a %>% filter(trait_name %in% cat_tr, taxon_name %in% tx) %>% group_by(taxon_name, trait_name) %>% summarise(a = paste(sort(unique(unlist(str_split(value, " ")))), collapse = " "), .groups = "drop")
bc <- b %>% filter(trait_name %in% cat_tr, taxon_name %in% tx) %>% group_by(taxon_name, trait_name) %>% summarise(b = paste(sort(unique(unlist(str_split(value, " ")))), collapse = " "), .groups = "drop")
cc <- ac %>% left_join(bc, by = c("taxon_name", "trait_name")) %>%
  mutate(status = case_when(is.na(b) ~ "missing in _2", a == b ~ "same",
                            map2_lgl(str_split(a, " "), str_split(coalesce(b, ""), " "), ~ all(.x %in% .y)) ~ "_2 has more",
                            map2_lgl(str_split(a, " "), str_split(coalesce(b, ""), " "), ~ any(.x %in% .y)) ~ "overlap",
                            TRUE ~ "differs"))
write_csv(cc, "ignore/jobs/erem1/compare_categorical.csv")
print(cc %>% count(trait_name, status) %>% pivot_wider(names_from = status, values_from = n, values_fill = 0))
