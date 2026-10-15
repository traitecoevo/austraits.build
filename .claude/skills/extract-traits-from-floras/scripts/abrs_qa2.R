suppressPackageStartupMessages({library(dplyr);library(stringr)})
S <- Sys.getenv("S"); q <- readRDS(file.path(S,"qa.rds")); r <- q$recs
options(width=250)
u <- q$universal %>% mutate(desc=str_trunc(desc,90)); print(as.data.frame(u %>% select(group_name, trait, value, desc, members_reporting)), right=F)
cat("\nREJECTED\n"); print(as.data.frame(q$rejected %>% mutate(desc=str_trunc(desc,60)) %>% select(group_name, trait, value, desc, members_reporting, members_agree)), right=F)
cat("\nOUTLIERS\n")
r %>% filter(kind %in% "num", (trait=="flower_anther_length" & max>20) | (trait=="flower_style_length" & max>50) | (trait=="tepal_length"&max>50) | (trait=="petiole_length" & coalesce(min,max)==0) | trait=="stem_length" | (trait=="plant_height" & max>40) | (trait=="leaf_length" & max>3000)| (trait=="seed_length" & max>40)) %>% select(taxon_name, trait, min, max, desc) %>% as.data.frame() %>% print(right=F)
