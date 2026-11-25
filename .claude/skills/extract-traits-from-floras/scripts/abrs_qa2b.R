suppressPackageStartupMessages({library(dplyr);library(stringr)})
S <- Sys.getenv("S"); q <- readRDS(file.path(S,"qa.rds")); r <- q$recs
options(width=250)
u <- q$universal %>% mutate(desc=str_trunc(desc,60)); print(as.data.frame(u %>% select(group_name, trait, value, desc, members_reporting)), right=F)
cat("\nCOPIED counts\n"); print(as.data.frame(q$copied %>% mutate(g=word(desc,1,2)) %>% count(trait, value, scoring, sort=TRUE)), right=F)
