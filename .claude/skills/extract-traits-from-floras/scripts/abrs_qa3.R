suppressPackageStartupMessages({library(dplyr);library(stringr)})
S <- Sys.getenv("S"); q <- readRDS(file.path(S,"qa.rds")); r <- q$recs; src <- q$src
options(width=250)
ht <- r %>% filter(trait %in% c("plant_height","plant_height_climbing_plant","plant_height_reproductive")) %>% pull(taxon_name)
miss <- src %>% filter(!scientific_name %in% ht, str_detect(Description, "\\d\\s?(m|cm) (tall|high)")) 
cat("HEIGHT MISSES", nrow(miss), "\n"); for (i in seq_len(nrow(miss))) cat(miss$scientific_name[i], ": ", str_extract(miss$Description[i], "[^.;]*\\d\\s?(m|cm) (tall|high)[^.;]*"), "\n")
cat("\nGF\n"); r %>% filter(trait %in% c("plant_growth_form","stem_growth_habit","plant_growth_substrate"), is.na(scoring)) %>% left_join(src %>% transmute(taxon_name=scientific_name, s=str_trunc(Description,130)), by="taxon_name") %>% select(taxon_name, trait, value, qualifier, desc=s) %>% as.data.frame() %>% print(right=F)
