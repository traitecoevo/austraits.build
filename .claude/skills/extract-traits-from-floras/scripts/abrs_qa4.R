suppressPackageStartupMessages({library(dplyr);library(stringr)})
S <- Sys.getenv("S"); q <- readRDS(file.path(S,"qa.rds")); r <- q$recs; src <- q$src
options(width=300)
cat("PHENOLOGY\n")
ph <- r %>% filter(trait %in% c("flowering_time","fruiting_time")) %>% select(taxon_name, trait, value, region)
x <- src %>% select(taxon_name=scientific_name, Phenology) %>% left_join(ph %>% tidyr::pivot_wider(names_from=trait, values_from=value, values_fn=function(z) paste(z, collapse="/")), by="taxon_name")
for (i in seq_len(nrow(x))) if (!is.na(x$Phenology[i])) cat(sprintf("%-40s FL=%s FR=%s RG=%s | %s\n", str_trunc(x$taxon_name[i],40), x$flowering_time[i], x$fruiting_time[i], x$region[i], str_trunc(x$Phenology[i], 200)))
cat("\nQUALIFIED\n")
r %>% filter(!is.na(qualifier) | !is.na(region), !trait %in% c("flowering_time","fruiting_time")) %>% transmute(taxon_name, trait, value, qualifier, region, desc=str_trunc(desc,90)) %>% as.data.frame() %>% print(right=F)
