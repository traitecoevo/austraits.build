suppressPackageStartupMessages({library(dplyr);library(stringr);library(purrr)})
S <- Sys.getenv("S"); q <- readRDS(file.path(S,"qa.rds")); r <- q$recs; src <- q$src; u <- q$units
have <- r %>% distinct(taxon_name, trait)
chk <- list(leaf_length=c("lamina","leaf"), fruit_length="fruit", seed_length="seed")
for (t in names(chk)) {
  x <- u %>% filter(sec %in% chk[[t]], str_detect(text, "[0-9]\\s?(mm|cm|m)\\b.*long|long.*[0-9]\\s?(mm|cm|m)\\b|[0-9]\\s?(?:mm|cm|m)? ?(x|×)")) %>% anti_join(have %>% filter(trait==t), by="taxon_name") %>% group_by(taxon_name) %>% summarise(t=paste(text, collapse=" // "))
  cat("\n=== MISS", t, nrow(x), "\n"); for (i in seq_len(nrow(x))) cat(x$taxon_name[i], ":", str_trunc(x$t[i], 250), "\n")
}
# measurement clauses with no value from any trait: leading words
