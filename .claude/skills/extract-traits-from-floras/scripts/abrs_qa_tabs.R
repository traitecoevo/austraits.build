suppressPackageStartupMessages({library(dplyr);library(stringr)})
S <- Sys.getenv("S"); q <- readRDS(file.path(S,"qa.rds")); r <- q$recs
cat_r <- r %>% filter(kind %in% "cat" | is.na(kind), !trait %in% c("flowering_time","fruiting_time"))
tab <- cat_r %>% mutate(desc=str_trunc(desc,160)) %>% count(trait, desc, value, qualifier, region, scoring)
sink(file.path(S,"cat_tab.txt")); for (t in sort(unique(tab$trait))) { cat("\n=====",t,"\n"); x <- tab %>% filter(trait==t); for(i in seq_len(nrow(x))) cat(sprintf("[%s|%s|%s] %s  =>  %s (%d)\n", coalesce(x$qualifier[i],""), coalesce(x$region[i],""), coalesce(x$scoring[i],""), x$desc[i], x$value[i], x$n[i])) }; sink()
num <- r %>% filter(kind %in% "num") %>% group_by(trait) %>% summarise(n=n(), lo=min(min,max,na.rm=T), hi=max(max,na.rm=T), bad=sum(min>max,na.rm=T))
print(as.data.frame(num))
