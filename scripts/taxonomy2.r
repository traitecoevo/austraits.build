strip_names <- function(x) {
  x %>% 
    str_replace_all(" subsp. ", " ") %>% 
    str_replace_all(" var. ", " ") %>% 
    str_replace_all(" ser. ", " ") %>% 
    str_replace_all(" f. ", " ") %>%
    str_replace_all(" s.l. ", " ") %>% 
    str_replace_all(" s.s. ", " ") %>% 
    str_replace_all("[-._()]", " ") %>% 
    str_squish() %>% tolower() 
}
