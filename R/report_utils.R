get_SHA_link <- function(...) {
  sha <- get_SHA(...)
  sprintf("[%s](https://github.com/traitecoevo/austraits/tree/%s)",   sha, sha)
}

get_SHA <- function(path = rprojroot::find_root("remake.yml")) {
  git2r::branch_target(git2r::head(git2r::repository(path)))
}

pallette1 <- function(){
  c('red', 'seagreen3', 'steelblue3', 'yellow2')
}

## format a table with kable and default styling
my_kable_styling <- function(...) {
  kableExtra::kable(...) %>%
  kableExtra::kable_styling(..., 
                bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = FALSE, 
                position = "left"
                ) %>%
  # hack to add margin to plot
  gsub('style="width: auto ', 'style="margin-left:30px; width: auto ', .)
}

as_link <- function(link, text, type="md") {
  if(type=="md")
    sprintf('[%s](%s)', text, link)
  else
    sprintf("<a href='%s'> %s </a>", link, text)
}

as_link_tpl <- function(ID) {
  sprintf("http://www.theplantlist.org/tpl1.1/record/%s", ID) %>%
  as_link(ID)
}

as_link_apc <- function(ID) {
  sprintf("https://biodiversity.org.au/nsl/services/node/apc/%s", ID) %>%
  as_link(ID)
}

as_link_apni <- function(ID) {
  sprintf("http://id.biodiversity.org.au/node/apni/%s", ID) %>%
  as_link(ID)
}

# calculates coefficient of variation
CV <- function(x){
  sqrt(var(x))/mean(x)
}

md_link <- function(text, link) {
  sprintf("[%s](%s)", text, link)
}

md_link_doi <- function(doi) {
  md_link(doi, paste0("http://doi.org/", doi))
}


