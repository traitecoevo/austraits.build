
combine_austraits <- function(..., d=list(...), variable_definitions, compiler_contacts) {
  combine <- function(name, d) {
    ret <- plyr::llply(d, function(x) x[[name]])
#    rename_columns(ret, ".id", "studyName")
  }

  names(d) <- sapply(d, "[[", "key")
  ret <- list(data=combine("data", d)
              # methods=combine("methods", d),
              # contacts=rbind.fill(combine("contacts", d),
              #   data.frame(studyName="austraits_construction", compiler_contacts)),
              # references=combine("references", d),
              # metadata=combine("metadata", d)
    )

#  ret$bibtex <- do.call("c", unname(lapply(d, "[[", "bibtex")))
#  ret$dictionary <- variable_definitions
  ret
}

## Functions for extracting bits from austraits.  Works around some of the
## limitations in how I wrote remake.
extract_austraits_data <- function(austraits) {
  austraits$data
}
