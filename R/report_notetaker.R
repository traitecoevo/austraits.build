## helper functions to record a series of notes, with links
## Inspired from https://github.com/cran/rmngb/blob/master/R/counter.R

# ## Example usage
# qu(add_note, as_note("yo")) %>% print_notes(as_anchor=TRUE)

# # start recording
# qu <- start_notetaker()

# # Running this function on its own does doting
# qu()

# # See existing content by adding brackets to force printing, or call print
# (qu())
# qu() %>% print()

# # Create a note -- see that a random string is added as a poential link
# as_note("my_note")

# # Create a note -- with specified name of link
# as_note("my_note", "abbba"))

# # Create a note and add to record
# qu(add_note, as_note("my_note1"))
# qu(add_note, as_note("my_note2"))
# qu(add_note, as_note("my_note3"))
# qu(add_note, as_note("my_note4"))

# (qu())

# # get first note
#  qu() %>%
#     get_note(1)

# # get first two notes
#  qu() %>%
#     get_note(1:2)

# # get last note
#  qu() %>%
#     get_note()

# # print first note as anchor text
# qu() %>% print_notes(1, as_anchor=TRUE)

# # print first and link back to anchor text
# qu() %>% print_notes(1)

# # print last note and link back to anchor text
# qu() %>% print_notes()

# # create note and print it as anchor
# qu(add_note, as_note("new_note")) %>% print_notes(as_anchor=TRUE)

# A random string of letters -- useful for defining unique hyperlinks
#' Create a string of random letters
#' 
#' Creates a string of random letters with 8 characters as the default,
#' useful for defining unique hyperlinks 
#'
#' @param n numerical integer, default is 8 
#'
#' @return character string with 8 letters
#' @export
#'
#' @examples
random_string <- function(n=8) {
  base::sample(LETTERS, n, TRUE) %>% paste0(collapse="")
}

# Store a txt note in a tibble with two columns
#' Create a tibble with two columns with note and link
#' 
#' Creates a tibble with two columns with one column consisting 
#' of a randomly generated string of letters
#'
#' @param note character string 
#' @param link character string, default is NA_character_ which generates a random string 
#'
#' @return a tibble with two columns named note and link
#' @export
#'
#' @examples
as_note <- function(note, link=NA_character_) {
  tidyr::tibble(note = note, link = ifelse(is.na(link), random_string(), link)) %>% dplyr::mutate_all(as.character)
}

# start note recorder
#' Start note recorder (needs review?)
#' 
#' Note recorder used in report_study.Rmd file to initiate note recorder
#' 
#' @return 
#' @export
#'
#' @examples
start_notetaker <- function() {
  ret <- as_note(character(), character())

  function(f = function(x) x, ...){
    ret <<- f(ret, ...)
  }
}

#' Add a note to the note recorder as a new row
#'
#' @param notes object containing the report notes
#' @param new_note vector of character notes to be added to existing notes
#'
#' @return
#' @export
#'
#' @examples
add_note <- function(notes, new_note) {
  dplyr::bind_rows(notes, new_note)
}

#' Print note (needs review?)
#'
#' @param note object containing the report notes
#' @param as_anchor logical default is FALSE
#' @param anchor_text character string, default is ""
#' @param link_text character string, default is "link"
#'
#' @return character string containing the notes
#' @export
#'
#' @examples
print_note <- function(note, as_anchor=FALSE, anchor_text = "", link_text = "link") {
  if(as_anchor)
    sprintf('%s <a name="%s"> %s </a>', note$note, note$link, anchor_text )
  else
    sprintf('%s [%s](#%s)', note$note, link_text, note$link)
}

#' Print a specific row from notes
#' 
#' Prints a specific row from notes specified by i
#'
#' @param notes object containing the report notes
#' @param i 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
print_notes <- function(notes, i=nrow(notes), ...) {
  notes %>%
    get_note(i) %>%
    print_note(...)
}

#' Print all notes
#'
#' @param notes object containing the report notes
#' @param ... 
#' @param numbered logical default is TRUE
#'
#' @return
#' @export
#'
#' @examples
print_all_notes <- function(notes, ..., numbered=TRUE) {
  i <- seq_len(nrow(notes))
  x <- print_notes(notes, i =i)
  sprintf("%d. %s", i, x)
}

# returns a specific note, as indicated by row number i
#' Return a specific row from notes
#' 
#' Returns a specific row from notes specified by i 
#'
#' @param notes object containing the report notes
#' @param i 
#'
#' @return a single row from a tibble
#' @export
#'
#' @examples
get_note <- function(notes, i=nrow(notes)) {
  notes[i,]
}


