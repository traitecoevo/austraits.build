## helper functions to record a series of notes, with links
## Inspired from https://github.com/cran/rmngb/blob/master/R/counter.R

# ## Example usage
# qu(notetaker_add_note, notetaker_as_note("yo")) %>% notetaker_print_notes(as_anchor=TRUE)

# # start recording
# qu <- notetaker_start()

# # Running this function on its own does doting
# qu()

# # See existing content by adding brackets to force printing, or call print
# (qu())
# qu() %>% print()

# # Create a note -- see that a random string is added as a poential link
# notetaker_as_note("my_note")

# # Create a note -- with specified name of link
# notetaker_as_note("my_note", "abbba"))

# # Create a note and add to record
# qu(notetaker_add_note, notetaker_as_note("my_note1"))
# qu(notetaker_add_note, notetaker_as_note("my_note2"))
# qu(notetaker_add_note, notetaker_as_note("my_note3"))
# qu(notetaker_add_note, notetaker_as_note("my_note4"))

# (qu())

# # get first note
#  qu() %>%
#     notetaker_get_note(1)

# # get first two notes
#  qu() %>%
#     notetaker_get_note(1:2)

# # get last note
#  qu() %>%
#     notetaker_get_note()

# # print first note as anchor text
# qu() %>% notetaker_print_notes(1, as_anchor=TRUE)

# # print first and link back to anchor text
# qu() %>% notetaker_print_notes(1)

# # print last note and link back to anchor text
# qu() %>% notetaker_print_notes()

# # create note and print it as anchor
# qu(notetaker_add_note, notetaker_as_note("new_note")) %>% notetaker_print_notes(as_anchor=TRUE)

# A random string of letters -- useful for defining unique hyperlinks
#' Create a string of random letters
#' 
#' Creates a string of random letters with 8 characters as the default,
#' useful for defining unique hyperlinks 
#'
#' @param n numerical integer, default is 8 
#'
#' @return character string with 8 letters
notes_random_string <- function(n=8) {
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
notetaker_as_note <- function(note, link=NA_character_) {
  tibble::tibble(note = note, link = ifelse(is.na(link), notes_random_string(), link)) %>% dplyr::mutate_all(as.character)
}

# start note recorder
#' Start note recorder (needs review?)
#' 
#' Note recorder used in report_study.Rmd file to initiate note recorder
#' 
#' @return A tibble where notes are recorded
notetaker_start <- function() {
  ret <- notetaker_as_note(character(), character())

  function(f = function(x) x, ...){
    ret <<- f(ret, ...)
  }
}

#' Add a note to the note recorder as a new row
#'
#' @param notes object containing the report notes
#' @param new_note vector of character notes to be added to existing notes
#'
#' @return A tibble with additional notes added
notetaker_add_note <- function(notes, new_note) {
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
notetaker_print_note <- function(note, as_anchor=FALSE, anchor_text = "", link_text = "link") {
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
#' @param i specify the row which contains the note to be returned
#' @param ... arguments passed to notetaker_print_note()
#'
#' @return character string containing the notes
notetaker_print_notes <- function(notes, i=nrow(notes), ...) {
  notes %>%
    notetaker_get_note(i) %>%
    notetaker_print_note(...)
}

#' Print all notes
#'
#' @param notes object containing the report notes
#' @param ... arguments passed to other functions
#' @param numbered logical default is TRUE
#'
#' @return character string containing the notes
notetaker_print_all <- function(notes, ..., numbered=TRUE) {
  i <- seq_len(nrow(notes))
  x <- notetaker_print_notes(notes, i =i)
  sprintf("%d. %s", i, x)
}

#' Return a specific row from notes
#' 
#' Returns a specific row from notes specified by i. Default is nrow(notes) which 
#' returns the last note
#'
#' @param notes object containing the report notes
#' @param i numerical; row number for corresponding note, default is nrow(notes)
#'
#' @return a single row from a tibble
notetaker_get_note <- function(notes, i=nrow(notes)) {
  notes[i,]
}
