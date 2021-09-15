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

# A random strng of letters -- useful for defining unique hyperlinks
random_string <- function(n=8) {
  base::sample(LETTERS, n, TRUE) %>% paste0(collapse="")
}

# Store a txt note in a tibble with two columns
as_note <- function(note, link=NA_character_) {
  tibble(note = note, link = ifelse(is.na(link), random_string(), link)) %>% mutate_all(as.character)
}

# start note recoder
start_notetaker <- function() {
  ret <- as_note(character(), character())

  function(f = function(x) x, ...){
    ret <<- f(ret, ...)
  }
}

add_note <- function(notes, new_note) {
  bind_rows(notes, new_note)
}

print_note <- function(note, as_anchor=FALSE, anchor_text = "", link_text = "link") {
  if(as_anchor)
    sprintf('%s <a name="%s"> %s </a>', note$note, note$link, anchor_text )
  else
    sprintf('%s [%s](#%s)', note$note, link_text, note$link)
}

print_notes <- function(notes, i=nrow(notes), ...) {
  notes %>%
    get_note(i) %>%
    print_note(...)
}

print_all_notes <- function(notes, ..., numbered=TRUE) {
  i <- seq_len(nrow(notes))
  x <- print_notes(notes, i =i)
  sprintf("%d. %s", i, x)
}

# returns as secific note, as indicated by row number i
#' Title
#'
#' @param notes 
#' @param i 
#'
#' @return
#' @export
#'
#' @examples
get_note <- function(notes, i=nrow(notes)) {
  notes[i,]
}


