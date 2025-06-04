#' Card Vector Constructor
#'
#' Create a custom vctrs card vector.
#'
#' @param x A character vector of cards like "A♠", "10♣", "K♥"
#' @return A card_vector object
#' @export
card_vector <- function(x = character()) {
  vctrs::vec_assert(x, character())
  structure(x, class = c("card_vector", "vctrs_vctr"))
}

#' @export
format.card_vector <- function(x, ...) {
  paste0("[", x, "]")
}

#' @export
vec_ptype2.card_vector.card_vector <- function(x, y, ...) card_vector()
#' @export
vec_cast.card_vector.card_vector <- function(x, to, ...) x

#' Get card ranks
#'
#' @param x A card_vector
#' @return Character vector of ranks (e.g., "A", "10", "Q")
#' @export
card_rank <- function(x) {
  stopifnot(inherits(x, "card_vector"))
  sub("[\u2660\u2663\u2665\u2666]", "", x)  # remove suit
}

#' Get card suits
#'
#' @param x A card_vector
#' @return Character vector of suits (e.g., "♠", "♥")
#' @export
card_suit <- function(x) {
  stopifnot(inherits(x, "card_vector"))
  gsub("[0-9JQKA]+", "", x)  # remove rank
}

#' Check if a card is a face card (J, Q, K)
#'
#' @param x A card_vector
#' @return Logical vector
#' @export
card_is_face <- function(x) {
  rank <- card_rank(x)
  rank %in% c("J", "Q", "K")
}
