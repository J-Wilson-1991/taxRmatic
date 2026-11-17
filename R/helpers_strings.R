#' Split and trim a delimited character string
#'
#' Splits a string on a user-specified delimiter, trims whitespace,
#' and returns a sorted character vector.
#'
#' @param x A character string to split and trim.
#' @param split_char A character used to split the string (default: ",").
#' @return A character vector.
#' @keywords internal
split_and_trim <- function(x, split_char = ",") {
  if (is.na(x) || x == "") return(character(0))

  sort(
    trimws(
      unlist(
        strsplit(as.character(x), split_char)
      )
    )
  )
}
