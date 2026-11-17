################################################################################
# helper function: check uppercase
################################################################################

#' Test whether letters are uppercase
#'
#' Internal helper used by [collapse_ranges()] when processing alphabetical
#' input.
#'
#' @param char_vec Character vector of single letters.
#'
#' @return Logical vector.
#' @keywords internal
#' @noRd
is_uppercase <- function(char_vec) {
  if (!is.character(char_vec)) {
    stop("Input must be a character vector.", call. = FALSE)
  }

  vapply(char_vec, function(char) char == toupper(char), logical(1))
}


################################################################################
# helper function: append range
################################################################################

#' Format a collapsed range or individual values
#'
#' Internal helper used by [collapse_ranges()] to convert consecutive numeric or
#' alphabetical values into either a range or individual entries.
#'
#' @param start Start of the range.
#' @param end End of the range.
#' @param is_letter Logical. If `TRUE`, treat values as letters; otherwise treat
#'   them as numeric.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
append_range <- function(start, end, is_letter = FALSE) {

  if (is_letter) {
    if ((match(end, letters) - match(start, letters)) >= 2) {
      return(paste(start, end, sep = "\u2013"))
    }

    out <- c(start)
    if (end != start) {
      out <- c(out, end)
    }

    return(out)
  }

  if ((end - start) >= 2) {
    return(paste(start, end, sep = "\u2013"))
  }

  out <- c(start)
  if (end != start) {
    out <- c(out, end)
  }

  out
}


################################################################################
# main function: collapse ranges
################################################################################

#' Collapse numeric or alphabetical sequences into ranges
#'
#' Collapses consecutive numeric values or single letters into compact ranges.
#' This is useful for producing concise taxonomic text, such as figure-part
#' references or other ordered value lists.
#'
#' Consecutive runs of three or more values are collapsed using an en dash.
#' Runs of one or two values are returned as individual values. For example,
#' `c(1, 2, 3, 5, 6, 8)` uses an en dash between 1 and 3, while
#' `c("A", "B", "C", "E")` uses an en dash between A and C.
#'
#' Input must be either entirely numeric or entirely single alphabetical
#' characters. Mixed numeric and alphabetical input is not supported. Letter
#' input must also be consistently uppercase or consistently lowercase.
#'
#' Missing values and empty strings are not allowed.
#'
#' @param input_vec Numeric vector, or character vector containing numeric values
#'   or single letters.
#'
#' @return Character string containing the collapsed range expression. If
#'   `input_vec` has length zero, returns `""`.
#'
#' @examples
#' collapse_ranges(c(1, 2, 3, 5, 6, 8, 9, 10))
#' collapse_ranges(c("1", "2", "3", "5"))
#' collapse_ranges(c("a", "b", "c", "e"))
#' collapse_ranges(c("A", "B", "C", "E"))
#'
#' @export
collapse_ranges <- function(input_vec) {

  # VALIDATION -----------------------------------------------------------------

  if (length(input_vec) == 0) {
    return("")
  }

  if (any(is.na(input_vec))) {
    stop(
      "`input_vec` cannot contain missing values.",
      call. = FALSE
    )
  }

  if (any(trimws(as.character(input_vec)) == "")) {
    stop(
      "`input_vec` cannot contain empty strings.",
      call. = FALSE
    )
  }

  input_vec <- unique(input_vec)

  # HANDLE SINGLE ELEMENT ------------------------------------------------------

  if (length(input_vec) == 1) {
    if (is.character(input_vec)) {
      input_vec <- trimws(input_vec)

      if (grepl("^[A-Za-z]$", input_vec)) {
        return(input_vec)
      }

      suppressWarnings(num_val <- as.numeric(input_vec))

      if (!is.na(num_val)) {
        return(as.character(num_val))
      }

      stop(
        "`input_vec` must contain numeric values or single alphabetical characters.",
        call. = FALSE
      )
    }

    if (is.numeric(input_vec)) {
      return(as.character(input_vec))
    }

    stop(
      "`input_vec` must contain numeric values or single alphabetical characters.",
      call. = FALSE
    )
  }

  # DETERMINE INPUT TYPE -------------------------------------------------------

  if (is.character(input_vec)) {
    input_vec <- trimws(input_vec)
  }

  suppressWarnings(numeric_vec <- as.numeric(input_vec))

  all_numeric <- all(!is.na(numeric_vec))
  all_alpha <- all(grepl("^[A-Za-z]$", input_vec))

  if (!(all_numeric || all_alpha)) {
    stop(
      "`input_vec` must be entirely numeric or entirely single alphabetical ",
      "characters. Mixed input is not supported.",
      call. = FALSE
    )
  }

  # NUMERIC INPUT --------------------------------------------------------------

  if (all_numeric) {
    vals <- sort(unique(numeric_vec))

    result <- character()
    range_start <- range_end <- vals[1]

    for (i in seq_along(vals)[-1]) {
      if (vals[i] == range_end + 1) {
        range_end <- vals[i]
      } else {
        result <- c(result, append_range(range_start, range_end, FALSE))
        range_start <- range_end <- vals[i]
      }
    }

    result <- c(result, append_range(range_start, range_end, FALSE))

    return(paste(result, collapse = ", "))
  }

  # LETTER INPUT ---------------------------------------------------------------

  upper_status <- is_uppercase(input_vec)

  if (any(upper_status) && any(!upper_status)) {
    stop(
      "Letter input must be consistently uppercase or consistently lowercase. ",
      "Mixed case is not supported.",
      call. = FALSE
    )
  }

  is_upper <- all(upper_status)

  letter_pos <- match(tolower(input_vec), letters)
  letter_pos <- sort(unique(letter_pos))

  result <- character()
  range_start <- range_end <- letter_pos[1]

  for (i in seq_along(letter_pos)[-1]) {
    if (letter_pos[i] == range_end + 1) {
      range_end <- letter_pos[i]
    } else {
      result <- c(
        result,
        append_range(letters[range_start], letters[range_end], TRUE)
      )

      range_start <- range_end <- letter_pos[i]
    }
  }

  result <- c(
    result,
    append_range(letters[range_start], letters[range_end], TRUE)
  )

  if (is_upper) {
    result <- toupper(result)
  }

  paste(result, collapse = ", ")
}
