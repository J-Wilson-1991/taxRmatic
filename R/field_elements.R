#' Field customisation helpers
#'
#' Helper functions used with [format_custom_fields()] to build customised fields
#' from column values and conditional text.
#'
#' These functions create small instruction objects that are passed to
#' [format_custom_fields()]. `column()` inserts values from a column in the input
#' data, while `prefix()` and `suffix()` add text conditionally. Prefixes are
#' added only when the following column value is present, and suffixes are added
#' only when the preceding column value is present.
#'
#' @param name Character(1). Name of the column to insert.
#' @param text Character(1). Text to add before or after a column value.
#'
#' @return
#' `column()` returns an object of class `"column_ref"`.
#' `prefix()` returns an object of class `"prefix"`.
#' `suffix()` returns an object of class `"suffix"`.
#'
#' @examples
#' specimens <- data.frame(
#'   regno = c("12345", "67890", ""),
#'   stringsAsFactors = FALSE
#' )
#' specimens$registration <- format_custom_fields(
#'   specimens,
#'   list(
#'     prefix("(WAM T"),
#'     column("regno"),
#'     suffix(")")
#'   )
#' )
#' specimens$registration
#'
#' @name field_customisation_helpers
NULL


#' @rdname field_customisation_helpers
#' @export
column <- function(name) {

  if (!is.character(name) || length(name) != 1 || is.na(name) || trimws(name) == "") {
    stop(
      "`name` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  structure(list(name = name), class = "column_ref")
}


#' @rdname field_customisation_helpers
#' @export
prefix <- function(text) {

  if (!is.character(text) || length(text) != 1 || is.na(text)) {
    stop(
      "`text` must be a single character string.",
      call. = FALSE
    )
  }

  structure(list(text = text), class = "prefix")
}


#' @rdname field_customisation_helpers
#' @export
suffix <- function(text) {

  if (!is.character(text) || length(text) != 1 || is.na(text)) {
    stop(
      "`text` must be a single character string.",
      call. = FALSE
    )
  }

  structure(list(text = text), class = "suffix")
}
