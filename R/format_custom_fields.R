#' Format custom fields row by row
#'
#' Combines information from several spreadsheet columns into text. You can
#' add words before or after a value, for example to put a unit after a
#' measurement. Each spreadsheet row produces one text value. Use the
#' `column()`, `prefix()` and `suffix()` helpers to choose the pieces.
#'
#' The elements to combine can be:
#'
#' \itemize{
#' \item column("colname") — insert the value from a column.
#' \item character strings — insert fixed text exactly as written.
#' \item prefix("text") — add text before a column value, but only when that
#' column contains data.
#' \item suffix("text") — add text after a column value, but only when that
#' column contains data.
#' }
#'
#' Use character strings for text that should always appear. Use prefix() or
#' suffix() for text or punctuation that should only appear when a particular
#' column has a value.
#'
#' This helps avoid unwanted punctuation when some fields are empty. For example,
#' if a state is missing, prefix("(") and suffix(")") will also be omitted,
#' so the output will not contain empty brackets.
#'
#' prefix() must be placed immediately before the column() it belongs to.
#' suffix() must be placed immediately after the column() it belongs to.
#'
#' @section When to use conditional prefixes and suffixes:
#' Use this before generating material examined when a database field needs additional wording. For example, `list(prefix("(DEMO "), column("regno"), suffix(")"))` turns the value `9002` into `(DEMO 9002)`. The pieces `prefix()` and `suffix()` are included only when the adjacent column has a value, preventing empty brackets.
#'
#' These helpers are **different** from the `prefix` and `suffix` instruction rows in the character matrix and from whole-statement `prefix`/`suffix` rows in the diagnostic table. Each serves its own step of the workflow.
#'
#' @param df Data frame containing the columns to use.
#' @param elements List describing how the output should be built.
#' @param separator Character(1). Text used to separate the main pieces of the
#' output. Defaults to a single space, " ".
#'
#' @return A character vector with one formatted value for each row of df.
#'
#' @examples
#' df <- data.frame(
#'   locality = c("Perth", ""),
#'   state = c("WA", ""),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Combine two columns
#' format_custom_fields(
#'   df = df,
#'   elements = list(
#'     column("locality"),
#'     column("state")
#'   )
#' )
#' # [1] "Perth WA" ""
#'
#' # Add brackets around a column value, but only when that value is present
#' format_custom_fields(
#'   df = df,
#'   elements = list(
#'     column("locality"),
#'     prefix("("), column("state"), suffix(")")
#'   )
#' )
#' # [1] "Perth (WA)" ""
#'
#' # Plain text is always included, even when the column is empty
#' format_custom_fields(
#'   df = df,
#'   elements = list(
#'     "State:",
#'     column("state")
#'   )
#' )
#' # [1] "State: WA" "State:"
#'
#' # Use prefix() when text should only appear if the column has data
#' format_custom_fields(
#'   df = df,
#'   elements = list(
#'     prefix("State: "), column("state")
#'   )
#' )
#' # [1] "State: WA" ""
#'
#' # Combine fixed text, a prefix, a column value, and a suffix
#' format_custom_fields(
#'   df = df,
#'   elements = list(
#'     "Locality:",
#'     prefix("["), column("locality"), suffix("]")
#'   )
#' )
#' # [1] "Locality: [Perth]" "Locality:"
#'
#' @export
format_custom_fields <- function(df, elements, separator = " ") {

  # CHECK INPUTS ----------------------------------------------------------------

  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  if (!is.list(elements)) {
    stop("`elements` must be supplied as a list.", call. = FALSE)
  }

  if (!is.character(separator) || length(separator) != 1 || is.na(separator)) {
    stop("`separator` must be a single character string.", call. = FALSE)
  }

  n <- nrow(df)

  if (n == 0) {
    return(character(0))
  }

  if (length(elements) == 0) {
    return(rep("", n))
  }

  # INTERNAL HELPER -------------------------------------------------------------

  get_column_values <- function(col_name) {
    values <- as.character(df[[col_name]])
    values[is.na(values)] <- ""
    values
  }

  # VALIDATE ELEMENT STRUCTURE --------------------------------------------------

  for (i in seq_along(elements)) {

    el <- elements[[i]]

    valid_element <- inherits(el, "column_ref") ||
      inherits(el, "prefix") ||
      inherits(el, "suffix") ||
      is.character(el)

    if (!valid_element) {
      stop(
        "All elements in `elements` must be character strings, `column()` ",
        "references, `prefix()` objects, or `suffix()` objects.",
        call. = FALSE
      )
    }

    if (inherits(el, "prefix")) {
      if (i == length(elements) || !inherits(elements[[i + 1]], "column_ref")) {
        stop(
          "`prefix()` must be placed immediately before `column()`.",
          call. = FALSE
        )
      }
    }

    if (inherits(el, "suffix")) {
      if (i == 1 || !inherits(elements[[i - 1]], "column_ref")) {
        stop(
          "`suffix()` must be placed immediately after `column()`.",
          call. = FALSE
        )
      }
    }

    if (inherits(el, "column_ref") && !(el$name %in% names(df))) {
      stop(
        "Column not found in `df`: '", el$name, "'.",
        call. = FALSE
      )
    }
  }

  # EVALUATE ELEMENTS -----------------------------------------------------------

  evaluated <- lapply(seq_along(elements), function(i) {

    el <- elements[[i]]

    if (inherits(el, "column_ref")) {
      return(get_column_values(el$name))
    }

    if (inherits(el, "prefix")) {
      next_col <- elements[[i + 1]]
      next_vals <- get_column_values(next_col$name)
      return(ifelse(next_vals != "", el$text, ""))
    }

    if (inherits(el, "suffix")) {
      prev_col <- elements[[i - 1]]
      prev_vals <- get_column_values(prev_col$name)
      return(ifelse(prev_vals != "", el$text, ""))
    }

    rep(el, n)
  })

  # BUILD OUTPUT ----------------------------------------------------------------

  out <- character(n)
  i <- 1

  while (i <= length(elements)) {

    el <- elements[[i]]
    row_values <- character(n)

    if (inherits(el, "prefix")) {

      pre_vals <- evaluated[[i]]
      col_vals <- evaluated[[i + 1]]

      if (i + 2 <= length(elements) && inherits(elements[[i + 2]], "suffix")) {
        suf_vals <- evaluated[[i + 2]]
        row_values <- paste0(pre_vals, col_vals, suf_vals)
        i <- i + 3
      } else {
        row_values <- paste0(pre_vals, col_vals)
        i <- i + 2
      }

    } else if (inherits(el, "column_ref")) {

      row_values <- evaluated[[i]]

      if (i + 1 <= length(elements) && inherits(elements[[i + 1]], "suffix")) {
        suf_vals <- evaluated[[i + 1]]
        row_values <- paste0(row_values, suf_vals)
        i <- i + 2
      } else {
        i <- i + 1
      }

    } else {

      row_values <- evaluated[[i]]
      i <- i + 1
    }

    out <- ifelse(
      out == "",
      row_values,
      ifelse(
        row_values == "",
        out,
        paste(out, row_values, sep = separator)
      )
    )
  }

  out
}
