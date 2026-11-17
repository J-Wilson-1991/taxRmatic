#' Parse a taxRmatic figure-filter string
#'
#' Converts a taxRmatic figure-filter string into a list of filter sets suitable
#' for filtering a `taxRmatic_figures` object.
#'
#' @details
#' Figure-filter strings are used to connect descriptive text to matching figure
#' metadata. They can be used in the `figure_filter` row of a taxRmatic
#' character-state matrix, and in `fg{}` tags used by [replace_figure_tags()].
#'
#' A figure-filter string describes one or more sets of conditions that are used
#' to match rows in a figure metadata table. Column names on the left-hand side
#' of `=` should correspond to columns in the relevant `taxRmatic_figures`
#' object. Values on the right-hand side should correspond to values in those
#' columns.
#'
#' ## Filter syntax
#'
#' - `;` separates alternative filter sets. These are treated as OR.
#' - `&` separates column filters within one filter set. These are treated as
#'   AND.
#' - `=` separates a filter column from its accepted value or values.
#' - `OR` separates multiple accepted values for the same column.
#'
#' The `OR` operator must be written in uppercase as a separate word, with spaces
#' around it. Lowercase `or` is treated as ordinary text. This makes the syntax
#' safer for biological or descriptive terms that may contain the letters `or`.
#'
#' ## Examples of interpretation
#'
#' The filter:
#'
#' `"feature = cephalothorax"`
#'
#' matches figures where `feature` is `"cephalothorax"`.
#'
#' The filter:
#'
#' `"feature = cephalothorax OR eye group"`
#'
#' matches figures where `feature` is either `"cephalothorax"` or
#' `"eye group"`.
#'
#' The filter:
#'
#' `"feature = cephalothorax & angle = dorsal"`
#'
#' matches figures where `feature` is `"cephalothorax"` and `angle` is
#' `"dorsal"`.
#'
#' The filter:
#'
#' `"feature = cephalothorax OR eye group & angle = dorsal; feature = mouthparts"`
#'
#' is interpreted as:
#'
#' `(feature %in% c("cephalothorax", "eye group") AND angle == "dorsal") OR
#'  (feature == "mouthparts")`
#'
#' ## Notes
#'
#' Figure filters are not R code and values should not be quoted. For example,
#' use `"angle = dorsal"`, not `"angle = \"dorsal\""`.
#'
#' Blank or missing filters return `NULL`.
#'
#' @section Choosing figures without programming:
#' A figure filter is ordinary text that refers to the column names and values in your figures CSV. For example, `feature = prosoma & angle = dorsal` matches rows whose feature is prosoma **and** whose angle is dorsal. `feature = prosoma OR eye group` accepts either value in one column; a semicolon (`;`) joins alternative sets of conditions.
#'
#' These expressions are **not R code**. Enter them as cell text in a `figure_filter` column or row, or inside a figure placeholder such as `fg{tag = demo_map}`. The operators have specific roles: `=` matches a column to values, `&` means both conditions, uppercase `OR` means either value in one column, and `;` means either complete filter set.
#'
#' @param x Single character string containing the figure-filter expression.
#'
#' @return A list of named lists. Each inner named list is one filter set.
#'   Within each filter set, names are column names and values are character
#'   vectors of accepted values. Returns `NULL` if `x` is `NA` or empty.
#'
#' @examples
#' parse_figure_filter("feature = cephalothorax")
#' parse_figure_filter("feature = cephalothorax OR eye group")
#' parse_figure_filter("feature = cephalothorax & angle = dorsal")
#' parse_figure_filter(
#'   "feature = cephalothorax OR eye group & angle = dorsal; feature = mouthparts"
#' )
#'
#' @export
parse_figure_filter <- function(x) {

  # VALIDATE -------------------------------------------------------------------

  if (length(x) != 1 || !is.character(x)) {
    stop(
      "parse_figure_filter() expects a single character string.",
      call. = FALSE
    )
  }

  if (is.na(x) || trimws(x) == "") {
    return(NULL)
  }

  # SPLIT INTO ALTERNATIVE FILTER SETS -----------------------------------------
  # Semicolons separate complete alternative filter sets. Each set is combined
  # using AND internally, while the sets themselves are combined using OR.

  filter_sets <- strsplit(x, ";", fixed = TRUE)[[1]]
  filter_sets <- trimws(filter_sets)
  filter_sets <- filter_sets[nzchar(filter_sets)]

  parsed_sets <- lapply(filter_sets, function(filter_set) {

    # SPLIT INTO AND CONDITIONS ------------------------------------------------
    # Ampersands separate column-level requirements within one filter set.

    clauses <- strsplit(filter_set, "&", fixed = TRUE)[[1]]
    clauses <- trimws(clauses)
    clauses <- clauses[nzchar(clauses)]

    if (length(clauses) == 0) {
      stop(
        "Invalid figure_filter set: '", filter_set,
        "'. No filter clauses supplied.",
        call. = FALSE
      )
    }

    parsed_clauses <- lapply(clauses, function(clause) {

      # PARSE COLUMN + VALUES --------------------------------------------------

      parts <- strsplit(clause, "=", fixed = TRUE)[[1]]

      if (length(parts) != 2) {
        stop(
          "Invalid figure_filter clause: '", clause,
          "'. Each clause must contain exactly one '='.",
          call. = FALSE
        )
      }

      filter_col <- trimws(parts[1])

      if (filter_col == "") {
        stop(
          "Invalid figure_filter clause: '", clause,
          "'. Filter column name is empty.",
          call. = FALSE
        )
      }

      # SPLIT ACCEPTABLE VALUES ------------------------------------------------
      # Multiple accepted values within a column are separated using uppercase OR.
      # The OR operator must be a standalone word with whitespace on both sides.

      filter_vals <- strsplit(
        parts[2],
        "\\s+OR\\s+",
        perl = TRUE
      )[[1]]

      filter_vals <- trimws(filter_vals)
      filter_vals <- filter_vals[nzchar(filter_vals)]

      if (length(filter_vals) == 0) {
        stop(
          "Invalid figure_filter clause: '", clause,
          "'. No filter values supplied.",
          call. = FALSE
        )
      }

      stats::setNames(list(filter_vals), filter_col)
    })

    # COMBINE FILTERS WITHIN SET -----------------------------------------------

    parsed_set <- do.call(c, parsed_clauses)

    if (anyDuplicated(names(parsed_set))) {
      stop(
        "Invalid figure_filter set: '", filter_set,
        "'. The same filter column is used more than once within one '&' set.",
        call. = FALSE
      )
    }

    parsed_set
  })

  # RETURN PARSED FILTER SETS --------------------------------------------------

  parsed_sets
}
