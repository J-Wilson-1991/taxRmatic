#' Collapse Rows of Specimen Data into a Material String
#'
#' Concatenate selected fields from each row of a data frame into a single
#' formatted material string.
#'
#' Each row is treated as a specimen (or specimen lot), and selected fields are
#' concatenated to form a specimen-level string. All specimen strings are then
#' joined together into a single output string.
#'
#' @param data_frame A data frame in which each row represents a specimen or
#'   specimen lot.
#' @param fields_to_concat A character vector giving the column names to
#'   concatenate for each row, in the order they should appear.
#' @param field_separator A character string used to separate fields within each
#'   specimen entry. Defaults to `", "`.
#' @param specimen_separator A character string used to separate specimen
#'   entries from one another. Defaults to `"; "`.
#' @param order_specimens_by An optional character vector giving column names by
#'   which to sort rows before concatenation.
#' @param compress_same_data Logical; if `TRUE`, repeated data shared with the
#'   immediately preceding row can be replaced with `same_data_text`, optionally
#'   retaining specified prefix and/or suffix fields. Defaults to `FALSE`.
#' @param same_data_compare_fields A character vector of column names used to
#'   determine whether the current row has the same shared data as the previous
#'   row. Required if `compress_same_data = TRUE`.
#' @param same_data_prefix_fields An optional character vector of column names
#'   to retain before `same_data_text` when repeated data are compressed.
#' @param same_data_suffix_fields An optional character vector of column names
#'   to retain after `same_data_text` when repeated data are compressed.
#' @param same_data_text A character string used when repeated data are
#'   compressed. Defaults to `"same data"`.
#'
#' @details
#' For each row of `data_frame`, the function concatenates the values in
#' `fields_to_concat`, omitting `NA` values and empty strings. The resulting
#' row-level strings are then joined using `specimen_separator`, and a final
#' full stop plus trailing space is appended.
#'
#' If `order_specimens_by` is supplied, rows are sorted before processing.
#'
#' If `compress_same_data = TRUE`, each row after the first is compared with the
#' immediately preceding row using `same_data_compare_fields`. If those values
#' are identical, the repeated data are replaced with `same_data_text`, with any
#' specified prefix and/or suffix fields retained.
#'
#' Returns an empty string if `data_frame` has zero rows.
#'
#' @return A single character string containing all formatted specimen entries,
#'   ending in `". "`, or `""` if no rows are present.
#'
#' @keywords internal
get_material_string <- function(
    data_frame,
    fields_to_concat,
    field_separator = ", ",
    specimen_separator = "; ",
    order_specimens_by = NULL,
    compress_same_data = FALSE,
    same_data_compare_fields = NULL,
    same_data_prefix_fields = NULL,
    same_data_suffix_fields = NULL,
    same_data_text = "same data"
) {

  # VALIDATE INPUTS ------------------------------------------------------------

  if (!is.data.frame(data_frame)) {
    stop("'data_frame' must be a data frame.", call. = FALSE)
  }

  if (nrow(data_frame) == 0) {
    return("")
  }

  if (!is.character(fields_to_concat) || length(fields_to_concat) == 0) {
    stop(
      "'fields_to_concat' must be a non-empty character vector.",
      call. = FALSE
    )
  }

  if (!is.character(field_separator) ||
      length(field_separator) != 1 ||
      is.na(field_separator)) {
    stop("'field_separator' must be a single character string.", call. = FALSE)
  }

  if (!is.character(specimen_separator) ||
      length(specimen_separator) != 1 ||
      is.na(specimen_separator)) {
    stop(
      "'specimen_separator' must be a single character string.",
      call. = FALSE
    )
  }

  if (!is.logical(compress_same_data) || length(compress_same_data) != 1) {
    stop(
      "'compress_same_data' must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.character(same_data_text) ||
      length(same_data_text) != 1 ||
      is.na(same_data_text)) {
    stop("'same_data_text' must be a single character string.", call. = FALSE)
  }



  # SORT SPECIMENS IF REQUESTED ------------------------------------------------

  if (!is.null(order_specimens_by)) {

    if (!is.character(order_specimens_by) || length(order_specimens_by) == 0) {
      stop(
        "'order_specimens_by' must be NULL or a non-empty character vector.",
        call. = FALSE
      )
    }

    missing_order_cols <- setdiff(order_specimens_by, names(data_frame))
    if (length(missing_order_cols) > 0) {
      stop(
        "The following columns specified in 'order_specimens_by' do not exist: ",
        paste(missing_order_cols, collapse = ", "),
        call. = FALSE
      )
    }

    order_df <- data_frame[, order_specimens_by, drop = FALSE]
    order_df[] <- lapply(order_df, function(x) {
      x <- as.character(x)
      x[is.na(x)] <- ""
      x
    })

    data_frame <- data_frame[
      do.call(order, unname(order_df)),
      ,
      drop = FALSE
    ]
  }

  # CHECK REQUIRED COLUMNS -----------------------------------------------------

  missing_cols <- setdiff(fields_to_concat, names(data_frame))
  if (length(missing_cols) > 0) {
    stop(
      "The following columns are missing in 'data_frame': ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # VALIDATE SAME-DATA SETTINGS ------------------------------------------------

  if (compress_same_data) {

    if (is.null(same_data_compare_fields)) {
      stop(
        "'same_data_compare_fields' must be provided when ",
        "'compress_same_data = TRUE'.",
        call. = FALSE
      )
    }

    if (!is.character(same_data_compare_fields) ||
        length(same_data_compare_fields) == 0) {
      stop(
        "'same_data_compare_fields' must be a non-empty character vector.",
        call. = FALSE
      )
    }

    missing_compare <- setdiff(same_data_compare_fields, names(data_frame))
    if (length(missing_compare) > 0) {
      stop(
        "The following 'same_data_compare_fields' are missing in ",
        "'data_frame': ",
        paste(missing_compare, collapse = ", "),
        call. = FALSE
      )
    }

    if (!is.null(same_data_prefix_fields)) {

      if (!is.character(same_data_prefix_fields)) {
        stop(
          "'same_data_prefix_fields' must be NULL or a character vector.",
          call. = FALSE
        )
      }

      missing_prefix <- setdiff(same_data_prefix_fields, names(data_frame))
      if (length(missing_prefix) > 0) {
        stop(
          "The following 'same_data_prefix_fields' are missing in ",
          "'data_frame': ",
          paste(missing_prefix, collapse = ", "),
          call. = FALSE
        )
      }
    }

    if (!is.null(same_data_suffix_fields)) {

      if (!is.character(same_data_suffix_fields)) {
        stop(
          "'same_data_suffix_fields' must be NULL or a character vector.",
          call. = FALSE
        )
      }

      missing_suffix <- setdiff(same_data_suffix_fields, names(data_frame))
      if (length(missing_suffix) > 0) {
        stop(
          "The following 'same_data_suffix_fields' are missing in ",
          "'data_frame': ",
          paste(missing_suffix, collapse = ", "),
          call. = FALSE
        )
      }
    }
  }

  # DEFINE HELPER TO COLLAPSE A SINGLE ROW -------------------------------------

  collapse_row_fields <- function(row, fields, sep) {

    if (is.null(fields) || length(fields) == 0) {
      return("")
    }

    vals <- unlist(row[fields], use.names = FALSE)
    vals <- vals[!is.na(vals) & nzchar(vals)]

    paste(vals, collapse = sep)
  }

  # BUILD SPECIMEN STRINGS ROW BY ROW ------------------------------------------

  row_strings <- character(nrow(data_frame))

  for (i in seq_len(nrow(data_frame))) {

    current_row <- data_frame[i, , drop = FALSE]

    full_string <- collapse_row_fields(
      row = current_row,
      fields = fields_to_concat,
      sep = field_separator
    )

    # Always print the first row in full, and print all rows in full if
    # same-data compression is not enabled.
    if (!compress_same_data || i == 1) {
      row_strings[i] <- full_string
      next
    }

    previous_row <- data_frame[i - 1, , drop = FALSE]

    # Compare the current row with the immediately preceding row using the
    # nominated comparison fields.
    current_compare <- vapply(
      same_data_compare_fields,
      function(x) {
        val <- current_row[[x]]
        if (is.na(val)) "" else as.character(val)
      },
      character(1)
    )

    previous_compare <- vapply(
      same_data_compare_fields,
      function(x) {
        val <- previous_row[[x]]
        if (is.na(val)) "" else as.character(val)
      },
      character(1)
    )

    if (identical(current_compare, previous_compare)) {

      prefix_string <- collapse_row_fields(
        row = current_row,
        fields = same_data_prefix_fields,
        sep = field_separator
      )

      suffix_string <- collapse_row_fields(
        row = current_row,
        fields = same_data_suffix_fields,
        sep = field_separator
      )

      repeated_parts <- c(prefix_string, same_data_text, suffix_string)
      repeated_parts <- repeated_parts[nzchar(repeated_parts)]

      row_strings[i] <- paste(repeated_parts, collapse = field_separator)

    } else {
      row_strings[i] <- full_string
    }
  }

  # COLLAPSE ALL SPECIMEN STRINGS INTO FINAL OUTPUT ----------------------------

  paste0(
    paste(row_strings, collapse = specimen_separator),
    ". "
  )
}
