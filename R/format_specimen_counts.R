#' Format Specimen Category Counts into Natural-Language Strings
#'
#' Convert specimen count columns in a data frame into natural-language strings
#' suitable for taxonomic manuscripts.
#'
#' For each row, the function reports only those specimen categories with
#' positive, non-missing counts, using user-defined singular and plural labels
#' (e.g. `"male"` versus `"males"`), and joins the resulting parts with a
#' customisable separator.
#'
#' @param df A data frame containing one column per specimen category. Relevant
#'   columns should contain non-negative integers, `NA`, or integer-like
#'   character values (e.g. `"1"`, `"2"`), which will be coerced with a warning.
#' @param specimen_category_labels A named list specifying the singular and
#'   plural labels for each specimen category. Each name must match a column in
#'   `df`, and each element must be a character vector of length 2 in the form
#'   `c("singular", "plural")`.
#' @param separator A character string used to join category phrases in the
#'   output. Defaults to `", "`.
#'
#' @details
#' For each row, the function iterates over the specimen categories listed in
#' `specimen_category_labels`. Counts of `0` or `NA` are omitted. Positive
#' counts are converted into strings such as `"1 male"` or `"3 females"`, with
#' the singular or plural form selected automatically.
#'
#' If a row contains no positive, non-missing counts, the output for that row
#' is an empty string.
#'
#' @return A character vector of length `nrow(df)`, with one formatted specimen
#'   count string per row.
#'
#' @examples
#' # Default specimen categories
#' df <- data.frame(
#' males = c(1, 0, 2),
#' females = c(0, 3, NA),
#' juveniles = c(1, 0, 4)
#' )
#'
#' format_specimen_counts(df)
#' # [1] "1 male, 1 juvenile"
#' # [2] "3 females"
#' # [3] "2 males, 4 juveniles"
#'
#' # Custom specimen categories and singular/plural wording
#' df2 <- data.frame(
#' adult_males = c(1, 0, 2),
#' adult_females = c(0, 1, 1),
#' juveniles = c(2, 3, 0)
#' )
#'
#' format_specimen_counts(
#' df = df2,
#' specimen_category_labels = list(
#' adult_males = c("adult male", "adult males"),
#' adult_females = c("adult female", "adult females"),
#' juveniles = c("juvenile", "juveniles")
#' )
#' )
#' # [1] "1 adult male, 2 juveniles"
#' # [2] "1 adult female, 3 juveniles"
#' # [3] "2 adult males, 1 adult female"
#' @export
format_specimen_counts <- function(
    df,
    specimen_category_labels = list(
      males = c("male", "males"),
      females = c("female", "females"),
      juveniles = c("juvenile", "juveniles")
    ),
    separator = ", "
) {

  # VALIDATION -----------------------------------------------------------------

  if (!is.data.frame(df)) {
    stop("'df' must be a data frame.", call. = FALSE)
  }

  if (!is.list(specimen_category_labels) ||
      is.null(names(specimen_category_labels)) ||
      any(names(specimen_category_labels) == "")) {
    stop(
      "'specimen_category_labels' must be a named list.",
      call. = FALSE
    )
  }

  invalid_labels <- !vapply(
    specimen_category_labels,
    function(x) is.character(x) && length(x) == 2,
    logical(1)
  )

  if (any(invalid_labels)) {
    stop(
      "Each element of 'specimen_category_labels' must be a character vector ",
      "of length 2 in the form c('singular', 'plural').",
      call. = FALSE
    )
  }

  if (!is.character(separator) || length(separator) != 1 || is.na(separator)) {
    stop("'separator' must be a single character string.", call. = FALSE)
  }

  # CHECK COLUMNS ARE PRESENT --------------------------------------------------

  count_columns <- names(specimen_category_labels)

  missing_cols <- setdiff(count_columns, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "The following count columns are missing from 'df': ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # VALIDATE AND COERCE COUNT COLUMNS TO NUMERIC -------------------------------

  for (col in count_columns) {

    values <- df[[col]]

    if (is.character(values)) {

      trimmed <- trimws(values)
      trimmed[trimmed == ""] <- NA_character_

      is_integer_like <- grepl("^[0-9]+$", trimmed[!is.na(trimmed)])

      if (!all(is_integer_like)) {
        stop(
          "Column '", col, "' contains non-integer-like character values.",
          call. = FALSE
        )
      }

      values <- as.numeric(trimmed)

      warning(
        "Column '", col, "' was coerced from character to numeric.",
        call. = FALSE
      )
    }

    if (!is.numeric(values)) {
      stop(
        "Column '", col, "' must be numeric or integer-like character.",
        call. = FALSE
      )
    }

    non_missing <- values[!is.na(values)]

    if (any(non_missing < 0)) {
      stop(
        "Column '", col, "' contains negative values.",
        call. = FALSE
      )
    }

    if (any(non_missing != floor(non_missing))) {
      stop(
        "Column '", col, "' must contain whole numbers only.",
        call. = FALSE
      )
    }

    df[[col]] <- values
  }

  # FORMAT ---------------------------------------------------------------------

  specimen_count_strings <- character(nrow(df))

  for (i in seq_len(nrow(df))) {
    row_parts <- character(0)

    for (col in count_columns) {
      count <- df[[col]][i]

      if (!is.na(count) && count > 0) {
        label <- if (count == 1) {
          specimen_category_labels[[col]][1]
        } else {
          specimen_category_labels[[col]][2]
        }

        row_parts <- c(row_parts, paste(count, label))
      }
    }

    specimen_count_strings[i] <- paste(row_parts, collapse = separator)
  }

  specimen_count_strings
}
