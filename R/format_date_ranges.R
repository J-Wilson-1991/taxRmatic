################################################################################
# Helper function: parse partial dates
################################################################################

#' Parse partial date strings
#'
#' Internal helper used to parse date strings that may contain incomplete date
#' information, such as full dates, month-year combinations, or year-only
#' values.
#'
#' The function interprets the input according to a user-supplied format string
#' and returns numeric day, month, and year components, using `NA` for missing
#' parts.
#'
#' Supported inputs include formats with separators (e.g. `"%d/%m/%Y"`) as well
#' as year-only formats with no separator.
#'
#' @param date_str A character string representing a date, which may be partial.
#' @param format_str A format string using `strptime`-style directives, for
#'   example `"%d/%m/%Y"`, `"%m/%Y"`, or `"%Y"`.
#'
#' @return A list with elements:
#'   \itemize{
#'     \item `day`: integer day value, or `NA`
#'     \item `month`: integer month value, or `NA`
#'     \item `year`: integer year value, or `NA`
#'   }
#'
#' @keywords internal
#' @noRd
parse_partial_date <- function(date_str, format_str) {

  # HANDLE EMPTY INPUT ---------------------------------------------------------

  if (is.na(date_str) || trimws(date_str) == "") {
    return(
      list(
        day = NA_integer_,
        month = NA_integer_,
        year = NA_integer_
      )
    )
  }

  date_str <- trimws(date_str)

  # IDENTIFY SEPARATOR ---------------------------------------------------------

  # Extract the first non-format character used as a separator in the format
  # string. If no separator is present, treat the input as a year-only value.
  sep <- regmatches(format_str, regexpr("[^%a-zA-Z]", format_str))
  sep <- if (length(sep) == 0) "" else sep

  if (sep == "") {

    year_val <- suppressWarnings(as.integer(date_str))

    if (is.na(year_val)) {
      stop(
        "Could not parse partial date '", date_str,
        "' using format '", format_str, "'.",
        call. = FALSE
      )
    }

    return(
      list(
        day = NA_integer_,
        month = NA_integer_,
        year = year_val
      )
    )
  }

  # SPLIT FORMAT AND DATE COMPONENTS -------------------------------------------

  format_parts <- strsplit(format_str, sep, fixed = TRUE)[[1]]
  date_parts   <- strsplit(date_str, sep, fixed = TRUE)[[1]]

  if (length(date_parts) > length(format_parts)) {
    stop(
      "Could not parse partial date '", date_str,
      "' using format '", format_str, "'.",
      call. = FALSE
    )
  }

  # If a partial date is supplied, use only the rightmost components of the
  # format string. For example, "03/2024" with "%d/%m/%Y" is interpreted as
  # "%m/%Y".
  format_used <- utils::tail(format_parts, length(date_parts))

  date_info <- list(day = NA_integer_, month = NA_integer_, year = NA_integer_)

  # PARSE EACH COMPONENT -------------------------------------------------------

  for (i in seq_along(date_parts)) {

    fmt <- format_used[i]
    val <- trimws(date_parts[[i]])

    if (fmt %in% c("%d", "%e")) {

      parsed_val <- suppressWarnings(as.integer(val))
      if (is.na(parsed_val)) {
        stop(
          "Could not parse day value '", val, "' in date '", date_str, "'.",
          call. = FALSE
        )
      }
      date_info$day <- parsed_val

    } else if (fmt == "%m") {

      parsed_val <- suppressWarnings(as.integer(val))
      if (is.na(parsed_val)) {
        stop(
          "Could not parse month value '", val, "' in date '", date_str, "'.",
          call. = FALSE
        )
      }
      date_info$month <- parsed_val

    } else if (fmt == "%b") {

      parsed_val <- match(tolower(val), tolower(month.abb))
      if (is.na(parsed_val)) {
        stop(
          "Could not parse abbreviated month '", val, "' in date '", date_str, "'.",
          call. = FALSE
        )
      }
      date_info$month <- parsed_val

    } else if (fmt == "%B") {

      parsed_val <- match(tolower(val), tolower(month.name))
      if (is.na(parsed_val)) {
        stop(
          "Could not parse month name '", val, "' in date '", date_str, "'.",
          call. = FALSE
        )
      }
      date_info$month <- parsed_val

    } else if (fmt == "%Y") {

      parsed_val <- suppressWarnings(as.integer(val))
      if (is.na(parsed_val)) {
        stop(
          "Could not parse year value '", val, "' in date '", date_str, "'.",
          call. = FALSE
        )
      }
      date_info$year <- parsed_val
    }
  }

  # BASIC RANGE CHECKS ---------------------------------------------------------

  if (!is.na(date_info$month) &&
      (date_info$month < 1 || date_info$month > 12)) {
    stop(
      "Month value ", date_info$month, " is out of range in date '", date_str, "'.",
      call. = FALSE
    )
  }

  if (!is.na(date_info$day) &&
      (date_info$day < 1 || date_info$day > 31)) {
    stop(
      "Day value ", date_info$day, " is out of range in date '", date_str, "'.",
      call. = FALSE
    )
  }

  date_info
}


################################################################################
# Helper function: format parsed date components
################################################################################

#' Format parsed date components
#'
#' Internal helper used to convert day, month, and year components into a
#' formatted date string.
#'
#' The function supports full dates, month-year combinations, and year-only
#' values. Any components supplied as `NULL` in the output format arguments are
#' omitted from the output.
#'
#' Where abbreviated month names are used via `"%b"`, a period can optionally be
#' appended (e.g. `"Sep."` rather than `"Sep"`).
#'
#' @param day Integer day value, or `NA`.
#' @param month Integer month value, or `NA`.
#' @param year Integer year value, or `NA`.
#' @param output_day_format Output format for the day component. Defaults to
#'   `"%d"`. Set to `NULL` to omit the day.
#' @param output_month_format Output format for the month component. Defaults to
#'   `"%B"`. Set to `NULL` to omit the month.
#' @param output_year_format Output format for the year component. Defaults to
#'   `"%Y"`. Set to `NULL` to omit the year.
#' @param append_period Logical. If `TRUE`, append a period to abbreviated month
#'   names produced via `"%b"`.
#'
#' @return A character string containing the formatted date. Returns `""` if the
#'   supplied date components cannot be formatted into a supported partial date.
#'
#' @keywords internal
#' @noRd
format_date <- function(day, month, year,
                        output_day_format = "%d",
                        output_month_format = "%B",
                        output_year_format = "%Y",
                        append_period = FALSE) {

  # BUILD OUTPUT FORMAT STRING -------------------------------------------------

  format_parts <- c()

  if (!is.null(output_day_format)) {
    format_parts <- c(format_parts, output_day_format)
  }

  if (!is.null(output_month_format)) {
    format_parts <- c(format_parts, output_month_format)
  }

  if (!is.null(output_year_format)) {
    format_parts <- c(format_parts, output_year_format)
  }

  format_str <- paste(format_parts, collapse = " ")

  # FORMAT BASED ON AVAILABLE COMPONENTS ---------------------------------------

  if (!is.na(day) && !is.na(month) && !is.na(year)) {

    date_obj <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
    out <- format(date_obj, format_str)

  } else if (is.na(day) && !is.na(month) && !is.na(year)) {

    date_obj <- as.Date(sprintf("%04d-%02d-01", year, month))
    out <- format(date_obj, format_str)

  } else if (is.na(day) && is.na(month) && !is.na(year)) {

    date_obj <- as.Date(sprintf("%04d-01-01", year))
    out <- format(date_obj, format_str)

  } else {

    out <- ""
  }

  # OPTIONALLY APPEND PERIOD TO ABBREVIATED MONTHS -----------------------------

  if (append_period &&
      !is.null(output_month_format) &&
      grepl("%b", output_month_format, fixed = TRUE) &&
      !is.na(month)) {
    out <- sub("\\b(Mar|Apr|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\b", "\\1.", out)
  }

  out
}

################################################################################
# MAIN FUNCTION
################################################################################
#
#' Format date ranges
#'
#' Formats dates from a specimen data table into readable date strings, and
#' combines to and from dates into ranges. This is useful for preparing dates for
#' specimen lists, material examined sections, or other taxonomic text. Handles
#' partial dates e.g. (12/01/2025, 01/2025, 2025).
#'
#' The function works row by row. For each row of the data frame, it returns one
#' formatted date or date range.
#'
#' Dates can be supplied in either of two ways:
#'
#' \itemize{
#'   \item as separate day, month, and year columns
#'   \item as one or two text columns containing dates
#' }
#'
#' For example, separate date columns might be named `start_day`,
#' `start_month`, and `start_year`. A text date column might contain values such
#' as `12/01/2024`, `03/2024`, or `2024`.
#'
#' Start dates are required at the column level: you must tell the function where
#' to find the start date information. However, individual rows may have missing
#' start-date values; those rows return an empty string.
#'
#' End dates are optional. If end-date columns are supplied, the function formats
#' the result as a date range where possible. For example, a row with a start
#' date of January 2024 and an end date of March 2024 can be formatted as a date
#' range.
#'
#' Partial dates are supported. A date can be:
#'
#' \itemize{
#'   \item a full date, with day, month, and year
#'   \item a month and year only
#'   \item a year only
#' }
#'
#' If using separate day, month, and year columns, a year column is required.
#' A day column can only be used if a month column is also supplied.
#'
#' If using text date columns, use `input_date_format` to tell the function how
#' the dates are written. The default is `"%d/%m/%y"`, for dates such as
#' `12/01/24`. Use `"%d/%m/%Y"` for dates with a four-digit year, such as
#' `12/01/2024`.
#'
#' Do not mix the two input styles for the same start or end date. For example,
#' do not supply both `start_date_col` and `start_day_col`.
#'
#' If `compact_range_formatting = TRUE`, repeated date information is left out
#' of ranges where the function can do so. For example, a month range within the
#' same year may be written as `January-March 2024` rather than
#' `January 2024-March 2024`.
#'
#' If a date cannot be understood, the function stops and reports the row where
#' the problem occurred.
#'
#' @param df Data frame containing the date columns.
#' @param start_day_col,start_month_col,start_year_col Optional column names for
#'   the start day, start month, and start year.
#' @param end_day_col,end_month_col,end_year_col Optional column names for the
#'   end day, end month, and end year.
#' @param start_date_col Optional column name for a text column containing the
#'   start date.
#' @param end_date_col Optional column name for a text column containing the end
#'   date.
#' @param input_date_format Format used to read dates from `start_date_col` and
#'   `end_date_col`. Defaults to `"%d/%m/%y"`, for dates such as `12/01/24`.
#'   Use `"%d/%m/%Y"` for dates such as `12/01/2024`.
#' @param output_day_format Format used for days in the output. Defaults to
#'   `"%d"`.
#' @param output_month_format Format used for months in the output. Defaults to
#'   `"%B"`, which gives full month names such as `January`.
#' @param output_year_format Format used for years in the output. Defaults to
#'   `"%Y"`, which gives four-digit years such as `2024`.
#' @param range_separator Text used between the start and end dates in a range.
#'   Defaults to an en dash.
#' @param compact_range_formatting Logical. If `TRUE`, repeated date information
#'   is left out of date ranges where possible.
#' @param append_period_to_abbrev_month Logical. If `TRUE`, add a period after
#'   abbreviated month names, such as `Jan.` instead of `Jan`.
#'
#' @return A character vector with one formatted date or date range for each row
#'   of `df`.
#'
#' @examples
#' # Example 1: dates stored in separate day, month, and year columns
#' df <- data.frame(
#'   start_day = c(12, NA, NA),
#'   start_month = c(1, 3, NA),
#'   start_year = c(2024, 2024, 2024),
#'   end_day = c(15, NA, NA),
#'   end_month = c(1, 5, NA),
#'   end_year = c(2024, 2024, 2025)
#' )
#'
#' format_date_ranges(
#'   df = df,
#'   start_day_col = "start_day",
#'   start_month_col = "start_month",
#'   start_year_col = "start_year",
#'   end_day_col = "end_day",
#'   end_month_col = "end_month",
#'   end_year_col = "end_year"
#' )
#'
#' # Example 2: dates stored as text in one start column and one end column
#' df2 <- data.frame(
#'   start = c("12/01/2024", "03/2024", "2024"),
#'   end = c("15/01/2024", "05/2024", "2025")
#' )
#'
#' format_date_ranges(
#'   df = df2,
#'   start_date_col = "start",
#'   end_date_col = "end",
#'   input_date_format = "%d/%m/%Y"
#' )
#'
#' @export
format_date_ranges <- function(df,
                               start_day_col = NULL, start_month_col = NULL, start_year_col = NULL,
                               end_day_col = NULL, end_month_col = NULL, end_year_col = NULL,
                               start_date_col = NULL, end_date_col = NULL,
                               input_date_format = "%d/%m/%y",
                               output_day_format = "%d",
                               output_month_format = "%B",
                               output_year_format = "%Y",
                               range_separator = "\u2013",
                               compact_range_formatting = TRUE,
                               append_period_to_abbrev_month = FALSE) {

  # CHECK INPUTS ----------------------------------------------------------------

  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  if (!is.character(input_date_format) ||
      length(input_date_format) != 1 ||
      is.na(input_date_format)) {
    stop("`input_date_format` must be a single character string.", call. = FALSE)
  }

  if (!is.character(range_separator) ||
      length(range_separator) != 1 ||
      is.na(range_separator)) {
    stop("`range_separator` must be a single character string.", call. = FALSE)
  }

  if (!is.logical(compact_range_formatting) ||
      length(compact_range_formatting) != 1 ||
      is.na(compact_range_formatting)) {
    stop("`compact_range_formatting` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(append_period_to_abbrev_month) ||
      length(append_period_to_abbrev_month) != 1 ||
      is.na(append_period_to_abbrev_month)) {
    stop("`append_period_to_abbrev_month` must be TRUE or FALSE.", call. = FALSE)
  }

  # CHECK COLUMN REFERENCES -----------------------------------------------------

  all_col_args <- c(
    start_day_col, start_month_col, start_year_col,
    end_day_col, end_month_col, end_year_col,
    start_date_col, end_date_col
  )

  non_null_cols <- all_col_args[!vapply(all_col_args, is.null, logical(1))]

  if (length(non_null_cols) > 0) {

    if (!all(vapply(non_null_cols, function(x) is.character(x) && length(x) == 1 && !is.na(x), logical(1)))) {
      stop("All supplied column arguments must be single character strings.", call. = FALSE)
    }

    missing_cols <- setdiff(non_null_cols, names(df))

    if (length(missing_cols) > 0) {
      stop(
        "The following columns were not found in `df`: ",
        paste(missing_cols, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  # CHECK INPUT STYLE -----------------------------------------------------------

  using_start_text <- !is.null(start_date_col)
  using_end_text   <- !is.null(end_date_col)

  using_start_parts <- !all(vapply(
    list(start_day_col, start_month_col, start_year_col),
    is.null,
    logical(1)
  ))

  using_end_parts <- !all(vapply(
    list(end_day_col, end_month_col, end_year_col),
    is.null,
    logical(1)
  ))

  if (using_start_text && using_start_parts) {
    stop(
      "Start dates must be supplied either via `start_date_col` or via ",
      "separate start day/month/year columns, not both.",
      call. = FALSE
    )
  }

  if (using_end_text && using_end_parts) {
    stop(
      "End dates must be supplied either via `end_date_col` or via separate ",
      "end day/month/year columns, not both.",
      call. = FALSE
    )
  }

  if (!using_start_text && !using_start_parts) {
    stop(
      "You must supply start dates either via `start_date_col` or via at least ",
      "one of `start_day_col`, `start_month_col`, or `start_year_col`.",
      call. = FALSE
    )
  }

  # CHECK DATE PART COLUMN COMPLETENESS -----------------------------------------

  # When using separate columns, require at least a year column. Dates without a
  # year are not supported by the downstream formatting logic.
  if (using_start_parts && is.null(start_year_col)) {
    stop(
      "When supplying start dates via separate columns, `start_year_col` must be provided.",
      call. = FALSE
    )
  }

  if (using_end_parts && !is.null(end_day_col) && is.null(end_month_col)) {
    stop(
      "If `end_day_col` is supplied, `end_month_col` must also be supplied.",
      call. = FALSE
    )
  }

  if (using_start_parts && !is.null(start_day_col) && is.null(start_month_col)) {
    stop(
      "If `start_day_col` is supplied, `start_month_col` must also be supplied.",
      call. = FALSE
    )
  }

  # FORMAT EACH ROW -------------------------------------------------------------

  n <- nrow(df)

  res <- vapply(
    seq_len(n),
    FUN = function(i) {

      tryCatch({

        row <- df[i, , drop = FALSE]

        # EXTRACT START DATE COMPONENTS -----------------------------------------

        if (!is.null(start_date_col)) {

          parsed_start <- parse_partial_date(
            as.character(row[[start_date_col]]),
            input_date_format
          )

          start_day   <- parsed_start$day
          start_month <- parsed_start$month
          start_year  <- parsed_start$year

        } else {

          start_day <- if (!is.null(start_day_col)) {
            suppressWarnings(as.integer(row[[start_day_col]]))
          } else {
            NA_integer_
          }

          start_month <- if (!is.null(start_month_col)) {
            suppressWarnings(as.integer(row[[start_month_col]]))
          } else {
            NA_integer_
          }

          start_year <- if (!is.null(start_year_col)) {
            suppressWarnings(as.integer(row[[start_year_col]]))
          } else {
            NA_integer_
          }
        }

        # EXTRACT END DATE COMPONENTS -------------------------------------------

        if (!is.null(end_date_col)) {

          parsed_end <- parse_partial_date(
            as.character(row[[end_date_col]]),
            input_date_format
          )

          end_day   <- parsed_end$day
          end_month <- parsed_end$month
          end_year  <- parsed_end$year

        } else {

          end_day <- if (!is.null(end_day_col)) {
            suppressWarnings(as.integer(row[[end_day_col]]))
          } else {
            NA_integer_
          }

          end_month <- if (!is.null(end_month_col)) {
            suppressWarnings(as.integer(row[[end_month_col]]))
          } else {
            NA_integer_
          }

          end_year <- if (!is.null(end_year_col)) {
            suppressWarnings(as.integer(row[[end_year_col]]))
          } else {
            NA_integer_
          }
        }

        # FORMAT FULL DATES -----------------------------------------------------

        if (all(!is.na(c(start_day, start_month, start_year)))) {

          # START DATE ONLY -----------------------------------------------------

          if (all(is.na(c(end_day, end_month, end_year)))) {

            out <- format_date(
              start_day, start_month, start_year,
              output_day_format, output_month_format, output_year_format,
              append_period_to_abbrev_month
            )

            # RANGE WITH SAME MONTH AND YEAR --------------------------------------

          } else if (!is.na(end_day) &&
                     !is.na(end_month) &&
                     !is.na(end_year) &&
                     start_month == end_month &&
                     start_year == end_year &&
                     compact_range_formatting) {

            out <- paste0(
              trimws(
                format_date(
                  start_day, start_month, start_year,
                  output_day_format, output_month_format, output_year_format,
                  append_period_to_abbrev_month
                ),
                "right"
              ),
              range_separator,
              trimws(
                format_date(
                  end_day, end_month, end_year,
                  output_day_format, output_month_format, output_year_format,
                  append_period_to_abbrev_month
                ),
                "left"
              )
            )

            # RANGE WITH SAME YEAR ------------------------------------------------

          } else if (!is.na(end_day) &&
                     !is.na(end_month) &&
                     !is.na(end_year) &&
                     start_year == end_year &&
                     compact_range_formatting) {

            out <- paste0(
              trimws(
                format_date(
                  start_day, start_month, start_year,
                  output_day_format, output_month_format, NULL,
                  append_period_to_abbrev_month
                ),
                "right"
              ),
              range_separator,
              trimws(
                format_date(
                  end_day, end_month, end_year,
                  output_day_format, output_month_format, output_year_format,
                  append_period_to_abbrev_month
                ),
                "left"
              )
            )

            # FULL RANGE ----------------------------------------------------------

          } else if (all(!is.na(c(end_day, end_month, end_year)))) {

            out <- paste0(
              trimws(
                format_date(
                  start_day, start_month, start_year,
                  output_day_format, output_month_format, output_year_format,
                  append_period_to_abbrev_month
                ),
                "right"
              ),
              range_separator,
              trimws(
                format_date(
                  end_day, end_month, end_year,
                  output_day_format, output_month_format, output_year_format,
                  append_period_to_abbrev_month
                ),
                "left"
              )
            )

          } else {
            stop(
              "End date must be either fully specified or fully absent when start date is a full date.",
              call. = FALSE
            )
          }

          # FORMAT MONTH-YEAR DATES -----------------------------------------------

        } else if (is.na(start_day) && !is.na(start_month) && !is.na(start_year)) {

          # START DATE ONLY -----------------------------------------------------

          if (all(is.na(c(end_month, end_year)))) {

            out <- format_date(
              NA, start_month, start_year,
              NULL, output_month_format, output_year_format,
              append_period_to_abbrev_month
            )

            # RANGE WITH SAME YEAR ------------------------------------------------

          } else if (is.na(end_day) &&
                     !is.na(end_month) &&
                     !is.na(end_year) &&
                     start_year == end_year &&
                     compact_range_formatting) {

            out <- paste0(
              trimws(
                format_date(
                  NA, start_month, start_year,
                  NULL, output_month_format, NULL,
                  append_period_to_abbrev_month
                ),
                "right"
              ),
              range_separator,
              trimws(
                format_date(
                  NA, end_month, end_year,
                  NULL, output_month_format, output_year_format,
                  append_period_to_abbrev_month
                ),
                "left"
              )
            )

            # FULL RANGE ----------------------------------------------------------

          } else if (is.na(end_day) && !is.na(end_month) && !is.na(end_year)) {

            out <- paste0(
              trimws(
                format_date(
                  NA, start_month, start_year,
                  NULL, output_month_format, output_year_format,
                  append_period_to_abbrev_month
                ),
                "right"
              ),
              range_separator,
              trimws(
                format_date(
                  NA, end_month, end_year,
                  NULL, output_month_format, output_year_format,
                  append_period_to_abbrev_month
                ),
                "left"
              )
            )

          } else {
            stop(
              "End date must be month-year, year-only, or absent when start date is month-year.",
              call. = FALSE
            )
          }

          # FORMAT YEAR-ONLY DATES -----------------------------------------------

        } else if (is.na(start_day) && is.na(start_month) && !is.na(start_year)) {

          out <- format_date(
            NA, NA, start_year,
            NULL, NULL, output_year_format
          )

          if (!is.na(end_year)) {

            if (!is.na(end_day) || !is.na(end_month)) {
              stop(
                "End date cannot include day or month when start date is year-only.",
                call. = FALSE
              )
            }

            out <- paste0(
              trimws(out, "right"),
              range_separator,
              trimws(
                format_date(
                  NA, NA, end_year,
                  NULL, NULL, output_year_format
                ),
                "left"
              )
            )
          }

          # NO VALID START DATE ---------------------------------------------------

        } else if (all(is.na(c(start_day, start_month, start_year)))) {

          out <- ""

        } else {
          stop(
            "Start date does not represent a supported combination of day, month, and year values.",
            call. = FALSE
          )
        }

        out

      }, error = function(e) {

        stop(
          sprintf(
            "Row %d does not conform to the requirements of format_date_ranges(): %s",
            i, e$message
          ),
          call. = FALSE
        )

      })
    },
    FUN.VALUE = character(1)
  )

  res
}
