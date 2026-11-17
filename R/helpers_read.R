#' Read a CSV file using taxRmatic defaults
#'
#' Internal helper used by taxRmatic reader functions to standardise CSV import.
#' Files are read as character with empty strings preserved. Common invisible
#' characters are removed, wholly empty rows and unnamed empty columns are
#' discarded, and duplicate column names are made unique with a warning.
#' Named columns are retained even when all of their values are blank, because
#' some taxRmatic input formats require such columns to exist.
#'
#' @param file Path to the input CSV file.
#' @param fileEncoding Character string giving the file encoding.
#' @param message_label Data-specific label used in a success message after import.
#' @param duplicate_columns Whether to rename duplicates (default) or stop with an error.
#' @param ... Additional arguments passed to \code{utils::read.csv()}.
#' @return A data frame.
#' @keywords internal
read_taxRmatic_csv <- function(file,
                               fileEncoding = "UTF-8",
                               message_label = NULL,
                               duplicate_columns = "rename",
                               ...) {
  df <- utils::read.csv(
    file = file,
    fileEncoding = fileEncoding,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(0),
    strip.white = FALSE,
    stringsAsFactors = FALSE,
    ...
  )

  # Remove only known spreadsheet artefacts, not meaningful spaces, newlines
  # or typography in description text.
  invisible_chars <- paste0("[", intToUtf8(c(0xFEFF, 0x200B, 0x2060)), "]")
  clean_invisible <- function(x) {
    gsub(invisible_chars, "", x, perl = TRUE)
  }
  names(df) <- trimws(clean_invisible(names(df)), whitespace = "[\\h\\v]")
  df[] <- lapply(df, clean_invisible)

  # Treat Unicode whitespace-only cells as blank for the purpose of finding
  # phantom rows/columns. Preserve the original content of populated cells.
  blank_cell <- function(x) {
    is.na(x) | !nzchar(trimws(x, whitespace = "[\\h\\v]"))
  }

  # Remove rows that have no data anywhere, including between populated rows.
  empty_rows <- if (ncol(df) > 0L) {
    Reduce(`&`, lapply(df, blank_cell))
  } else {
    rep(TRUE, nrow(df))
  }
  n_removed_rows <- sum(empty_rows)
  if (n_removed_rows > 0L) {
    df <- df[!empty_rows, , drop = FALSE]
    rownames(df) <- NULL
  }

  # Only remove columns with BOTH a blank header and entirely blank values.
  # Retain named blank columns, such as optional taxRmatic metadata fields.
  empty_columns <- vapply(seq_along(df), function(i) {
    blank_cell(names(df)[i]) && all(blank_cell(df[[i]]))
  }, logical(1))
  n_removed_cols <- sum(empty_columns)
  if (n_removed_cols > 0L) {
    df <- df[, !empty_columns, drop = FALSE]
  }

  if (n_removed_rows > 0L || n_removed_cols > 0L) {
    message("CSV cleanup removed ", n_removed_rows, " empty row(s) and ",
            n_removed_cols, " unnamed empty column(s).")
  }

  # For specimen data, reject ambiguous names before automatic renaming.
  duplicate_columns <- match.arg(duplicate_columns, c("rename", "error"))
  if (duplicate_columns == "error") {
    check_df <- df
    names(check_df) <- tolower(names(check_df))
  }

  # Keep every column, but avoid ambiguous lookups of duplicate names.
  df <- make_taxRmatic_column_names_unique(df)

  if (!is.null(message_label)) {
    message(message_label, " file read successfully.")
  }

  df
}


#' Make duplicate and blank column names unique without losing data
#'
#' Blank names are replaced with position-based names. Duplicate names are
#' made unique, and a warning lists original and final names for all affected
#' columns (including the first occurrence of each duplicated name).
#'
#' @param df A data frame.
#' @return A data frame with unique, non-blank column names.
#' @keywords internal
make_taxRmatic_column_names_unique <- function(df) {
  original <- names(df)
  revised <- original

  blanks <- is.na(revised) | !nzchar(trimws(revised))
  revised[blanks] <- paste0("unnamed_column_", which(blanks))
  revised <- make.unique(revised, sep = ".")

  duplicates <- duplicated(original) |
    duplicated(original, fromLast = TRUE)
  affected <- which(blanks | duplicates | original != revised)

  if (length(affected) > 0L) {
    display_names <- function(x) {
      ifelse(is.na(x) | !nzchar(trimws(x)), "<blank>", paste0("'", x, "'"))
    }

    details <- paste0(
      "  Column ", affected, ": ", display_names(original[affected]),
      " -> '", revised[affected], "'"
    )

    warning(
      "Duplicate or blank column names detected:\n",
      paste(details, collapse = "\n"),
      "\nColumn names have been made unique where necessary.",
      "\nPlease check the source CSV if this was unintentional.",
      call. = FALSE
    )
  }

  names(df) <- revised
  df
}


#' Convert all column names to lowercase
#'
#' Also resolves case-only duplicates created during conversion.
#' @param df A data frame.
#' @param inform Logical. If \code{TRUE}, report conversion.
#' @return A data frame.
#' @keywords internal
lowercase_column_names <- function(df, inform = TRUE) {
  colnames(df) <- tolower(colnames(df))
  df <- make_taxRmatic_column_names_unique(df)

  if (inform) {
    message("Column names converted to lowercase.")
  }

  df
}


#' Check that required columns are present
#'
#' Internal validation helper used by reader functions to confirm that an input
#' data frame contains all mandatory columns. Stops with an informative error if
#' any required columns are missing.
#'
#' @param df A data frame.
#' @param required_cols Character vector of required column names.
#' @param function_name Optional function name to include in the error message.
#' @param inform Logical. If \code{TRUE}, report success with a message.
#'
#' @return Invisibly returns \code{TRUE} if all required columns are present.
#' @keywords internal
check_required_columns <- function(df,
                                   required_cols,
                                   function_name = NULL,
                                   inform = TRUE) {
  missing_cols <- setdiff(required_cols, colnames(df))

  if (length(missing_cols) > 0) {
    msg <- paste0(
      "The following required columns are missing from the input file: ",
      paste(missing_cols, collapse = ", "),
      "."
    )

    if (!is.null(function_name)) {
      msg <- paste0(
        msg, "\n",
        "These columns are required for ", function_name, "()."
      )
    }

    stop(msg, call. = FALSE)
  }

  if (inform) {
    message(
      "All required columns are present: ",
      paste(required_cols, collapse = ", "),
      "."
    )
  }

  invisible(TRUE)
}


#' Trim whitespace from selected columns
#'
#' Internal helper used by reader functions to trim leading and trailing
#' whitespace from selected columns, and optionally convert selected columns to
#' lowercase afterwards.
#'
#' @param df A data frame.
#' @param cols Character vector of column names to trim.
#' @param tolower_cols Optional character vector of column names to convert to
#'   lowercase after trimming.
#' @param inform Logical. If \code{TRUE}, report which columns were trimmed.
#'
#' @return A data frame.
#' @keywords internal
trim_columns <- function(df, cols, tolower_cols = NULL, inform = FALSE) {
  cols <- intersect(cols, colnames(df))

  for (col in cols) {
    df[[col]] <- trimws(df[[col]])
  }

  if (!is.null(tolower_cols)) {
    tolower_cols <- intersect(tolower_cols, colnames(df))

    for (col in tolower_cols) {
      df[[col]] <- tolower(df[[col]])
    }
  }

  if (inform && length(cols) > 0) {
    message(
      "Whitespace trimmed from columns: ",
      paste(cols, collapse = ", "),
      "."
    )
  }

  df
}


#' Check that selected columns are non-empty
#'
#' Internal validation helper used to ensure that specified fields contain
#' non-empty values in every row of a data frame. Empty strings and \code{NA}
#' values are both treated as missing.
#'
#' @param df A data frame.
#' @param cols Character vector of column names to check.
#' @param subject Character string used in messages, e.g. \code{"Rows"} or
#'   \code{"Specimens"}.
#' @param call. Logical. Passed to \code{base::stop()}.
#'
#' @return Invisibly returns \code{TRUE} if all specified columns are non-empty.
#' @keywords internal
check_non_empty_columns <- function(df,
                                    cols,
                                    subject = "Rows",
                                    call. = FALSE) {
  for (col in cols) {
    bad_rows <- which(is_missing_or_empty(df[[col]]))

    if (length(bad_rows) > 0) {
      stop(
        subject, " contain missing or empty values in column '", col,
        "' at row(s) ", paste(bad_rows, collapse = ", "), ".\n",
        "All ", tolower(subject), " must have non-empty values for ",
        paste(shQuote(cols), collapse = ", "),
        ".",
        call. = call.
      )
    }
  }

  message(
    "All ", tolower(subject), " have non-empty values for the required fields: ",
    paste(cols, collapse = ", "),
    "."
  )

  invisible(TRUE)
}

#' Test whether values are missing or empty
#'
#' Internal helper to identify missing or empty values in vectors or
#' list-columns. For list-columns, elements of length zero are treated as empty.
#'
#' @param x Vector or list-column.
#'
#' @return Logical vector.
#' @keywords internal
is_missing_or_empty <- function(x) {
  if (is.list(x)) {
    vapply(x, function(el) {

      # NULL or length 0 means empty.
      if (is.null(el) || length(el) == 0) {
        return(TRUE)
      }

      # Flatten and check contents.
      el <- unlist(el, use.names = FALSE)

      # All NA or empty strings means empty.
      all(is.na(el) | !nzchar(trimws(as.character(el))))

    }, logical(1))

  } else {
    is.na(x) | !nzchar(trimws(as.character(x)))
  }
}
