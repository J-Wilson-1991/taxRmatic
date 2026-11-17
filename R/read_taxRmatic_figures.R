#' Read a taxRmatic figures table from CSV
#'
#' Reads a CSV file listing figures and their identifying information (such
#' as taxon, specimen code, view and anatomical feature). taxRmatic uses the
#' table to add matching figure references and generate captions. The CSV
#' records figures, but does not need to contain the image files themselves.
#'
#' @details
#' Expected input structure
#'
#' The input file must be a CSV in which each row represents one figure record.
#' A figure record may correspond to a whole figure or to one labelled figure
#' part.
#'
#' Additional metadata columns may be included. These are retained in the output
#' and can be used for figure filtering, including through \code{figure_filter}
#' strings in a taxRmatic character-state matrix and through \code{fg{}}
#' tags used by \code{replace_figure_tags()}.
#'
#' Column names are converted to lowercase during import. Figure-filter strings
#' should therefore refer to lowercase column names.
#'
#' File encoding
#'
#' Input CSV files should be saved using UTF-8 encoding. This helps preserve
#' special characters such as accented letters, degree symbols, en dashes, Greek
#' letters, and other characters that may occur in taxonomic names, locality
#' data, specimen notes, captions, or other figure metadata.
#'
#' By default, this function reads files using \code{fileEncoding = "UTF-8"}.
#' In spreadsheet software such as Microsoft Excel, users should usually save
#' the file as CSV UTF-8 (Comma delimited) rather than as the older default CSV
#' format.
#'
#' Mandatory columns
#'
#' The following columns are mandatory:
#'
#' \itemize{
#'   \item \code{figure_number}: identifies the figure number. This must be
#'     filled for all rows.
#'   \item \code{figure_part}: identifies the figure part, where relevant. This
#'     column must exist, but values may be left blank for figures without
#'     labelled parts. Within a given \code{figure_number}, use of
#'     \code{figure_part} must be consistent: either all records for that figure
#'     number should have a \code{figure_part} value, or all should leave
#'     \code{figure_part} blank.
#'   \item \code{taxon}: identifies the taxon associated with the figure record.
#'     This column must exist, but values may be left blank for records that are
#'     not taxon-specific. Taxon values can be used for taxon-based filtering,
#'     figure captions, and user-supplied figure filters, but are not required
#'     for specimen-code-based matching in description-generating functions.
#'   \item \code{specimen_code}: contains one or more specimen codes associated
#'     with the figure record. Multiple specimen codes should be separated by
#'     commas. This column must exist, but values may be left blank for records
#'     that are not specimen-specific. Specimen codes are used to link figures to
#'     description rows in functions such as \code{get_taxRmatic_descriptions()}.
#' }
#'
#' Each \code{figure_number} + \code{figure_part} combination must be unique.
#' For figures without labelled parts, uniqueness is checked using the
#' \code{figure_number} and a blank \code{figure_part} value.
#'
#' Multi-value fields
#'
#' The \code{specimen_code} column is always split automatically on commas and
#' stored as a list-column. This keeps specimen-code handling consistent with
#' \code{read_taxRmatic_character_state_matrix()}.
#'
#' Other metadata columns can be split into multi-value list-columns using
#' \code{split_columns}. This is useful for fields such as \code{taxon},
#' \code{feature}, \code{view}, \code{sex}, \code{life_stage}, or other
#' project-specific metadata fields.
#'
#' If \code{taxon} is supplied in \code{split_columns}, it will be split using
#' \code{split_char}, allowing a single figure record to be associated with
#' multiple taxa.
#'
#' The structural columns \code{figure_number}, \code{figure_part}, and
#' \code{specimen_code} cannot be split using \code{split_columns};
#' \code{specimen_code} is handled automatically.
#'
#' Behaviour and validation
#'
#' During import, the function:
#'
#' \itemize{
#'   \item reads all columns as character
#'   \item converts column names to lowercase
#'   \item trims leading and trailing whitespace from character values
#'   \item checks mandatory columns
#'   \item checks that all rows have a \code{figure_number} value
#'   \item checks that \code{figure_part} is used consistently within each
#'     \code{figure_number}
#'   \item checks that each \code{figure_number} + \code{figure_part}
#'     combination is unique
#'   \item splits \code{specimen_code} on commas into a list-column
#'   \item optionally splits selected metadata columns into list-columns using
#'     \code{split_columns} and \code{split_char}
#'   \item reports records that lack \code{specimen_code} or \code{taxon}
#'     values, because those records may not be available for some downstream
#'     matching or filtering workflows
#' }
#'
#' @section Using the figure table:
#' This CSV is a list of **figure metadata**, not a collection of image files. Enter one row for each entire figure or each lettered part, giving its `figure_number` and, when applicable, `figure_part`. The `taxon` and `specimen_code` columns are required to exist but may be blank for a map or other overview figure.
#'
#' Add your own useful columns, such as `feature`, `angle`, `tag` or `specimen_category`. Later, figure filters use those column names and values to find the correct illustration; they do not require a standard plate arrangement. Keep specimen codes consistent with the character-state matrix if descriptions must cite a particular specimen.
#'
#' For examples of figure selection, see the *Preparing CSV data* vignette and `parse_figure_filter()`.
#'
#' @param file Character string. Path to the input CSV file.
#' @param fileEncoding Character string giving the file encoding passed to
#'   \code{utils::read.csv()}. Default is \code{"UTF-8"}.
#' @param split_columns Character vector or \code{NULL}. Optional names of
#'   metadata columns to split into multi-value list-columns. Column names are
#'   matched after conversion to lowercase. The \code{taxon} column may be
#'   included here if a figure record should be associated with multiple taxa.
#' @param split_char Character string. Delimiter used when splitting values in
#'   \code{split_columns}. Default is \code{";"}.
#' @param ... Additional arguments passed to \code{utils::read.csv()} via
#'   taxRmatic's internal CSV reader.
#'
#' @return A data frame of class \code{"taxRmatic_figures"}.
#'
#' @examples
#' \dontrun{
#' figs <- read_taxRmatic_figures("figures.csv")
#'
#' figs <- read_taxRmatic_figures(
#'   "figures.csv",
#'   split_columns = c("feature", "view"),
#'   split_char = ";"
#' )
#'
#' figs <- read_taxRmatic_figures(
#'   "figures.csv",
#'   split_columns = c("taxon", "feature"),
#'   split_char = ";"
#' )
#' }
#'
#' @export
read_taxRmatic_figures <- function(
    file,
    fileEncoding = "UTF-8",
    split_columns = NULL,
    split_char = ";",
    ...
) {

  # READ INPUT -----------------------------------------------------------------
  # Import the CSV using the shared reader so behaviour is consistent across
  # taxRmatic read functions.

  df <- read_taxRmatic_csv(
    file = file,
    fileEncoding = fileEncoding,
    message_label = "Figures",
    ...
  )

  # STANDARDISE INPUT ----------------------------------------------------------
  # Normalise formatting so downstream code can rely on consistent structure:
  # - force lowercase column names
  # - trim leading and trailing whitespace from character values

  df <- lowercase_column_names(df, inform = FALSE)


  # Trim figure metadata values (used in grouping and filter comparisons).
  df[] <- lapply(df, trimws)

  # CHECK MANDATORY COLUMNS ----------------------------------------------------
  # Ensure that all mandatory columns needed for figure workflows are present.

  mandatory_cols <- c(
    "figure_number",
    "figure_part",
    "taxon",
    "specimen_code"
  )

  check_required_columns(
    df,
    required_cols = mandatory_cols,
    inform = FALSE
  )

  # VALIDATE FIGURE IDENTIFIERS ------------------------------------------------
  # Figure number is the only mandatory value that must be filled for every
  # record. Figure part may be blank for whole-figure records.

  empty_figure_number <- which(is_missing_or_empty(df$figure_number))

  if (length(empty_figure_number) > 0) {
    stop(
      "Some figure records lack a 'figure_number' value.",
      call. = FALSE
    )
  }

  # VALIDATE FIGURE PART CONSISTENCY -------------------------------------------
  # For each figure number, figure_part must be used consistently. A figure
  # should either be represented as a whole figure with blank figure_part values,
  # or as a set of labelled parts with figure_part filled for every record.

  figure_part_filled <- !is_missing_or_empty(df$figure_part)

  figure_part_by_number <- split(
    figure_part_filled,
    df$figure_number
  )

  inconsistent_figure_numbers <- names(figure_part_by_number)[
    vapply(
      figure_part_by_number,
      function(x) any(x) && !all(x),
      logical(1)
    )
  ]

  if (length(inconsistent_figure_numbers) > 0) {
    stop(
      "Inconsistent 'figure_part' values found. ",
      "For each figure_number, 'figure_part' must be either filled for all ",
      "records or blank for all records.\n",
      "Affected figure numbers:\n  - ",
      paste(inconsistent_figure_numbers, collapse = "\n  - "),
      call. = FALSE
    )
  }

  # CHECK DUPLICATE FIGURE RECORDS ---------------------------------------------
  # Each row should correspond to a unique figure or figure part.

  figure_keys <- paste(
    df$figure_number,
    ifelse(is_missing_or_empty(df$figure_part), "", df$figure_part),
    sep = "___"
  )

  duplicate_figure_keys <- figure_keys[duplicated(figure_keys)]

  if (length(duplicate_figure_keys) > 0) {
    duplicate_rows <- which(figure_keys %in% duplicate_figure_keys)

    duplicate_records <- ifelse(
      is_missing_or_empty(df$figure_part[duplicate_rows]),
      paste0("Figure ", df$figure_number[duplicate_rows]),
      paste0(
        "Figure ",
        df$figure_number[duplicate_rows],
        " (part ",
        df$figure_part[duplicate_rows],
        ")"
      )
    )

    stop(
      "Duplicate figure_number + figure_part combinations found. ",
      "Each figure record should represent a unique figure or figure part.\n",
      "Affected figure records:\n  - ",
      paste(unique(duplicate_records), collapse = "\n  - "),
      call. = FALSE
    )
  }


  # VALIDATE SPLITTING ARGUMENTS ----------------------------------------------
  # Optional user-selected metadata columns can be split into list-columns.
  # The taxon column may be split if users want one figure record to apply to
  # multiple taxa. Structural figure fields are not allowed here.

  if (!is.character(split_char) || length(split_char) != 1L ||
      is.na(split_char) || !nzchar(split_char)) {
    stop("`split_char` must be a single non-empty delimiter string.",
         call. = FALSE)
  }

  if (!is.null(split_columns)) {

    if (!is.character(split_columns)) {
      stop(
        "`split_columns` must be NULL or a character vector of column names.",
        call. = FALSE
      )
    }

    split_columns <- unique(tolower(trimws(split_columns)))

    if (any(is.na(split_columns) | split_columns == "")) {
      stop(
        "`split_columns` cannot contain missing or empty column names.",
        call. = FALSE
      )
    }

    missing_split_cols <- setdiff(split_columns, colnames(df))

    if (length(missing_split_cols) > 0) {
      stop(
        "The following `split_columns` are not present in the input file: ",
        paste(missing_split_cols, collapse = ", "),
        call. = FALSE
      )
    }

    # `specimen_code` is mandatory and is always split automatically.
    if ("specimen_code" %in% split_columns) {
      message(
        "`specimen_code` is split automatically on commas and was ignored in ",
        "`split_columns`."
      )

      split_columns <- setdiff(split_columns, "specimen_code")
    }

    protected_split_cols <- c("figure_number", "figure_part")
    invalid_split_cols <- intersect(split_columns, protected_split_cols)

    if (length(invalid_split_cols) > 0) {
      stop(
        "The following structural columns cannot be supplied in `split_columns`: ",
        paste(invalid_split_cols, collapse = ", "),
        ". The `taxon` column may be split if multiple taxa need to be ",
        "associated with one figure record.",
        call. = FALSE
      )
    }

  } else {
    split_columns <- character(0)
  }

  # SPLIT SELECTED COLUMNS -----------------------------------------------------
  # Convert selected metadata columns into multi-value list-columns. This is
  # useful for fields such as taxon, tags, features, views, or life stages.

  if (length(split_columns) > 0) {

    df[split_columns] <- lapply(df[split_columns], function(col) {
      lapply(col, split_and_trim, split_char = split_char)
    })

    message(
      "Columns split into multi-value fields using '",
      split_char,
      "': ",
      paste(split_columns, collapse = ", "),
      "."
    )
  }

  # SPLIT SPECIMEN CODES -------------------------------------------------------
  # Specimen codes are a mandatory field and are always split consistently using
  # commas, independent of `split_columns` and `split_char`.

  df$specimen_code <- lapply(
    df$specimen_code,
    split_and_trim,
    split_char = ","
  )


  # CHECK FILTERING AND DESCRIPTION-LINKING FIELDS -----------------------------
  # `specimen_code` is used for specimen-code-based matching in description
  # generation. `taxon` is useful for taxon-based filtering and captions, but is
  # not required for specimen-code-based matching.

  has_value <- function(x) {
    if (is.list(x)) {
      return(vapply(
        x,
        function(cell) {
          if (length(cell) == 0) {
            return(FALSE)
          }

          any(!is.na(cell) & nzchar(trimws(as.character(cell))))
        },
        logical(1)
      ))
    }

    !is_missing_or_empty(x)
  }

  format_figure_records <- function(rows, max_records = 10) {
    affected_rows <- df[rows, , drop = FALSE]

    records <- ifelse(
      is_missing_or_empty(affected_rows$figure_part),
      paste0("Figure ", affected_rows$figure_number),
      paste0(
        "Figure ",
        affected_rows$figure_number,
        " (part ",
        affected_rows$figure_part,
        ")"
      )
    )

    records <- unique(records)

    if (length(records) <= max_records) {
      return(paste(records, collapse = "\n  - "))
    }

    paste0(
      paste(records[seq_len(max_records)], collapse = "\n  - "),
      "\n  - ... and ",
      length(records) - max_records,
      " more"
    )
  }

  missing_specimen_code <- which(!has_value(df$specimen_code))
  missing_taxon <- which(!has_value(df$taxon))

  if (length(missing_specimen_code) > 0) {
    message(
      "Note: ",
      length(missing_specimen_code),
      " figure records are missing 'specimen_code'. ",
      "These records cannot be linked to specimen-specific descriptions using ",
      "specimen-code matching.\n",
      "Affected figure records:\n  - ",
      format_figure_records(missing_specimen_code)
    )
  }

  if (length(missing_taxon) > 0) {
    message(
      "Note: ",
      length(missing_taxon),
      " figure records are missing 'taxon'. ",
      "These records can still be matched by specimen_code or other metadata, ",
      "but cannot be used for taxon-based filtering unless taxon information is ",
      "supplied.\n",
      "Affected figure records:\n  - ",
      format_figure_records(missing_taxon)
    )
  }

  # ASSIGN CLASS AND PRINT SUMMARY ---------------------------------------------
  # Tag the result with its taxRmatic-specific class so downstream functions can
  # recognise and validate it.

  class(df) <- c("taxRmatic_figures", class(df))


  message(
    "\nread_taxRmatic_figures() completed successfully.\n",
    "\nFigure table:\n",
    "  - Figure records: ", nrow(df), "\n",
    "  - Columns: ", ncol(df), "\n",
    "  - Split metadata columns: ", length(split_columns), "\n",
    "  - Records missing specimen_code: ", length(missing_specimen_code), "\n",
    "  - Records missing taxon: ", length(missing_taxon), "\n"
  )

  return(df)
}
