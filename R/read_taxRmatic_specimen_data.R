#' Read specimen data for material examined sections from a CSV file
#'
#' Reads specimen records from a CSV file for use in the Material examined
#' section. Each row represents a specimen or specimen lot. taxRmatic uses
#' the taxon, type status and locality information to organise the records.
#' The returned object can be passed to `get_taxRmatic_material_examined()`.
#'
#' @details
#' ## Expected input structure
#'
#' The input file is read using the standard taxRmatic CSV reader. Column names
#' are converted to lowercase, duplicate column names cause an error, and
#' mandatory columns are validated. Key fields are trimmed, and `type_status`
#' values are converted to lowercase.
#'
#' ## File encoding
#'
#' Input CSV files should be saved with UTF-8 encoding. This helps preserve
#' special characters commonly used in taxonomic manuscripts, including degree
#' symbols, en dashes, accented characters, and other non-ASCII text.
#'
#' By default, `read_taxRmatic_specimen_data()` reads files using
#' `fileEncoding = "UTF-8"`. If your file uses a different encoding, supply it
#' with the `fileEncoding` argument.
#'
#' ## Mandatory columns
#'
#' The input file must contain the following mandatory columns:
#'
#' \itemize{
#'   \item `taxon` — the focal taxon to which each specimen record belongs.
#'   \item `type_status` — the type category for each specimen; this may be
#'   empty for non-type material.
#'   \item `country` — the country associated with the specimen record.
#'   \item `state` — the state or other internal region associated with the
#'   specimen record; this may be empty.
#' }
#'
#' ## Mandatory values
#'
#' The following columns must contain non-empty values in every row:
#'
#' \itemize{
#'   \item `taxon`
#'   \item `country`
#' }
#'
#' The `type_status` column may contain empty values for non-type material.
#' Non-empty values are checked against a set of commonly expected zoological
#' type categories. Any unexpected non-empty values trigger a warning rather
#' than an error, allowing intentional alternatives to be used while still
#' helping detect likely misspellings.
#'
#' The `state` column may contain empty values. However, state usage must be
#' consistent within each country: for a given country, either all specimen
#' records must have a `state` value, or none of them may have one. Mixed usage
#' within a country causes the function to stop with an informative error.
#'
#' Use `type_status` only for actual type designations (e.g. `holotype`,
#' `paratype`); leave it blank for all non-type specimens. The heading
#' *Other material examined* is generated automatically downstream.
#'
#' @section Preparing specimen records:
#' Keep the collection fields you already use. The CSV must have `taxon`, `type_status`, `country` and `state` columns; other fields, including locality descriptions, dates, collectors and registration numbers, can use names that suit your database.
#'
#' Use `type_status` only for genuine type specimens (for example `holotype` or `paratype`). Leave it blank for non-types; the later material-examined function prints these under Other material examined. After importing, you may add formatted fields, such as registration numbers in brackets or converted coordinates, before passing the result to `get_taxRmatic_material_examined()`.
#'
#' @param file Character(1). Path to the input CSV file.
#' @param fileEncoding Character(1). File encoding passed to the CSV reader.
#'   Defaults to `"UTF-8"`.
#' @param ... Additional arguments passed to the underlying CSV reader.
#'
#' @return A data frame of class `taxRmatic_specimen_data`.
#'
#' @examples
#' \dontrun{
#' specimen_data <- read_taxRmatic_specimen_data("specimens.csv")
#' }
#'
#' @export
read_taxRmatic_specimen_data <- function(file, fileEncoding = "UTF-8", ...) {

  # READ INPUT -----------------------------------------------------------------
  # Import the CSV using the shared reader so behaviour is consistent across
  # taxRmatic read functions.

  df <- read_taxRmatic_csv(
    file = file,
    fileEncoding = fileEncoding,
    message_label = "taxRmatic specimen data",
    duplicate_columns = "error",
    ...
  )

  # STANDARDISE INPUT ----------------------------------------------------------
  # Convert column names to lowercase so downstream code can rely on a
  # consistent naming convention regardless of how the file was authored.

  df <- lowercase_column_names(
    df,
    inform = FALSE
  )

  # CHECK COLUMN STRUCTURE -----------------------------------------------------
  # Ensure all required fields for material examined workflows are present.
  # Duplicate column names are rejected by the shared CSV reader.


  mandatory_cols <- c(
    "taxon",
    "type_status",
    "country",
    "state"
  )

  check_required_columns(
    df,
    required_cols = mandatory_cols,
    function_name = "read_taxRmatic_specimen_data",
    inform = FALSE
  )

  # TRIM WHITESPACE ------------------------------------------------------------
  # Remove leading and trailing whitespace from key fields so accidental spaces
  # do not interfere with grouping, validation, or downstream formatting.
  # Convert `type_status` to lowercase at the same time for consistency.

  df <- trim_columns(
    df,
    cols = mandatory_cols,
    tolower_cols = "type_status",
    inform = FALSE
  )

  # CHECK MANDATORY NON-EMPTY VALUES -------------------------------------------
  # Ensure the core identifying fields contain values in every row, not just
  # that the columns themselves exist.

  mandatory_value_cols <- c(
    "taxon",
    "country"
  )

  check_non_empty_columns(
    df,
    cols = mandatory_value_cols,
    subject = "Specimens",
    call. = FALSE
  )


  # CHECK TYPE STATUS VALUES ---------------------------------------------------
  # Compare non-empty type_status values against a set of commonly expected
  # zoological type categories. Unexpected values trigger a warning rather than
  # an error, so intentional alternatives remain possible.

  # Non-types are indicated by a blank type_status value. The display label
  # 'Other material examined' is generated later and is not a status.
  invalid_non_type <- which(tolower(trimws(as.character(df$type_status))) ==
                              "other material examined")
  if (length(invalid_non_type)) {
    stop("Do not enter 'other material examined' in `type_status`. ",
         "Leave the cell blank for non-type specimens. Row(s): ",
         paste(invalid_non_type, collapse = ", "), ".", call. = FALSE)
  }

  expected_type_status <- c(
    "holotype",
    "neotype",
    "syntype",
    "lectotype",
    "paralectotype",
    "allotype",
    "paratype"
  )

  entered_type_status <- unique(df$type_status)
  entered_type_status <- entered_type_status[
    !is_missing_or_empty(entered_type_status)
  ]

  unexpected_type_status <- setdiff(
    entered_type_status,
    expected_type_status
  )

  if (length(unexpected_type_status) > 0) {
    warning(
      "The following non-empty 'type_status' values are not among the commonly expected ",
      "zoological type categories: ",
      paste(unexpected_type_status, collapse = ", "),
      ".\n",
      "Please check for possible misspellings. If these values are intentional, they can still be used.",
      call. = FALSE
    )
  }

  # CHECK STATE CONSISTENCY WITHIN COUNTRIES -----------------------------------
  # Ensure that state usage is consistent within each country. A country may
  # either have state values for all specimen records or for none of them, but
  # mixed usage within the same country is not allowed.

  country_list <- split(df, df$country)

  for (country_name in names(country_list)) {
    country_data <- country_list[[country_name]]
    has_state <- !is_missing_or_empty(country_data$state)

    if (any(has_state) && any(!has_state)) {

      rows_with_state <- which(
        df$country == country_name & !is_missing_or_empty(df$state)
      )

      rows_without_state <- which(
        df$country == country_name & is_missing_or_empty(df$state)
      )

      stop(
        "Inconsistent 'state' data detected for country '", country_name, "'.\n",
        "Within a country, specimens must either all have 'state' values or all lack them.\n",
        "Rows with state values: ", paste(rows_with_state, collapse = ", "), ".\n",
        "Rows without state values: ", paste(rows_without_state, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }


  # ASSIGN CLASS ---------------------------------------------------------------
  # Tag the result with its taxRmatic-specific class so downstream functions
  # can recognise and validate it.

  class(df) <- c(
    "taxRmatic_specimen_data",
    setdiff(class(df), "taxRmatic_specimen_data")
  )

  # PRINT IMPORT SUMMARY -------------------------------------------------------
  # Report successful completion and the number of imported specimen records.

  n_states <- length(unique(df$state[!is_missing_or_empty(df$state)]))
  n_type_statuses <- length(unique(df$type_status[!is_missing_or_empty(df$type_status)]))

  message(
    "\nread_taxRmatic_specimen_data() completed successfully.\n",
    "\nSpecimen data table:\n",
    "  - Specimen records: ", nrow(df), "\n",
    "  - Taxa: ", length(unique(df$taxon)), "\n",
    "  - Countries: ", length(unique(df$country)), "\n",
    "  - States/regions: ", n_states, "\n",
    "  - Type-status categories: ", n_type_statuses, "\n"
  )

  return(df)
}
