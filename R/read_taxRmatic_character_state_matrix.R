#' Read a taxRmatic character-state matrix from CSV
#'
#' Reads the character-state spreadsheet used to generate taxonomic
#' descriptions. Save the spreadsheet as a CSV UTF-8 file. Some rows contain
#' character values; other rows tell taxRmatic how to write and arrange them.
#'
#' @details
#' Expected input structure
#'
#' The input file must be a CSV in which:
#'
#' \itemize{
#'   \item each column after the general metadata columns represents a character
#'     that may optionally be used in a written description
#'   \item the \code{row_type} column defines what each row means
#'   \item rows with \code{row_type == "description"} contain specimen- or
#'     taxon-level character states
#'   \item rows with other \code{row_type} values define metadata about each
#'     character
#' }
#'
#' File encoding
#'
#' Input CSV files should be saved using UTF-8 encoding. This helps preserve
#' special characters such as accented letters, degree symbols, en dashes, Greek
#' letters, and other characters that may occur in taxonomic names, locality
#' data, specimen notes, or descriptive text.
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
#'   \item \code{row_type}: identifies the function of each row
#'   \item \code{taxon}: identifies the taxon associated with each description
#'     row. Each description row must be associated with a single taxon.
#'   \item \code{title}: gives a unique title or label for each description row
#'   \item \code{specimen_code}: contains one or more specimen codes associated
#'     with each description row. Multiple specimen codes should be separated by
#'     commas. This column must exist, but values may be left blank for some or
#'     all rows. Specimen codes are used to link specimen-specific figure
#'     components to descriptions.
#' }
#'
#' Mandatory row types
#'
#' The following \code{row_type} values are mandatory:
#'
#' \itemize{
#'   \item \code{"print"}: indicates which character columns should be included
#'     in output descriptions. Characters marked \code{"y"} are kept; characters
#'     left blank are not used. The value \code{"y"} is treated
#'     case-insensitively.
#'   \item \code{"paragraph"}: gives the paragraph grouping for each printed
#'     character. This must be filled for all printed characters.
#'   \item \code{"sentence"}: gives the sentence grouping for each printed
#'     character. This must be filled for all printed characters.
#'   \item \code{"clause"}: can be used to group related characters into a
#'     clause, with each character state separated by a comma. Characters with
#'     blank \code{clause} values are treated as unique clauses downstream and
#'     will be separated from other clauses by a semicolon.
#'   \item \code{"prefix"}: optional text to place before a character state
#'     when generating descriptions. The row must exist, but values may be blank.
#'     Spaces are not needed around text, and capitalisation is not necessary.
#'   \item \code{"suffix"}: optional text to place after a character state when
#'     generating descriptions. The row must exist, but values may be blank.
#'     Spaces are not needed around text, and periods are not necessary.
#'   \item \code{"figure_filter"}: figure-filter text used to insert
#'     character-specific figure references. The row must exist, but values may
#'     be blank. Non-blank values are parsed with \code{parse_figure_filter()}.
#'   \item \code{"description"}: marks the specimen- or taxon-level rows that
#'     contain actual character states.
#' }
#'
#' Each mandatory row type must occur exactly once, except for
#' \code{"description"}, which can occur multiple times.
#'
#' Additional row types
#'
#' Additional non-mandatory \code{row_type} values may also be included. These
#' are retained in the imported object, but are not used by the standard
#' downstream description-generating functions. This allows users to keep notes,
#' definitions, data-source fields, draft scoring rows, or other project-specific
#' information in the same spreadsheet without affecting description generation.
#'
#' How the spreadsheet is interpreted
#'
#' The function separates the input into two components:
#'
#' \enumerate{
#'   \item Character information
#'   \itemize{
#'     \item built from all rows where \code{row_type != "description"}
#'     \item filtered to retain only character columns marked \code{"y"} in the
#'       \code{print} row
#'     \item transposed so each row corresponds to a printable character
#'     \item includes mandatory character metadata such as paragraph, sentence,
#'       clause, prefix, suffix, and figure filters
#'     \item also retains any additional non-mandatory row types, although these
#'       are not used by standard downstream description-generating functions
#'   }
#'
#'   \item Character state matrix
#'   \itemize{
#'     \item built from all rows where \code{row_type == "description"}
#'     \item retains description-level metadata and character states
#'     \item checks that all description rows have \code{taxon} and
#'       \code{title} values
#'     \item checks that \code{title} values are unique
#'     \item splits the mandatory \code{specimen_code} column on commas and
#'       stores it as a list column
#'   }
#' }
#'
#' Behaviour and validation
#'
#' During import, the function:
#'
#' \itemize{
#'   \item reads all columns as character
#'   \item standardises column names to lowercase; the shared CSV reader
#'     handles duplicate and blank names and removes empty spreadsheet artefacts
#'   \item trims and lowercases \code{row_type} values
#'   \item accepts duplicate character-column names, made unique by the shared reader
#'   \item checks for mandatory columns
#'   \item checks for all mandatory \code{row_type} values
#'   \item checks that all non-\code{"description"} \code{row_type} values are
#'     unique
#'   \item treats \code{"y"} values in the \code{print} row case-insensitively
#'   \item checks that all printed characters have \code{paragraph} and
#'     \code{sentence} values, listing the offending character columns
#'     in any error
#'   \item allows blank \code{clause}, \code{prefix}, \code{suffix},
#'     \code{figure_filter}, and \code{specimen_code} values
#'   \item parses non-blank \code{figure_filter} values with
#'     \code{parse_figure_filter()}
#' }
#'
#' @section The next step:
#' Begin with the installed example `character_state_matrix.csv`, rather than a blank spreadsheet. The rows marked `description` contain the observations to describe. The `print`, `paragraph`, `sentence`, `clause`, `prefix`, `suffix` and `figure_filter` rows explain how those observations are turned into text.
#'
#' Every character selected with `y` in the `print` row needs both a `paragraph` and a `sentence` value, even if its `clause` is blank. Individual descriptions may leave measurements or characters blank when they do not apply. Give each description row a unique `title`; use matching `specimen_code` values in the figure table when you want references to a particular illustrated specimen.
#'
#' After reading, pass the returned object to `get_taxRmatic_descriptions()`. See the *Preparing CSV data* vignette for a worked example of the instruction rows.
#'
#' @param file Path to the input CSV file.
#' @param fileEncoding Character string giving the file encoding passed to
#'   \code{utils::read.csv()}. Default is \code{"UTF-8"}.
#' @param ... Additional arguments passed to \code{utils::read.csv()} via
#'   taxRmatic's internal CSV reader.
#'
#' @return A list of class \code{"taxRmatic_character_state_matrix"} with two
#'   elements:
#'   \describe{
#'     \item{character_information}{
#'       A data frame in which each row represents a printable character and
#'       columns contain character metadata such as paragraph, sentence, clause,
#'       prefix, suffix, title, taxon, figure filters, and any additional
#'       non-mandatory metadata fields supplied by the user.
#'     }
#'     \item{character_state_matrix}{
#'       A data frame containing all rows from the input where
#'       \code{row_type == "description"}, with description-level metadata,
#'       specimen codes, and character states.
#'     }
#'   }
#'
#' @examples
#' \dontrun{
#' x <- read_taxRmatic_character_state_matrix("character_state_matrix.csv")
#'
#' x$character_information
#' x$character_state_matrix
#' }
#'
#' @export
read_taxRmatic_character_state_matrix <- function(file, fileEncoding = "UTF-8", ...) {

  # READ INPUT -----------------------------------------------------------------

  df <- read_taxRmatic_csv(
    file = file,
    fileEncoding = fileEncoding,
    message_label = "Character-state matrix",
    ...
  )

  # STANDARDISE INPUT ----------------------------------------------------------

  df <- lowercase_column_names(df, inform = FALSE)

  # CHECK MANDATORY COLUMNS ----------------------------------------------------

  mandatory_cols <- c("row_type", "taxon", "title", "specimen_code")

  check_required_columns(
    df,
    required_cols = mandatory_cols,
    inform = FALSE
  )

  # STANDARDISE AND CHECK ROW TYPES -------------------------------------------

  df$row_type <- tolower(trimws(df$row_type))

  empty_row_type <- which(is_missing_or_empty(df$row_type))

  if (length(empty_row_type) > 0) {
    stop(
      "Some rows lack a 'row_type' value.",
      call. = FALSE
    )
  }

  row_types <- df$row_type

  mandatory_row_types <- c(
    "print",
    "paragraph",
    "sentence",
    "clause",
    "prefix",
    "suffix",
    "figure_filter",
    "description"
  )

  missing_row_types <- setdiff(mandatory_row_types, row_types)

  if (length(missing_row_types) > 0) {
    stop(
      "Missing mandatory row_type values: ",
      paste(missing_row_types, collapse = ", "),
      call. = FALSE
    )
  }

  # CHECK DUPLICATE METADATA ROWS ---------------------------------------------

  row_types_non_desc <- row_types[row_types != "description"]
  duplicate_row_types <- row_types_non_desc[duplicated(row_types_non_desc)]

  if (length(duplicate_row_types) > 0) {
    stop(
      "Duplicate non-description row_type values found: ",
      paste(unique(duplicate_row_types), collapse = ", "),
      ". Each non-description row_type value must be unique because these rows ",
      "are used to build character information. The 'description' row_type is ",
      "the only row_type value that can occur multiple times.",
      call. = FALSE
    )
  }

  # BUILD CHARACTER INFORMATION ------------------------------------------------

  character_information <- df[df$row_type != "description", , drop = FALSE]

  row.names(character_information) <- character_information$row_type
  character_information$row_type <- NULL

  printed_row <- character_information["print", , drop = FALSE]

  printed_values <- tolower(trimws(as.character(unlist(
    printed_row,
    use.names = FALSE
  ))))

  cols_to_keep <- printed_values == "y"
  names(cols_to_keep) <- colnames(printed_row)

  if (!any(cols_to_keep)) {
    stop(
      "No characters were marked with 'y' in the 'print' row.",
      call. = FALSE
    )
  }

  character_information <- character_information[, cols_to_keep, drop = FALSE]

  character_information <- character_information[
    rownames(character_information) != "print",
    ,
    drop = FALSE
  ]

  character_information <- as.data.frame(
    t(character_information),
    stringsAsFactors = FALSE
  )

  # PARSE FIGURE FILTERS -------------------------------------------------------

  parse_one_figure_filter <- function(x) {
    if (is.na(x) || !nzchar(trimws(x))) {
      return(list())
    }

    parse_figure_filter(x)
  }

  character_information$figure_filter <- I(
    lapply(character_information$figure_filter, parse_one_figure_filter)
  )

  empty_paragraph <- which(is_missing_or_empty(character_information$paragraph))
  empty_sentence <- which(is_missing_or_empty(character_information$sentence))

  # Report exactly which printed character columns need
  # metadata. Unprinted columns intentionally need no paragraph/sentence value.
  if (length(empty_paragraph) > 0) {
    stop(
      "Missing 'paragraph' value for printed character(s): ",
      paste(rownames(character_information)[empty_paragraph], collapse = ", "),
      ". Fill the 'paragraph' row for these columns (marked 'y' in 'print'); ",
      "unprinted columns may remain blank.",
      call. = FALSE
    )
  }

  if (length(empty_sentence) > 0) {
    stop(
      "Missing 'sentence' value for printed character(s): ",
      paste(rownames(character_information)[empty_sentence], collapse = ", "),
      ". Fill the 'sentence' row for these columns (marked 'y' in 'print'); ",
      "the 'clause' row is optional and unprinted columns may remain blank.",
      call. = FALSE
    )
  }

  # BUILD CHARACTER STATE MATRIX ----------------------------------------------

  character_state_matrix <- df[df$row_type == "description", , drop = FALSE]

  character_state_matrix <- character_state_matrix[
    ,
    colnames(character_state_matrix) != "row_type",
    drop = FALSE
  ]

  # VALIDATE DESCRIPTION ROWS --------------------------------------------------

  empty_taxon <- which(is_missing_or_empty(character_state_matrix$taxon))
  empty_title <- which(is_missing_or_empty(character_state_matrix$title))

  if (length(empty_taxon) > 0) {
    stop("Some description rows lack a 'taxon' value.", call. = FALSE)
  }

  if (length(empty_title) > 0) {
    stop("Some description rows lack a 'title' value.", call. = FALSE)
  }

  duplicate_titles <- character_state_matrix$title[
    duplicated(character_state_matrix$title) &
      !is_missing_or_empty(character_state_matrix$title)
  ]

  if (length(duplicate_titles) > 0) {
    stop(
      "Duplicate title values found in description rows: ",
      paste(unique(duplicate_titles), collapse = ", "),
      call. = FALSE
    )
  }

  message("Mandatory structure checked successfully.")

  # SPLIT SPECIMEN CODES -------------------------------------------------------

  character_state_matrix$specimen_code <- lapply(
    character_state_matrix$specimen_code,
    split_and_trim,
    split_char = ","
  )

  # RETURN OUTPUT --------------------------------------------------------------

  taxRmatic_characterstate <- list(
    character_information = character_information,
    character_state_matrix = character_state_matrix
  )

  class(taxRmatic_characterstate) <- "taxRmatic_character_state_matrix"

  clauses <- trimws(character_information$clause)
  clauses <- clauses[!is.na(clauses) & nzchar(clauses)]
  n_clauses <- length(unique(clauses))

  figure_filters <- character_information$figure_filter
  n_figure_filters <- sum(lengths(figure_filters) > 0)

  message(
    "\nread_taxRmatic_character_state_matrix() completed successfully.\n",
    "\nCharacter information:\n",
    "  - Printable characters: ", nrow(character_information), "\n",
    "  - Paragraphs: ", length(unique(character_information$paragraph)), "\n",
    "  - Sentences: ", length(unique(character_information$sentence)), "\n",
    "  - Explicit clauses: ", n_clauses, "\n",
    "  - Explicit figure filters: ", n_figure_filters, "\n",
    "\nCharacter state matrix:\n",
    "  - Description rows: ", nrow(character_state_matrix), "\n",
    "  - Columns: ", ncol(character_state_matrix), "\n"
  )

  return(taxRmatic_characterstate)
}
