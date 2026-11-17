#' Read written manuscript sections from a CSV file
#'
#' Imports sections you have written yourself, such as Etymology, Remarks,
#' Distribution or Natural history. Use this function when the text is already
#' written and does not need to be generated from a character matrix.
#'
#' @details
#' \strong{Preparing your CSV}
#'
#' Save your spreadsheet as a CSV UTF-8 file. Include these four columns:
#' \itemize{
#'   \item \code{taxon}: the taxon this paragraph belongs to.
#'   \item \code{section}: the section name, such as \code{Remarks}.
#'   \item \code{subsection}: an optional smaller heading, such as \code{Male}.
#'     Leave the cell blank if you do not want a smaller heading.
#'   \item \code{text}: the paragraph you want to print.
#' }
#'
#' Every row must have a \code{taxon}, \code{section} and \code{text} value.
#' You can leave \code{subsection} blank, but the column must be present.
#'
#' \strong{Paragraph order}
#'
#' Enter each paragraph on a separate spreadsheet row, in the order you
#' want it to appear. taxRmatic keeps this order within each taxon and section.
#' Repeated subsection names are allowed. For example, two consecutive rows
#' labelled \code{Male} become two paragraphs under one Male heading when
#' compiled. For details about how headings are printed or hidden, see
#' \code{compile_taxon_texts()}.
#'
#' \strong{Text formatting and figures}
#'
#' You can use \code{*italics*} and \code{**bold**} in the \code{text} column.
#' To insert a figure reference, use a figure tag such as
#' \code{fg\{taxon = Example species\}}. taxRmatic replaces the tag with matching
#' figure numbers during finalisation. For additional figure-tag examples,
#' see \code{replace_figure_tags()} and the CSV preparation vignette.
#'
#' The function returns a named list with one \code{taxRmatic_section}
#' object for each section in the CSV. The list names match the \code{section}
#' values; use these objects when calling \code{compile_taxon_texts()}.
#'
#' @section Preparing additional text:
#' Use this table for text you have already written, such as Etymology, Remarks, Distribution or Natural history. Each CSV row contains one paragraph for one taxon. Enter its section name in `section` and its prose in `text`; leave `subsection` blank unless you want a smaller heading such as Male or Female.
#'
#' After reading, run `names(sections)` to see which section objects were created. Supply only those existing objects to `compile_taxon_texts()`, for example `Remarks = sections$remarks`. The compiler uses the list name as the main section heading; it also decides when subsection headings appear. **The CSV row order determines paragraph order within each section.**
#'
#' @param file Location of the CSV file to read.
#' @param fileEncoding Character encoding of the file; normally \code{"UTF-8"}.
#' @param ... Other optional settings passed to the CSV reader.
#'
#' @return A named list of \code{taxRmatic_section} objects, one per section.
#'
#' @examples
#' \dontrun{
#' sections <- read_taxRmatic_sections("sections.csv")
#' names(sections)       # See which sections were imported.
#' sections$Remarks     # Select the Remarks section, if present.
#' }
#'
#' @export
read_taxRmatic_sections <- function(
    file,
    fileEncoding = "UTF-8",
    ...
) {

  # READ INPUT -----------------------------------------------------------------
  # Read the raw CSV using the shared taxRmatic import helper so behaviour is
  # consistent with other table-reading functions in the package. The shared

  df <- read_taxRmatic_csv(
    file = file,
    fileEncoding = fileEncoding,
    message_label = "taxRmatic sections",
    ...
  )

  # STANDARDISE INPUT ----------------------------------------------------------
  # Standardise column names.

  df <- lowercase_column_names(
    df,
    inform = FALSE
  )

  # CHECK COLUMN STRUCTURE -----------------------------------------------------
  # All reserved input columns are mandatory and must be present.


  mandatory_cols <- c(
    "taxon",
    "section",
    "subsection",
    "text"
  )

  check_required_columns(
    df,
    required_cols = mandatory_cols,
    function_name = "read_taxRmatic_sections",
    inform = FALSE
  )

  # TRIM WHITESPACE ------------------------------------------------------------
  # Trim leading and trailing whitespace from structural columns. The text
  # column is intentionally not trimmed to preserve free-text formatting.

  structural_cols <- c(
    "taxon",
    "section",
    "subsection"
  )

  df <- trim_columns(
    df,
    cols = structural_cols,
    inform = FALSE
  )

  # RETURN EMPTY LIST IF NEEDED ------------------------------------------------
  # If the input contains no data rows, return an empty named list.

  if (nrow(df) == 0) {
    out <- list()
    names(out) <- character(0)

    message(
      "\nread_taxRmatic_sections() completed successfully.\n",
      "\nSection table:\n",
      "  - Section records: 0\n",
      "  - Sections: 0\n",
      "  - Taxa: 0\n",
      "  - Explicit subsections: 0\n",
      "  - Returned objects: 0\n"
    )

    return(out)
  }

  # CHECK MANDATORY NON-EMPTY VALUES -------------------------------------------
  # These columns must contain meaningful values in every row.

  mandatory_value_cols <- c(
    "taxon",
    "section",
    "text"
  )

  check_non_empty_columns(
    df,
    cols = mandatory_value_cols,
    subject = "Rows",
    call. = FALSE
  )

  # PRESERVE INPUT ROW ORDER --------------------------------------------------
  # Named and blank subsections may be interleaved; row order determines
  # printing order within each section.

  sections <- unique(df$section)

  # HELPER TO BUILD taxRmatic_section ------------------------------------------
  # Convert a plain data frame subset into a taxRmatic_section object by
  # coercing each row through the standard single-row constructor. The section
  # column is not retained internally.

  make_taxRmatic_section <- function(sub_df) {

    all_rows <- lapply(seq_len(nrow(sub_df)), function(i) {
      new_taxRmatic_section_row(
        taxon = sub_df$taxon[i],
        subsection = sub_df$subsection[i],
        text = sub_df$text[i]
      )
    })

    out <- do.call(rbind, all_rows)
    rownames(out) <- NULL
    out
  }

  # BUILD NAMED SECTION LIST ---------------------------------------------------
  # Always return a named list, even if there is only one section.

  section_list <- lapply(sections, function(sec) {
    sub_df <- df[df$section == sec, , drop = FALSE]
    sub_df$section <- NULL
    make_taxRmatic_section(sub_df)
  })

  names(section_list) <- sections

  # SUMMARY --------------------------------------------------------------------

  subsection_pairs <- unique(
    paste(
      df$section[!is_missing_or_empty(df$subsection)],
      df$subsection[!is_missing_or_empty(df$subsection)],
      sep = "___"
    )
  )

  n_explicit_subsections <- length(subsection_pairs)


  message(
    "\nread_taxRmatic_sections() completed successfully.\n",
    "\nSection table:\n",
    "  - Section records: ", nrow(df), "\n",
    "  - Sections: ", length(sections), "\n",
    "  - Taxa: ", length(unique(df$taxon)), "\n",
    "  - Explicit subsections: ", n_explicit_subsections, "\n",
    "  - Returned objects: ", length(section_list), "\n"
  )

  section_list
}
