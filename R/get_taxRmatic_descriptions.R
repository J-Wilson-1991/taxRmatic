#' Generate descriptions for all taxa in a character-state matrix
#'
#' Creates descriptions from the data imported by
#' \code{read_taxRmatic_character_state_matrix()}. Each description row in the
#' spreadsheet becomes one description in the resulting
#' \code{taxRmatic_section} object, which you can pass to
#' \code{compile_taxon_texts()}.
#'
#' If you supply a figures object from \code{read_taxRmatic_figures()}, taxRmatic
#' can add matching figure references to the descriptions. It uses specimen
#' codes and the figure filters in your character-state matrix.
#'
#' @details
#' Descriptions are generated for all taxa and description titles in the
#' matrix. By default, the order within each taxon matches the spreadsheet.
#' You can change that order in your spreadsheet, or use the optional
#' \code{ordering} argument to sort by selected columns. For most projects,
#' arranging the rows in the spreadsheet is simplest.
#'
#' The \code{title} column supplies the subsection name (for example,
#' \code{Holotype male} or \code{Female}). The other formatting arguments
#' control paragraph labels and figure references. For the structure of the
#' original CSV, see the CSV preparation vignette.
#'
#' @section Using this function:
#' First read a CSV with `read_taxRmatic_character_state_matrix()`. You can then generate all its description rows at once. The `title` values in the matrix identify individual descriptions, while the `taxon` column determines which species treatment receives each one.
#'
#' Supply a figure object from `read_taxRmatic_figures()` as the second argument when you want descriptions to cite matching figures. Use `print_paragraph_title = FALSE` to omit labels such as Body or Leg I without changing the underlying character groups. If the prose is not arranged as intended, check the `paragraph`, `sentence`, `clause`, `prefix` and `suffix` rows in the CSV rather than editing the generated text.
#'
#' @param taxRmatic_character_state_matrix An object of class
#'   `"taxRmatic_character_state_matrix"`, as returned by
#'   [read_taxRmatic_character_state_matrix()].
#' @param taxRmatic_figures `NULL` or an object of class `"taxRmatic_figures"`,
#'   as returned by [read_taxRmatic_figures()]. If supplied, figure references
#'   may be appended within individual descriptions.
#' @param format_paragraph_title Named list of formatting options passed to
#'   [get_taxRmatic_description()] for paragraph titles.
#' @param print_paragraph_title Logical. If `TRUE` (default), paragraph titles
#'   are included in the output. If `FALSE`, paragraph titles are omitted.
#' @param paragraph_separator Character string. Text used to separate paragraphs
#'   within each returned description. Passed to [get_taxRmatic_description()].
#'   The default, `"  \n\n"`, produces Markdown-style paragraph separation with
#'   a line break.
#' @param figure_reference_style Character string. Style used when formatting
#'   figure references. Passed through to [get_taxRmatic_description()] and
#'   ultimately to [format_figure_reference_text()]. Must be one of `"full"` or
#'   `"compact"`.
#' @param figure_prefix_singular Character string. Prefix used when a figure
#'   reference resolves to a single figure, e.g. `"Fig."` or `"Figure"`.
#' @param figure_prefix_plural Character string. Prefix used when a figure
#'   reference resolves to multiple figures, e.g. `"Figs."` or `"Figures"`.
#' @param ordering `NULL` or a named list of ordering rules passed to
#'   [order_specimens()] to reorder description rows within each taxon before
#'   output is assembled. If `ordering = NULL`, the default, description rows are
#'   kept in their input order within each taxon. Columns used for ordering
#'   should be single-value columns; list-columns such as `specimen_code` cannot
#'   be used for ordering.
#'
#' @return A data frame of class `taxRmatic_section`. Each row represents one
#'   description, with:
#'   \itemize{
#'     \item `taxon` — the focal taxon
#'     \item `subsection` — the description `title`
#'     \item `text` — the formatted description body
#'   }
#'   If no descriptions are generated, an empty `taxRmatic_section` object is
#'   returned.
#'
#' @examples
#' \dontrun{
#' descs <- get_taxRmatic_descriptions(
#'   taxRmatic_character_state_matrix = x
#' )
#'
#' descs <- get_taxRmatic_descriptions(
#'   taxRmatic_character_state_matrix = x,
#'   taxRmatic_figures = figs,
#'   figure_reference_style = "compact",
#'   figure_prefix_singular = "Figure",
#'   figure_prefix_plural = "Figures"
#' )
#'
#' descs <- get_taxRmatic_descriptions(
#'   taxRmatic_character_state_matrix = x,
#'   print_paragraph_title = FALSE,
#'   paragraph_separator = " "
#' )
#'
#' descs <- get_taxRmatic_descriptions(
#'   taxRmatic_character_state_matrix = x,
#'   ordering = list(sex = c("male", "female"))
#' )
#' }
#'
#' @export
get_taxRmatic_descriptions <- function(
    taxRmatic_character_state_matrix,
    taxRmatic_figures = NULL,
    format_paragraph_title = list(case = "capitalise_first", prefix = "*", suffix = ".* "),
    print_paragraph_title = TRUE,
    paragraph_separator = "  \n\n",
    figure_reference_style = c("full", "compact"),
    figure_prefix_singular = "Fig.",
    figure_prefix_plural = "Figs.",
    ordering = NULL
) {

  # VALIDATE ARGUMENTS ---------------------------------------------------------

  figure_reference_style <- match.arg(figure_reference_style)

  if (!inherits(taxRmatic_character_state_matrix, "taxRmatic_character_state_matrix")) {
    stop(
      "`taxRmatic_character_state_matrix` must be an object of class ",
      "'taxRmatic_character_state_matrix', as returned by ",
      "`read_taxRmatic_character_state_matrix()`.",
      call. = FALSE
    )
  }

  if (!is.null(taxRmatic_figures) &&
      !inherits(taxRmatic_figures, "taxRmatic_figures")) {
    stop(
      "`taxRmatic_figures` must be an object of class ",
      "'taxRmatic_figures', as returned by `read_taxRmatic_figures()`.",
      call. = FALSE
    )
  }

  if (!is.logical(print_paragraph_title) ||
      length(print_paragraph_title) != 1 ||
      is.na(print_paragraph_title)) {
    stop(
      "`print_paragraph_title` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.character(paragraph_separator) ||
      length(paragraph_separator) != 1 ||
      is.na(paragraph_separator)) {
    stop(
      "`paragraph_separator` must be a single character string.",
      call. = FALSE
    )
  }

  if (!is.list(format_paragraph_title)) {
    stop(
      "`format_paragraph_title` must be a named list.",
      call. = FALSE
    )
  }

  if (!is.character(figure_prefix_singular) ||
      length(figure_prefix_singular) != 1 ||
      is.na(figure_prefix_singular)) {
    stop(
      "`figure_prefix_singular` must be a single character string.",
      call. = FALSE
    )
  }

  if (!is.character(figure_prefix_plural) ||
      length(figure_prefix_plural) != 1 ||
      is.na(figure_prefix_plural)) {
    stop(
      "`figure_prefix_plural` must be a single character string.",
      call. = FALSE
    )
  }

  if (!is.null(ordering) && !is.list(ordering)) {
    stop(
      "`ordering` must be NULL or a named list of ordering rules.",
      call. = FALSE
    )
  }

  # EXTRACT AND VALIDATE OBJECT STRUCTURE --------------------------------------

  character_info <- taxRmatic_character_state_matrix$character_information
  charmat <- taxRmatic_character_state_matrix$character_state_matrix

  if (!is.data.frame(character_info)) {
    stop(
      "`taxRmatic_character_state_matrix$character_information` must be a data frame. ",
      "Re-import the file with read_taxRmatic_character_state_matrix().",
      call. = FALSE
    )
  }

  if (!is.data.frame(charmat)) {
    stop(
      "`taxRmatic_character_state_matrix$character_state_matrix` must be a data frame. ",
      "Re-import the file with read_taxRmatic_character_state_matrix().",
      call. = FALSE
    )
  }

  required_character_info_cols <- c(
    "paragraph",
    "sentence",
    "clause",
    "prefix",
    "suffix",
    "figure_filter"
  )

  missing_character_info_cols <- setdiff(
    required_character_info_cols,
    colnames(character_info)
  )

  if (length(missing_character_info_cols) > 0) {
    stop(
      "`taxRmatic_character_state_matrix$character_information` is missing ",
      "required columns: ",
      paste(missing_character_info_cols, collapse = ", "),
      ". Re-import the file with read_taxRmatic_character_state_matrix().",
      call. = FALSE
    )
  }

  required_state_cols <- c("taxon", "title", "specimen_code")

  missing_state_cols <- setdiff(
    required_state_cols,
    colnames(charmat)
  )

  if (length(missing_state_cols) > 0) {
    stop(
      "`taxRmatic_character_state_matrix$character_state_matrix` is missing ",
      "required columns: ",
      paste(missing_state_cols, collapse = ", "),
      ". Re-import the file with read_taxRmatic_character_state_matrix().",
      call. = FALSE
    )
  }

  if (!is.list(charmat$specimen_code)) {
    stop(
      "`taxRmatic_character_state_matrix$character_state_matrix$specimen_code` ",
      "must be a list-column. Re-import the file with ",
      "read_taxRmatic_character_state_matrix().",
      call. = FALSE
    )
  }

  if (!is.list(character_info$figure_filter)) {
    stop(
      "`taxRmatic_character_state_matrix$character_information$figure_filter` ",
      "must be a parsed list-column. Re-import the file with ",
      "read_taxRmatic_character_state_matrix().",
      call. = FALSE
    )
  }

  if (!is.null(taxRmatic_figures)) {
    required_figure_cols <- c(
      "figure_number",
      "figure_part",
      "taxon",
      "specimen_code"
    )

    missing_figure_cols <- setdiff(
      required_figure_cols,
      colnames(taxRmatic_figures)
    )

    if (length(missing_figure_cols) > 0) {
      stop(
        "`taxRmatic_figures` is missing required columns: ",
        paste(missing_figure_cols, collapse = ", "),
        ". Re-import the file with read_taxRmatic_figures().",
        call. = FALSE
      )
    }

    if (!is.list(taxRmatic_figures$specimen_code)) {
      stop(
        "`taxRmatic_figures$specimen_code` must be a list-column. ",
        "Re-import the file with read_taxRmatic_figures().",
        call. = FALSE
      )
    }
  }

  # VALIDATE DESCRIPTION ROW VALUES -------------------------------------------
  # These should already have been checked by read_taxRmatic_character_state_matrix(),
  # but checking here gives clearer errors for hand-made or outdated objects.

  empty_taxon <- which(is_missing_or_empty(charmat$taxon))
  empty_title <- which(is_missing_or_empty(charmat$title))

  if (length(empty_taxon) > 0) {
    stop(
      "Some rows in `character_state_matrix` lack a 'taxon' value. ",
      "Re-import or repair the character-state matrix.",
      call. = FALSE
    )
  }

  if (length(empty_title) > 0) {
    stop(
      "Some rows in `character_state_matrix` lack a 'title' value. ",
      "Re-import or repair the character-state matrix.",
      call. = FALSE
    )
  }

  duplicate_titles <- charmat$title[
    duplicated(charmat$title) &
      !is_missing_or_empty(charmat$title)
  ]

  if (length(duplicate_titles) > 0) {
    stop(
      "Duplicate title values found in `character_state_matrix`: ",
      paste(unique(duplicate_titles), collapse = ", "),
      ". Each description row must have a unique title.",
      call. = FALSE
    )
  }

  # INDEX FIGURES BY SPECIMEN CODE ----------------------------------------------
  # Build this lookup once for the entire batch. Each code maps to the original
  # row numbers of figure records containing that code (including multi-code
  # records). Character-specific filtering still happens per sentence.
  figure_index <- NULL

  if (!is.null(taxRmatic_figures)) {
    figure_codes <- lapply(
      taxRmatic_figures$specimen_code,
      normalise_filter_values
    )
    code_lengths <- lengths(figure_codes)

    all_codes <- unlist(figure_codes, use.names = FALSE)
    if (length(all_codes)) {
      figure_rows <- rep(seq_along(figure_codes), times = code_lengths)
      figure_index <- split(figure_rows, all_codes)
    } else {
      figure_index <- list()
    }
  }

  # INITIALISE OUTPUT CONTAINERS -----------------------------------------------
  # Store each completed description row in a list, then bind them together
  # into a single `taxRmatic_section` object at the end.

  all_rows <- vector("list", nrow(charmat))
  output_index <- 0L
  all_taxa <- unique(charmat$taxon)

  # LOOP THROUGH TAXA ----------------------------------------------------------
  # Process descriptions one taxon at a time so ordering and output structure
  # are handled within taxon boundaries.

  for (tx in all_taxa) {

    taxon_df <- charmat[charmat$taxon == tx, , drop = FALSE]

    # APPLY OPTIONAL ORDERING --------------------------------------------------
    # If the user has supplied a named list of ordering rules, reorder the
    # description rows within this taxon before generating output.

    if (!is.null(ordering)) {
      taxon_df <- order_specimens(taxon_df, specimen_order = ordering)
    }

    # LOOP THROUGH DESCRIPTION TITLES ------------------------------------------
    # Each title within the current taxon is treated as one description to be
    # generated and stored as a subsection.

    source_indices <- match(taxon_df$title, charmat$title)

    for (position in seq_len(nrow(taxon_df))) {
      ttl <- taxon_df$title[position]

      # BUILD DESCRIPTION TEXT -------------------------------------------------
      # Delegate the actual natural-language assembly to the lower-level helper,
      # which handles paragraph structure, optional paragraph titles, optional
      # paragraph separators, and optional figure references.

      body <- get_taxRmatic_description(
        taxRmatic_character_state_matrix = taxRmatic_character_state_matrix,
        taxRmatic_figures = taxRmatic_figures,
        title = ttl,
        format_paragraph_title = format_paragraph_title,
        print_paragraph_title = print_paragraph_title,
        paragraph_separator = paragraph_separator,
        figure_reference_style = figure_reference_style,
        figure_prefix_singular = figure_prefix_singular,
        figure_prefix_plural = figure_prefix_plural,
        .validated = TRUE,
        .row_index = source_indices[position],
        .figure_index = figure_index
      )

      # Skip empty outputs if no description text was generated.

      if (is.null(body) || !nzchar(body)) {
        next
      }

      # STORE AS SECTION ROW ---------------------------------------------------
      # Save the completed description as one row in the standard
      # `taxRmatic_section` structure, in its intended printing order.

      output_index <- output_index + 1L
      all_rows[[output_index]] <-
        new_taxRmatic_section_row(
          taxon = tx,
          subsection = ttl,
          text = body
        )
    }
  }

  # RETURN EMPTY OBJECT IF NEEDED ----------------------------------------------
  # If no descriptions were generated, return an empty `taxRmatic_section`
  # object with the standard structure.

  if (output_index == 0L) {
    return(empty_taxRmatic_section())
  }

  # COMBINE OUTPUT ROWS --------------------------------------------------------
  # Bind all generated rows into a single `taxRmatic_section` object and reset
  # row names for a clean final result.

  out <- do.call(rbind, all_rows[seq_len(output_index)])
  rownames(out) <- NULL
  out
}
