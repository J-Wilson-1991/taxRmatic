#' Collect filter column names from parsed figure filters
#'
#' Internal helper that recursively extracts column names from parsed
#' `figure_filter` objects.
#'
#' @param x A parsed figure filter object.
#'
#' @return Character vector of unique column names.
#' @keywords internal
collect_filter_columns <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }

  if (!is.list(x)) {
    return(character(0))
  }

  this_names <- names(x)
  this_names <- this_names[!is.na(this_names) & nzchar(this_names)]

  child_names <- unlist(lapply(x, collect_filter_columns), use.names = FALSE)

  unique(c(this_names, child_names))
}


#' Build a formatted description from a taxRmatic character-state matrix
#'
#' Generates a natural-language description for a single row in a
#' `taxRmatic_character_state_matrix` object, identified by its `title` value.
#' Character states are grouped hierarchically into paragraphs, sentences,
#' clauses, and individual character-state fragments, then assembled into a
#' formatted taxonomic description.
#'
#' This function is designed to generate one complete description at a time. The
#' supplied `title` must match exactly one row in the character-state matrix. If
#' no matching row is found, or if more than one row has the same `title`, the
#' function stops with an error.
#'
#' @details
#' ## Expected input objects
#'
#' `taxRmatic_character_state_matrix` should be an object returned by
#' [read_taxRmatic_character_state_matrix()]. The function expects the
#' standardised structure produced by that reader, including:
#'
#' - a `character_information` data frame with the required metadata fields
#'   `paragraph`, `sentence`, `clause`, `prefix`, `suffix`, and
#'   `figure_filter`
#' - a `character_state_matrix` data frame with the required fields `taxon`,
#'   `title`, and `specimen_code`
#' - `specimen_code` stored as a list-column in `character_state_matrix`
#' - `figure_filter` stored as a parsed list-column in `character_information`
#'
#' If `taxRmatic_figures` is supplied, it should be an object returned by
#' [read_taxRmatic_figures()]. The function expects the standard figure columns
#' `figure_number`, `figure_part`, `taxon`, and `specimen_code`, with
#' `specimen_code` stored as a list-column.
#'
#' ## Description hierarchy
#'
#' Descriptions are assembled using metadata stored in the
#' `character_information` component of the
#' `taxRmatic_character_state_matrix` object. Each character is assigned to a
#' paragraph, sentence, and clause.
#'
#' The output hierarchy is:
#'
#' \enumerate{
#'   \item paragraph
#'   \item sentence
#'   \item clause
#'   \item character-state fragment
#' }
#'
#' Within a sentence, character-state fragments belonging to the same clause are
#' joined with commas. Separate clauses are joined with semicolons. Each
#' resulting sentence is capitalised and terminated with a full stop.
#'
#' The `clause` field is mandatory in the standard character-state matrix
#' structure, but individual clause values may be blank. Characters with blank
#' `clause` values are treated as unique clauses.
#'
#' ## Character-state formatting
#'
#' Character-state values are taken from the selected row of the character-state
#' matrix. Empty or missing values are skipped. If the corresponding
#' `character_information` row contains `prefix` or `suffix` values, these are
#' added around the character-state value using [format_text()]. Prefixes and
#' suffixes are only included for non-empty character-state values.
#'
#' ## Paragraph titles
#'
#' Paragraph titles are controlled by `print_paragraph_title` and
#' `format_paragraph_title`. When `print_paragraph_title = TRUE`, the paragraph
#' value is formatted with [format_text()] and added to the start of the
#' paragraph. By default, paragraph titles are italicised in Markdown-style
#' formatting and followed by a full stop.
#'
#' `format_paragraph_title` should be a named list containing any of `case`,
#' `prefix`, and `suffix`. These values are passed to [format_text()]. For
#' example:
#'
#' ```
#' list(case = "capitalise_first", prefix = "*", suffix = ".* ")
#' ```
#'
#' Paragraphs are separated in the final output using `paragraph_separator`.
#' The default, `"  \n\n"`, preserves the previous Markdown-style paragraph
#' separation. Use `" "` to return a single continuous paragraph, or `"\n\n"`
#' for a plain blank-line paragraph break.
#'
#' ## Figure references
#'
#' If a `taxRmatic_figures` object is supplied, the function can append
#' sentence-level figure references automatically. Figure references are built by
#' combining:
#'
#' \itemize{
#'   \item any `specimen_code` values associated with the selected description
#'     row; and
#'   \item parsed `figure_filter` metadata associated with the characters that
#'     contribute to each sentence.
#' }
#'
#' The `taxon` value in the selected description row is not used for automatic
#' specimen-code-based figure matching. Taxon-based filtering can still be
#' applied manually through `figure_filter` values where relevant, provided the
#' relevant filter column is present in `taxRmatic_figures`.
#'
#' Figure references are only attempted when `taxRmatic_figures` is supplied,
#' the selected row contains at least one non-empty `specimen_code`, at least one
#' contributing character has non-empty `figure_filter` metadata, and all filter
#' columns referred to by those figure filters are present in
#' `taxRmatic_figures`.
#'
#' If required filter columns are missing, figure references are skipped for the
#' description and a warning is emitted. If figure filtering fails for an
#' individual sentence, that sentence is returned without a figure reference and
#' a warning is emitted.
#'
#' ## Final cleaning
#'
#' After all paragraphs are assembled, they are joined using
#' `paragraph_separator`. The complete description is then passed through
#' [clean_taxRmatic_text()] and leading/trailing whitespace is removed.
#'
#' @section Using this function:
#' Use this when you want to inspect or generate **one** description from the character matrix, rather than all of them. Read the matrix first with `read_taxRmatic_character_state_matrix()` and provide the exact `title` of the desired description row.
#'
#' The wording and character order come from the matrix instruction rows. Optional figure metadata can add references for the described taxon or specimen. For every description row, use `get_taxRmatic_descriptions()` instead.
#'
#' @param taxRmatic_character_state_matrix An object of class
#'   `"taxRmatic_character_state_matrix"`, as returned by
#'   [read_taxRmatic_character_state_matrix()].
#' @param taxRmatic_figures `NULL` or an object of class
#'   `"taxRmatic_figures"`, as returned by [read_taxRmatic_figures()]. If
#'   supplied, sentence-level figure references may be appended automatically.
#' @param title Character string. Value in the `title` column identifying the
#'   single description row to convert into text.
#' @param format_paragraph_title Named list of formatting options passed to
#'   [format_text()] when formatting paragraph titles. Supported elements are
#'   `case`, `prefix`, and `suffix`.
#' @param print_paragraph_title Logical. If `TRUE` (default), paragraph titles
#'   are included in the output. If `FALSE`, paragraph titles are omitted.
#' @param paragraph_separator Character string. Text used to separate paragraphs
#'   in the returned description. The default, `"  \n\n"`, produces
#'   Markdown-style paragraph separation with a line break. Use `" "` to return
#'   the description as a single continuous paragraph, or `"\n\n"` for a plain
#'   blank-line paragraph break.
#' @param figure_reference_style Character string. Figure-reference formatting
#'   style passed to [format_figure_reference_text()]. Must be one of `"full"` or
#'   `"compact"`.
#' @param figure_prefix_singular Character string. Prefix used when a figure
#'   reference resolves to a single figure, e.g. `"Fig."` or `"Figure"`.
#' @param figure_prefix_plural Character string. Prefix used when a figure
#'   reference resolves to multiple figures, e.g. `"Figs."` or `"Figures"`.
#'
#' @return Character string. A formatted natural-language description.
#'
#' @examples
#' \dontrun{
#' desc <- get_taxRmatic_description(
#'   taxRmatic_character_state_matrix = x,
#'   title = "male"
#' )
#'
#' desc <- get_taxRmatic_description(
#'   taxRmatic_character_state_matrix = x,
#'   taxRmatic_figures = figs,
#'   title = "female",
#'   figure_reference_style = "compact"
#' )
#'
#' desc <- get_taxRmatic_description(
#'   taxRmatic_character_state_matrix = x,
#'   title = "female",
#'   paragraph_separator = " "
#' )
#'
#' desc <- get_taxRmatic_description(
#'   taxRmatic_character_state_matrix = x,
#'   title = "female",
#'   format_paragraph_title = list(
#'     case = "uppercase",
#'     prefix = "**",
#'     suffix = ".** "
#'   )
#' )
#' }
#'
#' @param .validated Internal use only; skip repeated validation in batch calls.
#' @param .row_index Internal use only; preselected description row.
#' @param .figure_index Internal use only; optional named list mapping specimen
#'   codes to figure-row indices, prepared once by get_taxRmatic_descriptions().
#' @export
get_taxRmatic_description <- function(
    taxRmatic_character_state_matrix,
    taxRmatic_figures = NULL,
    title,
    format_paragraph_title = list(case = "capitalise_first", prefix = "*", suffix = ".* "),
    print_paragraph_title = TRUE,
    paragraph_separator = "  \n\n",
    figure_reference_style = c("full", "compact"),
    figure_prefix_singular = "Fig.",
    figure_prefix_plural = "Figs.",
    .validated = FALSE,
    .row_index = NULL,
    .figure_index = NULL
) {

  # VALIDATE ARGUMENTS ---------------------------------------------------------

  figure_reference_style <- match.arg(figure_reference_style)

  # Batch calls have already been validated by get_taxRmatic_descriptions().
  if (!.validated) {
    if (!inherits(taxRmatic_character_state_matrix, "taxRmatic_character_state_matrix")) {
      stop(
        "Object supplied to `taxRmatic_character_state_matrix` must be of class ",
        "'taxRmatic_character_state_matrix'.",
        call. = FALSE
      )
    }

    if (!is.null(taxRmatic_figures) &&
        !inherits(taxRmatic_figures, "taxRmatic_figures")) {
      stop(
        "Object supplied to `taxRmatic_figures` must be of class ",
        "'taxRmatic_figures'.",
        call. = FALSE
      )
    }

    if (!is.character(title) ||
        length(title) != 1 ||
        is.na(title) ||
        !nzchar(trimws(title))) {
      stop("`title` must be a single non-empty character string.", call. = FALSE)
    }

    if (!is.logical(print_paragraph_title) ||
        length(print_paragraph_title) != 1 ||
        is.na(print_paragraph_title)) {
      stop("`print_paragraph_title` must be TRUE or FALSE.", call. = FALSE)
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
      stop("`format_paragraph_title` must be a named list.", call. = FALSE)
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

  }

  title <- trimws(title)

  # EXTRACT AND VALIDATE OBJECT STRUCTURE --------------------------------------

  character_info <- taxRmatic_character_state_matrix$character_information
  state_matrix <- taxRmatic_character_state_matrix$character_state_matrix

  if (!.validated) {
    if (!is.data.frame(character_info)) {
      stop(
        "`taxRmatic_character_state_matrix$character_information` must be a data frame. ",
        "Re-import the file with read_taxRmatic_character_state_matrix().",
        call. = FALSE
      )
    }

    if (!is.data.frame(state_matrix)) {
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
      colnames(state_matrix)
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

    if (!is.list(state_matrix$specimen_code)) {
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

  }

  # SELECT DESCRIPTION ROW -----------------------------------------------------

  if (is.null(.row_index)) {
    # Standalone calls retain strict title matching.
    matching_rows <- which(trimws(as.character(state_matrix$title)) == title)
    if (!length(matching_rows)) {
      stop("No description found for title: ", title, call. = FALSE)
    }
    if (length(matching_rows) > 1L) {
      stop("Multiple description rows found for title: ", title, call. = FALSE)
    }
    .row_index <- matching_rows
  }

  # The batch wrapper supplies the known row index, avoiding a full scan.
  states <- state_matrix[.row_index, , drop = FALSE]

  # EXTRACT SPECIMEN CODES -----------------------------------------------------
  # Specimen codes are already split into a list-column by
  # read_taxRmatic_character_state_matrix().

  specimen_code <- unlist(states$specimen_code, use.names = FALSE)
  specimen_code <- trimws(as.character(specimen_code))
  specimen_code <- specimen_code[!is.na(specimen_code) & nzchar(specimen_code)]
  specimen_code <- unique(specimen_code)

  # KEEP NON-EMPTY CHARACTER STATES -------------------------------------------
  # Retain only columns corresponding to printable characters in
  # character_information, and only those with a non-empty value in the selected
  # description row.

  character_cols <- row.names(character_info)[
    row.names(character_info) %in% colnames(states)
  ]

  state_values_present <- vapply(
    states[character_cols],
    function(x) {
      value <- x[[1]]

      if (is.list(value)) {
        value <- unlist(value, use.names = FALSE)
      }

      if (is.null(value) || length(value) == 0) {
        return(FALSE)
      }

      value <- trimws(as.character(value))
      any(!is.na(value) & nzchar(value))
    },
    logical(1)
  )

  states <- states[
    ,
    character_cols[state_values_present],
    drop = FALSE
  ]

  character_info <- character_info[
    colnames(states),
    ,
    drop = FALSE
  ]

  # STANDARDISE CLAUSE VALUES --------------------------------------------------
  # The clause field is mandatory, but blank values are allowed. Blank clause
  # values are treated as unique clauses using the character name.

  empty_clause <- is.na(character_info$clause) |
    !nzchar(trimws(character_info$clause))

  character_info$clause <- trimws(character_info$clause)

  character_info$clause[empty_clause] <-
    row.names(character_info)[empty_clause]

  # CHECK FIGURE FILTER COLUMNS ------------------------------------------------
  # Figure linking is specimen-code based. The taxon value from the selected
  # description row is not added automatically to the figure filters.

  figure_linking_allowed <- !is.null(taxRmatic_figures) &&
    length(specimen_code) > 0

  if (figure_linking_allowed) {
    filter_columns <- unique(unlist(
      lapply(character_info$figure_filter, collect_filter_columns),
      use.names = FALSE
    ))

    filter_columns <- filter_columns[
      !is.na(filter_columns) & nzchar(filter_columns)
    ]

    missing_cols <- setdiff(filter_columns, colnames(taxRmatic_figures))

    if (length(missing_cols)) {
      warning(
        "Description '", title,
        "' includes figure_filter columns not present in `taxRmatic_figures`: ",
        paste(missing_cols, collapse = ", "),
        ". Figure references skipped.",
        call. = FALSE
      )

      figure_linking_allowed <- FALSE
    }
  }

  # Match specimen codes once per description, rather than repeating this
  # list-column search for every sentence containing figure filters.
  # Keep the complete figures table separately for whole-figure detection.
  description_figures <- NULL
  if (figure_linking_allowed) {
    if (is.null(.figure_index)) {
      # Standalone call: retain the original specimen matching behaviour.
      specimen_matches <- matches_filter(
        taxRmatic_figures$specimen_code, specimen_code
      )
      description_figures <- taxRmatic_figures[
        specimen_matches, , drop = FALSE
      ]
    } else {
      # Batch call: look up matching rows in the shared specimen-to-figure index.
      # Sort/unique preserves source figure order and avoids duplicate records
      # where several codes refer to the same figure.
      figure_rows <- sort(unique(unlist(
        .figure_index[specimen_code], use.names = FALSE
      )))
      description_figures <- taxRmatic_figures[
        figure_rows, , drop = FALSE
      ]
    }
  }

  # BUILD DESCRIPTION ----------------------------------------------------------

  desc_parts <- character()

  for (p in unique(character_info[["paragraph"]])) {

    p_info <- character_info[
      character_info[["paragraph"]] == p,
      ,
      drop = FALSE
    ]

    para_parts <- character()

    if (print_paragraph_title) {
      para_parts <- c(
        format_text(
          p,
          style = format_paragraph_title
        )
      )
    }

    for (s in unique(p_info[["sentence"]])) {

      sentence_info <- p_info[
        p_info[["sentence"]] == s,
        ,
        drop = FALSE
      ]

      clause_parts <- character()
      sentence_filters <- list()

      for (cl in unique(sentence_info[["clause"]])) {

        clause_info <- sentence_info[
          sentence_info[["clause"]] == cl,
          ,
          drop = FALSE
        ]

        char_parts <- character()

        for (i in seq_len(nrow(clause_info))) {

          char <- row.names(clause_info)[i]
          value <- states[[char]][1]

          if (is.list(value)) {
            value <- unlist(value, use.names = FALSE)
          }

          if (is.null(value) || length(value) == 0) {
            next
          }

          value <- trimws(as.character(value))[1]

          if (!is.na(value) && nzchar(value)) {

            prefix <- trimws(as.character(clause_info[i, "prefix"]))
            suffix <- trimws(as.character(clause_info[i, "suffix"]))

            desc <- format_text(
              value,
              prefix = if (!is.na(prefix) && nzchar(prefix)) {
                paste0(prefix, " ")
              } else {
                NULL
              },
              suffix = if (!is.na(suffix) && nzchar(suffix)) {
                paste0(" ", suffix)
              } else {
                NULL
              }
            )

            char_parts <- c(char_parts, desc)

            if (figure_linking_allowed) {

              char_filter <- clause_info$figure_filter[[i]]

              if (!is.null(char_filter) && length(char_filter) > 0) {

                # Specimen matching has already been applied to
                # description_figures. Keep any specimen_code filter that
                # the user explicitly supplied (it further restricts matches).
                sentence_filters <- c(sentence_filters, char_filter)
              }
            }
          }
        }

        if (length(char_parts)) {
          clause_parts <- c(
            clause_parts,
            paste(char_parts, collapse = ", ")
          )
        }
      }

      if (length(clause_parts)) {

        sent_text <- paste(clause_parts, collapse = "; ")
        sent_text <- format_text(sent_text, case = "capitalise_first")

        if (figure_linking_allowed && length(sentence_filters)) {

          fig_text <- tryCatch(
            format_figure_reference_text(
              taxRmatic_figures = description_figures,
              filter = sentence_filters,
              .all_figures = taxRmatic_figures,
              figure_reference_style = figure_reference_style,
              error_if_empty = FALSE,
              figure_prefix_singular = figure_prefix_singular,
              figure_prefix_plural = figure_prefix_plural
            ),
            error = function(e) {
              warning(
                "Figure references could not be generated for description '",
                title,
                "': ",
                conditionMessage(e),
                call. = FALSE
              )

              NULL
            }
          )

          if (!is.null(fig_text)) {
            sent_text <- paste0(sent_text, " ", fig_text)
          }
        }

        sent_text <- sub("\\.$", "", sent_text)

        para_parts <- c(
          para_parts,
          paste0(sent_text, ".")
        )
      }
    }

    paragraph <- paste(para_parts, collapse = " ")

    desc_parts <- c(desc_parts, paragraph)
  }

  # CLEAN AND RETURN -----------------------------------------------------------

  description <- paste(desc_parts, collapse = paragraph_separator)
  description <- clean_taxRmatic_text(description)

  trimws(description)
}
