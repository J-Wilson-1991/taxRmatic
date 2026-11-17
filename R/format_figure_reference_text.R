#' Normalise filter values to a clean character vector
#'
#' Internal helper to flatten, trim, and de-duplicate filter values.
#'
#' @param x Vector or list of values.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
normalise_filter_values <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }

  if (is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }

  x <- as.character(x)
  x <- trimws(x)
  x <- x[!is.na(x) & nzchar(x)]

  unique(x)
}


#' Test whether values match a filter
#'
#' Internal helper to evaluate whether entries in a column match a set of filter
#' values. Supports list-columns.
#'
#' @param column Vector or list-column from a data frame.
#' @param filter_values Values to match against.
#'
#' @return Logical vector.
#' @keywords internal
#' @noRd
matches_filter <- function(column, filter_values) {
  filter_values <- normalise_filter_values(filter_values)

  if (length(filter_values) == 0) {
    return(rep(FALSE, length(column)))
  }

  if (is.list(column)) {
    return(vapply(
      column,
      function(cell_values) {
        cell_values <- normalise_filter_values(cell_values)
        any(cell_values %in% filter_values)
      },
      logical(1)
    ))
  }

  column <- trimws(as.character(column))
  !is.na(column) & nzchar(column) & column %in% filter_values
}


#' Format figure-reference text from figure metadata
#'
#' Filters a `taxRmatic_figures` object and returns formatted figure-reference
#' text, such as `"(Fig. 1A)"`, `"(Figs. 1A–C)"`, or
#' `"(Figs. 1A–C, 2)"`.
#'
#' This is a lower-level utility used by taxRmatic's description and figure-tag
#' workflows. It does not generate figure captions or insert references into
#' manuscript text.
#'
#' @details
#' ## Filtering
#'
#' The `filter` argument can be supplied as either:
#'
#' - a named list of filters, interpreted as one filter set using AND logic; or
#' - a list of named filter lists, interpreted using OR logic across filter sets
#'   and AND logic within each filter set.
#'
#' In a single named filter list, different columns are combined using AND logic.
#' For example:
#'
#' `#' list(taxon = "Exemplomygale currycomboides", feature = "male palp") #'`
#'
#' matches figure records where `taxon` is `"Exemplomygale currycomboides"` and
#' `feature` is `"male palp"`.
#'
#' Multiple accepted values can be supplied for one column. Values within the
#' same column are interpreted using OR logic. For example:
#'
#' `#' list(feature = c("male palp", "spermathecae")) #'`
#'
#' matches figure records where `feature` is either `"male palp"` or
#' `"spermathecae"`.
#'
#' These can be combined. For example:
#'
#' `#' list( #'   taxon = "Exemplomygale currycomboides", #'   feature = c("male palp", "spermathecae") #' ) #'`
#'
#' matches figure records where `taxon` is `"Exemplomygale currycomboides"` and
#' `feature` is either `"male palp"` or `"spermathecae"`.
#'
#' A list of named filter lists can be used to express alternative filter sets.
#' The alternative sets are combined using OR logic, while conditions within each
#' set are combined using AND logic. For example:
#'
#' `#' list( #'   list(sex = "male", feature = "male palp"), #'   list(sex = "female", feature = "spermathecae") #' ) #'`
#'
#' matches figure records where `(sex == "male" AND feature == "male palp")` OR
#' `(sex == "female" AND feature == "spermathecae")`.
#'
#' Filter columns may be ordinary character columns or list-columns. This allows
#' the function to work with metadata imported by [read_taxRmatic_figures()],
#' including automatically split `specimen_code` values and user-selected
#' multi-value metadata columns.
#'
#' ## Figure-reference styles
#'
#' If `figure_reference_style = "full"`, each selected figure record is listed
#' individually, for example `"1A, 1B, 1C"`.
#'
#' If `figure_reference_style = "compact"`, consecutive figure numbers or
#' figure parts are collapsed into ranges where possible, for example
#' `"1A–C"`.
#'
#' If all labelled parts of a figure are selected, the part labels are dropped
#' and the whole figure is cited instead. For example, if all parts of Figure 1
#' are selected, the reference is simplified to `"Fig. 1"` rather than listing
#' every part.
#'
#' @section When this function is useful:
#' This function formats references for figures that match the metadata or filter you supply, for example `Fig. 2A–C`. Most users will not need to call it directly: description, diagnosis and compilation functions use it internally when they add figure references.
#'
#' If a reference is missing or too broad, check the figure table and filter rather than typing the figure number into the description. See `parse_figure_filter()` and the vignette on preparing CSV data.
#'
#' @param taxRmatic_figures An object of class `"taxRmatic_figures"`, as returned
#'   by [read_taxRmatic_figures()]. If `NULL` or empty, the function returns
#'   `NULL` unless `error_if_empty = TRUE`.
#' @param filter `NULL`, a named list of filters, or a list of named filter
#'   lists. A single named list is interpreted using AND logic. A list of named
#'   lists is interpreted using OR logic across filter sets and AND logic within
#'   each set.
#' @param figure_reference_style Character string. Figure-reference formatting
#'   style. Must be one of `"full"` or `"compact"`.
#' @param parentheses Logical. If `TRUE`, wrap the returned figure reference in
#'   parentheses.
#' @param error_if_empty Logical. If `TRUE`, error when no figures are available
#'   or filtering produces no matches. If `FALSE`, return `NULL`.
#' @param figure_prefix_singular Character string. Prefix used when the figure
#'   reference resolves to a single figure, e.g. `"Fig."` or `"Figure"`.
#' @param figure_prefix_plural Character string. Prefix used when the figure
#'   reference resolves to multiple figures, e.g. `"Figs."` or `"Figures"`.
#'
#' @param .all_figures Internal use only. Complete figure table used to
#'   determine whether all parts of a figure have been selected. Defaults to
#'   `taxRmatic_figures`.
#'
#' @return Character string containing formatted figure-reference text, or
#'   `NULL` if no matching figures are found and `error_if_empty = FALSE`.
#'
#' @examples
#' \dontrun{
#' # One-column filter:
#' # match all records for one taxon.
#' format_figure_reference_text(
#'   taxRmatic_figures = figs,
#'   filter = list(taxon = "Exemplomygale currycomboides")
#' )
#'
#' # Single named list, AND across columns:
#' # match records for one taxon that also show the male palp.
#' format_figure_reference_text(
#'   taxRmatic_figures = figs,
#'   filter = list(
#'     taxon = "Exemplomygale currycomboides",
#'     feature = "male palp"
#'   )
#' )
#'
#' # Single named list, OR within one column:
#' # match records showing either the male palp or the spermathecae.
#' format_figure_reference_text(
#'   taxRmatic_figures = figs,
#'   filter = list(
#'     feature = c("male palp", "spermathecae")
#'   ),
#'   figure_reference_style = "compact"
#' )
#'
#' # Single named list, AND across columns plus OR within one column:
#' # match one taxon, but allow either of two features.
#' format_figure_reference_text(
#'   taxRmatic_figures = figs,
#'   filter = list(
#'     taxon = "Exemplomygale currycomboides",
#'     feature = c("male palp", "spermathecae")
#'   ),
#'   figure_reference_style = "compact"
#' )
#'
#' # List of named lists, OR across filter sets:
#' # match either male palp records or spermathecae records.
#' format_figure_reference_text(
#'   taxRmatic_figures = figs,
#'   filter = list(
#'     list(feature = "male palp"),
#'     list(feature = "spermathecae")
#'   ),
#'   figure_reference_style = "compact"
#' )
#'
#' # List of named lists, OR across sets and AND within each set:
#' # match male palps from males, or spermathecae from females.
#' format_figure_reference_text(
#'   taxRmatic_figures = figs,
#'   filter = list(
#'     list(sex = "male", feature = "male palp"),
#'     list(sex = "female", feature = "spermathecae")
#'   ),
#'   figure_reference_style = "compact"
#' )
#'
#' # A reference without parentheses.
#' format_figure_reference_text(
#'   taxRmatic_figures = figs,
#'   filter = list(feature = "male palp"),
#'   parentheses = FALSE
#' )
#' }
#'
#' @export
format_figure_reference_text <- function(
    taxRmatic_figures,
    filter = NULL,
    figure_reference_style = c("full", "compact"),
    parentheses = TRUE,
    error_if_empty = FALSE,
    figure_prefix_singular = "Fig.",
    figure_prefix_plural = "Figs.",
    .all_figures = NULL
) {

  # VALIDATION -----------------------------------------------------------------

  figure_reference_style <- match.arg(figure_reference_style)

  if (!is.logical(parentheses) ||
      length(parentheses) != 1 ||
      is.na(parentheses)) {
    stop("`parentheses` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(error_if_empty) ||
      length(error_if_empty) != 1 ||
      is.na(error_if_empty)) {
    stop("`error_if_empty` must be TRUE or FALSE.", call. = FALSE)
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

  if (is.null(taxRmatic_figures) || nrow(taxRmatic_figures) == 0) {
    if (error_if_empty) {
      stop("No figures available in `taxRmatic_figures`.", call. = FALSE)
    }

    return(NULL)
  }

  if (!inherits(taxRmatic_figures, "taxRmatic_figures")) {
    stop(
      "`taxRmatic_figures` must be an object of class 'taxRmatic_figures', ",
      "as returned by read_taxRmatic_figures().",
      call. = FALSE
    )
  }

  required_cols <- c("figure_number", "figure_part")

  missing_cols <- setdiff(required_cols, colnames(taxRmatic_figures))

  if (length(missing_cols) > 0) {
    stop(
      "`taxRmatic_figures` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  figures <- taxRmatic_figures

  # FILTERING ------------------------------------------------------------------
  # Avoid scanning already-excluded rows for each additional condition.
  # A single named list is treated as one AND filter set.
  # A list of named lists is treated as OR across filter sets.

  if (!is.null(filter)) {

    if (!is.list(filter)) {
      stop(
        "`filter` must be either a named list or a list of named lists.",
        call. = FALSE
      )
    }

    is_single_filter <- !is.null(names(filter))

    if (is_single_filter) {
      filter <- list(filter)
    } else {
      valid <- vapply(
        filter,
        function(x) is.list(x) && !is.null(names(x)),
        logical(1)
      )

      if (!all(valid)) {
        stop(
          "`filter` must be either a named list or a list of named lists.",
          call. = FALSE
        )
      }
    }

    # Check all filter columns before matching so invalid filters still error,
    # even when earlier conditions would have produced no matches.
    filter_names <- unique(unlist(lapply(filter, names), use.names = FALSE))
    for (nm in filter_names) {
      if (!nm %in% colnames(figures)) {
        stop(
          "Column '", nm, "' not found in `taxRmatic_figures`.",
          call. = FALSE
        )
      }
    }

    n_figures <- nrow(figures)
    keep_any <- rep(FALSE, n_figures)

    for (filter_set in filter) {

      # Once a row matches an alternative (OR) filter set, there is no need
      # to evaluate it against subsequent sets.
      candidates <- which(!keep_any)
      if (!length(candidates)) break

      # Each condition is ANDed with previous conditions. Evaluate only rows
      # that still qualify, especially valuable for specimen_code list-columns.
      for (nm in names(filter_set)) {
        if (!length(candidates)) break
        matched <- matches_filter(figures[[nm]][candidates], filter_set[[nm]])
        candidates <- candidates[matched]
      }

      if (length(candidates)) keep_any[candidates] <- TRUE
    }

    figures <- figures[keep_any, , drop = FALSE]
  }

  if (nrow(figures) == 0) {
    if (error_if_empty) {
      stop("Filtering resulted in zero matching figures.", call. = FALSE)
    }

    return(NULL)
  }

  # Use the complete figure universe for whole-figure reference decisions.
  # Normally this is identical to the filtering table; description generation
  # can instead pass a smaller, specimen-matched table for filtering.
  if (is.null(.all_figures)) {
    .all_figures <- taxRmatic_figures
  }

  # REMOVE DUPLICATES ----------------------------------------------------------
  # Preserve the first record for each figure-number/part combination.

  figure_key <- paste0(figures$figure_number, "___", figures$figure_part)
  figures <- figures[!duplicated(figure_key), , drop = FALSE]

  # DROP PARTS WHEN ALL PARTS ARE REFERENCED -----------------------------------
  # Compare distinct, non-empty part labels for each figure. Work with grouped
  # indices instead of repeatedly scanning and rebuilding the whole figure table.

  all_key <- paste0(.all_figures$figure_number, "___",
                    .all_figures$figure_part)
  unique_all <- !duplicated(all_key)

  all_parts_by_figure <- split(
    .all_figures$figure_part[unique_all],
    .all_figures$figure_number[unique_all]
  )
  selected_indices <- split(seq_len(nrow(figures)), figures$figure_number)

  for (fig_num in names(selected_indices)) {
    idx <- selected_indices[[fig_num]]

    all_parts <- all_parts_by_figure[[fig_num]]
    all_parts <- trimws(all_parts[!is.na(all_parts)])
    all_parts <- unique(all_parts[nzchar(all_parts)])

    selected_parts <- figures$figure_part[idx]
    selected_parts <- trimws(selected_parts[!is.na(selected_parts)])
    selected_parts <- unique(selected_parts[nzchar(selected_parts)])

    if (length(all_parts) > 1L &&
        setequal(selected_parts, all_parts)) {
      figures$figure_part[idx] <- ""
    }
  }

  figure_key <- paste0(figures$figure_number, "___", figures$figure_part)
  figures <- figures[!duplicated(figure_key), , drop = FALSE]

  # SORTING --------------------------------------------------------------------

  figures <- figures[
    order(
      suppressWarnings(as.numeric(figures$figure_number)),
      ifelse(is.na(figures$figure_part), "", figures$figure_part)
    ),
    ,
    drop = FALSE
  ]

  # BUILD TEXT USING COLLAPSE RANGES -------------------------------------------

  if (figure_reference_style == "full") {

    ref_text <- paste0(
      figures$figure_number,
      figures$figure_part,
      collapse = ", "
    )

  } else {

    has_parts <- any(
      !is.na(figures$figure_part) &
        nzchar(trimws(figures$figure_part))
    )

    if (has_parts) {

      fig_nums <- unique(figures$figure_number)

      figure_refs <- vapply(
        fig_nums,
        function(fig_num) {

          parts <- figures$figure_part[figures$figure_number == fig_num]
          parts <- parts[!is.na(parts)]
          parts <- trimws(parts)
          parts <- parts[nzchar(parts)]

          if (length(parts) == 0) {
            return(as.character(fig_num))
          }

          paste0(fig_num, collapse_ranges(parts))
        },
        character(1)
      )

      ref_text <- paste(figure_refs, collapse = ", ")

    } else {

      fig_nums <- unique(figures$figure_number)
      fig_nums <- as.character(sort(suppressWarnings(as.numeric(fig_nums))))

      ref_text <- collapse_ranges(fig_nums)
    }
  }

  # ADD PREFIXES ---------------------------------------------------------------

  n_figs <- length(unique(figures$figure_number))

  prefix <- if (n_figs == 1) {
    figure_prefix_singular
  } else {
    figure_prefix_plural
  }

  full_text <- paste0(prefix, " ", ref_text)

  if (parentheses) {
    full_text <- paste0("(", full_text, ")")
  }

  full_text
}
