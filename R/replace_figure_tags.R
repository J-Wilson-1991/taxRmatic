#' Replace figure tags with formatted figure references
#'
#' Replaces figure tags in text with formatted figure references.
#'
#' Figure tags are placeholders written directly into manuscript text. They allow
#' users to refer to figures by filtering the figure metadata, rather than typing
#' figure numbers manually.
#'
#' A figure tag has the form:
#'
#' `fg{column = value}`
#'
#' where `column` is a column in the figure metadata and `value` is the value to
#' match.
#'
#' For example:
#'
#' `fg{taxon = Exemplomygale aculeatus}`
#'
#' will be replaced with a formatted figure reference for figures matching that
#' taxon.
#'
#' More specific tags can be made by combining filters. Use:
#'
#' \itemize{
#'   \item `&` to require more than one condition to be matched
#'   \item uppercase `OR` to allow more than one accepted value within a column
#'   \item `;` to separate alternative filter sets
#' }
#'
#' For example:
#'
#' `fg{taxon = Exemplomygale aculeatus & sex = male}`
#'
#' matches figures for male specimens of `Exemplomygale aculeatus`.
#'
#' This tag:
#'
#' `fg{structure = fang OR chelicera & view = anterior}`
#'
#' matches figures where `structure` is either `fang` or `chelicera`, and `view`
#' is `anterior`.
#'
#' This tag:
#'
#' `fg{taxon = Exemplomygale aculeatus & sex = male; taxon = Exemplomygale eboracum & sex = female}`
#'
#' contains two alternative filter sets. It matches either male figures of
#' `Exemplomygale aculeatus` or female figures of `Exemplomygale eboracum`.
#'
#' If a tag cannot be understood, or if it refers to columns that are not present
#' in `taxRmatic_figures`, the original tag is left unchanged and a warning is
#' issued. This makes problems easier to find without silently deleting text.
#'
#' @section Using figure placeholders:
#' You can write a placeholder such as `fg{tag = demo_map}` inside a remarks or etymology paragraph. The placeholder identifies figure metadata instead of a fixed number, so figure renumbering does not require editing that paragraph.
#'
#' Normally you do not need to call this function directly: `finalise_taxRmatic_text()` can replace the tags while preparing the complete manuscript.
#'
#' @param text Character(1). Text containing zero or more `fg{...}` figure tags.
#' @param taxRmatic_figures A `taxRmatic_figures` object containing the figure
#'   metadata used to resolve figure tags.
#' @param figure_reference_style Character(1). Figure-reference style to use.
#'   Must be either `"full"` or `"compact"`.
#' @param figure_prefix_singular Character string. Prefix used when a figure
#'   reference resolves to a single figure, e.g. `"Fig."` or `"Figure"`.
#' @param figure_prefix_plural Character string. Prefix used when a figure
#'   reference resolves to multiple figures, e.g. `"Figs."` or `"Figures"`.
#'
#' @return Character(1). The input text with recognised `fg{...}` tags replaced
#'   by formatted figure references.
#'
#' @examples
#' \dontrun{
#' text <- "Male palp fg{taxon = Exemplomygale aculeatus & sex = male}."
#'
#' replace_figure_tags(
#'   text = text,
#'   taxRmatic_figures = figures,
#'   figure_reference_style = "compact"
#' )
#'
#' replace_figure_tags(
#'   text = text,
#'   taxRmatic_figures = figures,
#'   figure_reference_style = "compact",
#'   figure_prefix_singular = "Figure",
#'   figure_prefix_plural = "Figures"
#' )
#'
#' text <- paste(
#'   "Cheliceral morphology",
#'   "fg{structure = fang OR chelicera & view = anterior}."
#' )
#'
#' replace_figure_tags(
#'   text = text,
#'   taxRmatic_figures = figures,
#'   figure_reference_style = "full"
#' )
#' }
#'
#' @export
replace_figure_tags <- function(
    text,
    taxRmatic_figures,
    figure_reference_style = c("full", "compact"),
    figure_prefix_singular = "Fig.",
    figure_prefix_plural = "Figs."
) {

  # STANDARDISE INPUT ----------------------------------------------------------

  figure_reference_style <- match.arg(figure_reference_style)

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

  # Match fg{...} tags.
  # This pattern captures the shortest block beginning with 'fg{' and ending at
  # the next '}'.
  fg_tag_pattern <- "fg\\{[^}]*\\}"

  # PARSE A SINGLE TAG ---------------------------------------------------------
  # Convert the contents of an fg{...} tag into a list of filter sets.
  #
  # Filter grammar:
  # - ';' separates alternative filter sets, treated as OR
  # - '&' separates column filters within a set, treated as AND
  # - 'OR' separates accepted values within one column
  #
  # Example:
  # fg{feature = fang OR chelicera & view = anterior; specimen_code = WAM123}
  #
  # becomes:
  # list(
  #   list(feature = c("fang", "chelicera"), view = "anterior"),
  #   list(specimen_code = "WAM123")
  # )
  parse_tag <- function(tag_content) {

    filter_sets <- strsplit(tag_content, ";", fixed = TRUE)[[1]]
    filter_sets <- trimws(filter_sets)
    filter_sets <- filter_sets[nzchar(filter_sets)]

    parsed_filters <- list()

    for (filter_set in filter_sets) {

      parts <- strsplit(filter_set, "&", fixed = TRUE)[[1]]
      parts <- trimws(parts)
      parts <- parts[nzchar(parts)]

      filter <- list()

      for (part in parts) {

        # Split only on the first '=' so values themselves are not broken if
        # they happen to contain additional '=' characters.
        eq_pos <- regexpr("=", part, fixed = TRUE)

        if (eq_pos[1] == -1) {
          next
        }

        col <- trimws(substr(part, 1, eq_pos[1] - 1))
        val <- trimws(substr(part, eq_pos[1] + 1, nchar(part)))

        # Skip malformed clauses with an empty column or value.
        if (!nzchar(col) || !nzchar(val)) {
          next
        }

        # Split multiple accepted values joined by 'OR'. Optional surrounding
        # spaces are tolerated.
        vals <- trimws(unlist(strsplit(val, "\\s+OR\\s+", perl = TRUE)))
        vals <- vals[nzchar(vals)]

        if (length(vals)) {
          filter[[col]] <- vals
        }
      }

      if (length(filter) > 0) {
        parsed_filters[[length(parsed_filters) + 1]] <- filter
      }
    }

    if (length(parsed_filters) == 0) {
      return(NULL)
    }

    parsed_filters
  }

  # FIND TAGS ------------------------------------------------------------------
  # Locate all fg{...} tags and record their positions so replacements can be
  # made safely without relying on repeated regex substitution.

  m <- gregexpr(fg_tag_pattern, text, perl = TRUE)
  starts <- m[[1]]

  if (length(starts) == 1 && starts[1] == -1) {
    return(text)
  }

  lens <- attr(m[[1]], "match.length")

  # REPLACE TAGS ---------------------------------------------------------------
  # Work backwards through the matches so earlier character positions are not
  # affected by later replacements.

  for (k in rev(seq_along(starts))) {

    start <- starts[k]
    end <- start + lens[k] - 1
    fg_tag <- substr(text, start, end)

    # Extract the contents inside fg{...}.
    tag_content <- sub("^fg\\{(.*)\\}$", "\\1", fg_tag)
    filter <- parse_tag(tag_content)

    replacement <- fg_tag

    if (is.null(filter)) {

      warning(
        "fg{} tag skipped (could not parse filter): '",
        fg_tag,
        "'"
      )

    } else {

      # Check that every requested filter column is present in the figures data.
      requested_cols <- unique(unlist(lapply(filter, names)))

      missing_cols <- setdiff(
        requested_cols,
        colnames(taxRmatic_figures)
      )

      if (length(missing_cols) > 0) {

        warning(
          "fg{} tag skipped because these filter columns were not found in ",
          "'taxRmatic_figures': ",
          paste(missing_cols, collapse = ", "),
          ". Tag was: '", fg_tag, "'. Available columns: ",
          paste(colnames(taxRmatic_figures), collapse = ", ")
        )

      } else {

        # Resolve the tag to formatted figure reference text. Parentheses are
        # suppressed here because they are handled at a higher level if needed.
        fig_text <- format_figure_reference_text(
          taxRmatic_figures = taxRmatic_figures,
          filter = filter,
          figure_reference_style = figure_reference_style,
          figure_prefix_singular = figure_prefix_singular,
          figure_prefix_plural = figure_prefix_plural,
          parentheses = FALSE,
          error_if_empty = FALSE
        )

        # Only replace with resolved text if something was returned. Otherwise,
        # retain the original tag so unresolved cases remain visible.
        if (!is.null(fig_text)) {

          replacement <- fig_text

        } else {

          warning(
            "fg{} tag could not be resolved to any figure reference and was ",
            "left unchanged: '", fg_tag, "'"
          )
        }
      }
    }

    # Rebuild the full string with the current tag replaced.
    text <- paste0(
      if (start > 1) substr(text, 1, start - 1) else "",
      replacement,
      if (end < nchar(text)) {
        substr(text, end + 1, nchar(text))
      } else {
        ""
      }
    )
  }

  text
}
