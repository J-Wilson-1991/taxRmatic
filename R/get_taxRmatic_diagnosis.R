# taxRmatic - generate diagnosis for a single taxon
# Source this script in development, or place it in the package R/ directory.
# Uses existing taxRmatic helper functions for figure formatting and section
# construction. The small .taxrm_* functions below are internal to the
# diagnosis-generation workflow; they are not exported.

# INTERNAL HELPERS -------------------------------------------------------------

.taxrm_sentence <- function(x) {
  x <- trimws(x)
  if (!nzchar(x)) return("")
  x <- paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))
  if (!grepl("[.!?]$", x)) x <- paste0(x, ".")
  x
}

.taxrm_taxa_text <- function(x) {
  x <- unique(x)
  if (!length(x)) return("")
  if (length(x) == 1L) return(x)
  if (length(x) == 2L) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", utils::tail(x, 1L))
}

# Join comparisons in a category into a single sentence. The first clause
# establishes the subject; subsequent clauses can begin with "from those of".
.taxrm_join_comparisons <- function(x) {
  x <- x[nzchar(x)]
  if (!length(x)) return("")
  if (length(x) == 1L) return(x)
  x <- sub("\\.$", "", x)
  paste0(paste(x[-length(x)], collapse = "; "), "; and ", utils::tail(x, 1L), ".")
}

# FIGURE REFERENCES ------------------------------------------------------------
# Use taxRmatic's existing figure-filter syntax and formatter, rather than
# introducing another interpretation of figure_filter.
# Required existing helpers:
#   split_and_trim()
#   parse_figure_filter()
#   format_figure_reference_text()
#   collapse_ranges() (used by the formatter)
#   matches_filter() and normalise_filter_values() (used by the formatter)
#   is_uppercase() and append_range() (used by collapse_ranges)

.taxrm_figure_reference <- function(figures, taxa, category, figure_filter,
                                    figure_reference_style = "compact") {
  if (is.null(figures) || !length(taxa)) return(NULL)

  required_helpers <- c("parse_figure_filter", "format_figure_reference_text")
  missing_helpers <- required_helpers[!vapply(required_helpers, exists,
                                               logical(1), mode = "function")]
  if (length(missing_helpers)) {
    stop("Missing figure helpers: ", paste(missing_helpers, collapse = ", "),
         ". Load the existing taxRmatic helpers before generating figure references.",
         call. = FALSE)
  }

  required_cols <- c("taxon", "specimen_category", "figure_number", "figure_part")
  missing_cols <- setdiff(required_cols, names(figures))
  if (length(missing_cols)) {
    stop("Missing figure columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  # The figure helper requires a taxRmatic_figures object. Accept the usual
  # output of read_taxRmatic_figures(), or an ordinary data.frame from read.csv().
  all_figures <- figures
  if (!inherits(all_figures, "taxRmatic_figures")) {
    if (!is.data.frame(all_figures)) {
      stop("`figures` must be a taxRmatic_figures object or a data frame.",
           call. = FALSE)
    }
    class(all_figures) <- unique(c("taxRmatic_figures", class(all_figures)))
  }

  # The category is an exact figure filter, NOT a display heading. Never
  # include figures of another specimen category if this category is missing
  # from the figure table (e.g. molecular comparisons with no illustrations).
  matched <- matches_filter(all_figures$taxon, taxa)
  if (nzchar(category)) {
    matched <- matched & matches_filter(all_figures$specimen_category, category)
  }
  small_figures <- all_figures[matched, , drop = FALSE]
  if (!nrow(small_figures)) return(NULL)

  # Optional extra conditions: ; = OR, & = AND, uppercase OR inside one column.
  parsed <- if (is.na(figure_filter) || !nzchar(trimws(figure_filter))) {
    NULL
  } else {
    parse_figure_filter(figure_filter)
  }

  format_figure_reference_text(
    taxRmatic_figures = small_figures,
    filter = parsed,
    figure_reference_style = figure_reference_style,
    parentheses = FALSE,
    error_if_empty = FALSE,
    .all_figures = all_figures
  )
}

.taxrm_row_prose <- function(row, focal_taxon, figures, compared = NULL,
                            figure_reference_style = "compact",
                            continuation = FALSE) {
  type <- row$statement_type[[1L]]
  statement <- trimws(row$statement[[1L]])
  intro <- trimws(row$starting_text[[1L]])
  category <- trimws(row$specimen_category[[1L]])
  if (type == "comparison") {
    comparison <- .taxrm_taxa_text(compared)
    linking <- trimws(row$comparison_text[[1L]])
    if (!nzchar(comparison) || !nzchar(linking) || !nzchar(statement)) return("")
    # After the first comparison within a category, omit the repeated subject
    # and introductory wording, retaining the comparison itself. The usual
    # "can be distinguished ... from those of" becomes "from those of".
    # If the linking text has no "from", keep the full wording instead of
    # attempting a potentially ungrammatical shortened clause.
    shortened <- continuation && grepl("\\bfrom\\b", linking,
                                      ignore.case = TRUE)
    if (shortened) {
      linking <- sub("^.*?\\b(from\\b.*)$", "\\1", linking,
                     ignore.case = TRUE, perl = TRUE)
      prose <- paste(linking, comparison, statement)
    } else {
      prose <- paste(intro, focal_taxon, linking, comparison, statement)
    }

    # Both sides use the same optional category match and extra figure_filter.
    # The focal figures and comparison figures are formatted independently,
    # so that a 'cf.' reference has the intended structure.
    focal_ref <- .taxrm_figure_reference(
      figures, focal_taxon, category, row$figure_filter[[1L]],
      figure_reference_style)
    comparison_ref <- .taxrm_figure_reference(
      figures, compared, category, row$figure_filter[[1L]],
      figure_reference_style)

    if (!is.null(focal_ref) || !is.null(comparison_ref)) {
      if (!is.null(focal_ref) && !is.null(comparison_ref)) {
        ref <- paste0(focal_ref, "; cf. ", comparison_ref)
      } else if (!is.null(focal_ref)) {
        ref <- focal_ref
      } else {
        ref <- paste0("cf. ", comparison_ref)
      }
      prose <- paste0(prose, " (", ref, ")")
    }
    # A shortened clause follows a semicolon, so it should start with a
    # lowercase "from", not be capitalised as a standalone sentence.
    if (shortened) {
      prose <- trimws(prose)
      if (!grepl("[.!?]$", prose)) prose <- paste0(prose, ".")
      return(prose)
    }
    return(.taxrm_sentence(prose))
  }

  # Prefixes and suffixes may be complete sentences or sentence fragments.
  # For example, 'are currently unknown' gets its subject inserted. Existing
  # fg{...} tags in these statements are left for the normal finalisation step.
  if (!nzchar(statement)) return("")
  if (grepl("^(are|is|were|was|have|has)\\b", statement,
            ignore.case = TRUE)) {
    subject <- if (nzchar(intro)) paste(intro, focal_taxon) else focal_taxon
    statement <- paste(subject, statement)
  }
  .taxrm_sentence(statement)
}

#' Generate a diagnosis for one taxon
#'
#' Selects comparative statements that collectively distinguish a focal taxon
#' from other taxa in its subset, then assembles the selected statements into
#' readable diagnostic prose. Within each category, comparisons are joined
#' into one semicolon-separated sentence to avoid repeated introductions.
#'
#' @details
#' Input data must have been processed by `read_taxRmatic_diagnostics()`.
#' The reader validates the CSV, splits comma-separated taxon fields into
#' list-columns, and expands the `all` comparison shortcut. This function
#' therefore uses the prepared taxon vectors directly.
#'
#' Comparison statements are chosen iteratively, prioritising the statement
#' covering the most comparison taxa not already covered. Diagnostic categories
#' appear in the order of their first occurrence in the input CSV. Any
#' category-wide prefixes and suffixes are placed around its comparisons.
#' Diagnosis-wide prefixes appear first, and diagnosis-wide suffixes last.
#'
#' Figure references are filtered automatically by focal or comparison taxon.
#' The `specimen_category` column is used as an exact figure filter, independently
#' of the printed heading stored in `subsection`. A category with no matching
#' figure rows yields no automatic figure references for that category.
#' Additional conditions in `figure_filter` are applied as well.
#'
#' For `separate_categories = TRUE`, the optional `subsection` column supplies
#' a heading for each category, such as `Male morphology`. A blank subsection
#' means no heading, even if `specimen_category` is `male`. The heading is read
#' from any row belonging to that subset/category; the CSV reader checks for
#' conflicting nonblank headings. Diagnosis-wide prefixes and suffixes use
#' their own `subsection` values (normally blank).
#'
#' @section How comparisons are selected:
#' Generate one taxon's diagnosis after importing the diagnostics CSV. For each `specimen_category`, the function looks at comparisons written for the focal taxon and selects the statement distinguishing it from the most **not-yet-covered** comparison taxa. It repeats this until no remaining statement adds a new comparison. When two rows cover equally many new taxa, the earlier CSV row is preferred.
#'
#' Use `separate_categories = TRUE` for separate diagnostic paragraphs, such as Male morphology and Female morphology; the printed headings come from `subsection` in the CSV. `FALSE` produces a single paragraph. Examine the returned `text` and `attr(result, "coverage")` to check that every intended comparison is covered. The function selects from your own statements; it does **not** infer diagnostic differences from measurements.
#'
#' @param diagnostics A `taxRmatic_diagnostics` object returned by
#'   `read_taxRmatic_diagnostics()`.
#' @param taxon Full name of the focal taxon, e.g. `"Exemplomygale mainae"`.
#' @param figures Optional `taxRmatic_figures` object containing the figure
#'   metadata and the columns `taxon`, `specimen_category`, `figure_number`,
#'   and `figure_part`.
#' @param subset Optional subset identifier. Needed only if the focal taxon
#'   belongs to more than one subset.
#' @param warn_unresolved Logical; warn when some comparison taxa are not
#'   covered by any selected statement.
#' @param figure_reference_style `"compact"` (default) or `"full"`.
#' @param warn_unmatched_categories Logical; warn when a diagnostic category
#'   does not match any `specimen_category` in the figure data. Unmatched
#'   categories receive no figure references.
#' @param separate_categories Logical. If \code{FALSE} (default), return
#'   the complete diagnosis as one unheaded paragraph. If \code{TRUE},
#'   return one paragraph for the general prefix (if present), one for each
#'   populated diagnostic category (e.g. `male`, `female`, or `COI`),
#'   and one for the general suffix (if present). Category headings come from
#'   the optional `subsection` values in the CSV, not `specimen_category`.
#'   Both category and general prefix/suffix headings may be blank. Multiple
#'   diagnostic statements within one category are combined inside that
#'   category's single output row. In the compiler, each returned row is a separate paragraph. Use
#'   \code{omit_subsection_headings = "Diagnosis"} in
#'   \code{compile_taxon_texts()} to hide category headings while keeping
#'   the paragraphs separate.
#'
#' @return A \code{taxRmatic_section} data frame with columns \code{taxon},
#'   \code{subsection}, and \code{text}. Rows are returned
#'   in printing order. If \code{separate_categories = FALSE}, subsection is
#'   blank and the whole diagnosis occupies one row. If \code{TRUE}, the
#'   subsection names come from the CSV and can be blank. General
#'   prefix/suffix rows retain their own optional subsection names.
#'   Consecutive output rows are not merged during manuscript compilation.
#'   Attributes \code{coverage}, \code{selected_rows}, and \code{subset}
#'   contain additional details.
#'
#' @examples
#' \dontrun{
#' diagnostics <- read_taxRmatic_diagnostics("diagnostics.csv")
#' diagnosis <- get_taxRmatic_diagnosis(
#'   diagnostics, taxon = "Exemplomygale mainae"
#' )
#' diagnosis$text
#' attr(diagnosis, "coverage")
#' }
#'
#' @export
get_taxRmatic_diagnosis <- function(diagnostics, taxon, figures = NULL,
                                    subset = NULL, warn_unresolved = TRUE,
                                    figure_reference_style = "compact",
                                    warn_unmatched_categories = TRUE,
                                    separate_categories = FALSE) {
  # CHECK INPUT ---------------------------------------------------------------

  figure_reference_style <- match.arg(figure_reference_style, c("compact", "full"))
  if (!inherits(diagnostics, "taxRmatic_diagnostics")) {
    stop("`diagnostics` must be a taxRmatic_diagnostics object, ",
         "as returned by read_taxRmatic_diagnostics().", call. = FALSE)
  }
  if (!is.character(taxon) || length(taxon) != 1L || is.na(taxon) || !nzchar(trimws(taxon)))
    stop("'taxon' must be one full species name.", call. = FALSE)
  for (arg in c("warn_unresolved", "warn_unmatched_categories",
                "separate_categories")) {
    value <- get(arg)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (!is.null(subset) &&
      (!is.character(subset) || length(subset) != 1L ||
       is.na(subset))) {
    stop("`subset` must be NULL or a single subset name.", call. = FALSE)
  }
  if (!is.null(figures) && !is.data.frame(figures)) {
    stop("`figures` must be NULL or a figure data frame.", call. = FALSE)
  }

  # PREPARE DIAGNOSTIC RECORDS -----------------------------------------------

  d <- as.data.frame(diagnostics, stringsAsFactors = FALSE)
  # Work on a cleaned local copy; structural validation belongs to the reader.
  cols <- c("taxon_subset", "subsection", "specimen_category", "starting_text", "taxon", "statement_type",
            "comparison_text", "comparison_taxon", "figure_filter", "statement")
  # Taxon columns are already split into list-columns by the reader.
  # Only tidy scalar metadata here; preserve the taxon vectors unchanged.
  scalar_cols <- setdiff(cols, c("taxon", "comparison_taxon"))
  d[scalar_cols] <- lapply(d[scalar_cols], function(z) {
    z <- as.character(z)
    z[is.na(z)] <- ""
    trimws(z)
  })
  d$.original_row <- seq_len(nrow(d)) + 1L
  d$.taxa <- d$taxon
  d$.other_taxa <- d$comparison_taxon

  # FIND FOCAL TAXON AND SUBSET ----------------------------------------------

  candidate_subsets <- unique(d$taxon_subset[vapply(d$.taxa, function(x) taxon %in% x, logical(1L))])
  if (is.null(subset)) {
    if (length(candidate_subsets) != 1L)
      stop("Focal taxon must occur in exactly one subset, or specify 'subset'.", call. = FALSE)
    subset <- candidate_subsets
  }
  d <- d[d$taxon_subset == subset, , drop = FALSE]
  if (!nrow(d)) stop("No data for subset '", subset, "'.", call. = FALSE)
  focal <- vapply(d$.taxa, function(x) taxon %in% x, logical(1L))
  if (!any(focal)) stop("Taxon not present in chosen subset.", call. = FALSE)

  # Every taxon named in either taxon column belongs to the subset.
  # Each diagnostic category has its own comparison pool. This matches the
  # reader's per-subset/per-category expansion of the 'all' shortcut; otherwise
  # taxa present only in a different category cause spurious unresolved alerts.
  category_membership <- function(category_rows) {
    comparisons <- category_rows[category_rows$statement_type == "comparison", , drop = FALSE]
    vals <- unique(c(unlist(comparisons$.taxa, use.names = FALSE),
                     unlist(comparisons$.other_taxa, use.names = FALSE)))
    setdiff(vals, c("", "all"))
  }

  # ASSEMBLE DIAGNOSTIC TEXT ------------------------------------------------

  assembled <- character()
  parts <- list()
  part_subsections <- character()
  used <- integer()
  append_rows <- function(rows) {
    if (!nrow(rows)) return(character())
    vapply(seq_len(nrow(rows)), function(i) {
      used <<- c(used, rows$.original_row[i])
      .taxrm_row_prose(rows[i, , drop = FALSE], taxon, figures,
                       figure_reference_style = figure_reference_style)
    }, character(1L))
  }

  # Diagnosis-wide prefixes.
  prefix_rows <- d[focal & d$statement_type == "prefix" &
                     d$specimen_category == "", , drop = FALSE]
  general_prefix <- append_rows(prefix_rows)
  assembled <- c(assembled, general_prefix)
  if (length(general_prefix)) {
    parts[[length(parts) + 1L]] <- general_prefix
    prefix_headings <- unique(prefix_rows$subsection[nzchar(prefix_rows$subsection)])
    if (length(prefix_headings) > 1L) {
      stop("Diagnosis-wide prefix rows have conflicting 'subsection' headings ",
           "for taxon '", taxon, "'.", call. = FALSE)
    }
    part_subsections <- c(part_subsections,
                          if (length(prefix_headings)) prefix_headings[1L] else "")
  }

  # Categories in the order they first appear in the CSV. Display headings
  # are independent from the specimen categories used for figure filtering.
  categories <- unique(d$specimen_category[nzchar(d$specimen_category)])
  headings <- stats::setNames(vapply(categories, function(category) {
    vals <- unique(d$subsection[d$specimen_category == category])
    vals <- vals[nzchar(vals)]
    if (length(vals)) vals[1L] else ""
  }, character(1L)), categories)
  if (!is.null(figures) && warn_unmatched_categories) {
    if (!"specimen_category" %in% names(figures)) {
      stop("`figures` lacks the `specimen_category` column.", call. = FALSE)
    }
    figure_categories <- normalise_filter_values(figures$specimen_category)
    unmatched <- setdiff(categories, figure_categories)
    if (length(unmatched)) {
      warning("These diagnostic categories do not match a figure specimen_category: ",
              paste(unmatched, collapse = ", "),
              ". No automatic figure references will be attached for unmatched ",
              "categories.", call. = FALSE)
    }
  }
  coverage <- vector("list", length(categories))
  names(coverage) <- categories

  for (category in categories) {
    relevant <- d[d$specimen_category == category, , drop = FALSE]
    membership <- category_membership(relevant)
    needed <- setdiff(membership, taxon)
    local_focal <- vapply(relevant$.taxa, function(x) taxon %in% x, logical(1L))
    category_parts <- append_rows(relevant[local_focal & relevant$statement_type == "prefix", , drop = FALSE])

    candidates <- relevant[local_focal & relevant$statement_type == "comparison", , drop = FALSE]
    covered <- character()
    selected <- integer()
    comparison_prose <- character()
    while (nrow(candidates)) {
      new_comparisons <- lapply(candidates$.other_taxa, function(x) {
        if ("all" %in% x) x <- union(setdiff(membership, taxon), setdiff(x, "all"))
        intersect(setdiff(x, c(taxon, covered)), needed)
      })
      counts <- lengths(new_comparisons)
      if (max(counts) == 0L) break
      chosen <- which.max(counts) # first occurrence breaks ties
      row <- candidates[chosen, , drop = FALSE]
      compared <- new_comparisons[[chosen]]
      used <- c(used, row$.original_row)
      comparison_prose <- c(comparison_prose, .taxrm_row_prose(
        row, taxon, figures, compared, figure_reference_style,
        continuation = length(comparison_prose) > 0L
      ))
      selected <- c(selected, row$.original_row)
      covered <- union(covered, compared)
      candidates <- candidates[-chosen, , drop = FALSE]
    }
    # Comparisons form one concise sentence per category; category prefixes
    # and suffixes remain separate sentences.
    category_parts <- c(category_parts, .taxrm_join_comparisons(comparison_prose))
    unresolved <- setdiff(needed, covered)
    coverage[[category]] <- data.frame(category = category,
                                        total = length(needed), covered = length(covered),
                                        unresolved = paste(unresolved, collapse = ", "),
                                        stringsAsFactors = FALSE)
    if (warn_unresolved && length(unresolved) && any(local_focal)) {
      warning("For ", taxon, " [", category, "], no comparison statement covers: ",
              paste(unresolved, collapse = ", "), call. = FALSE)
    }
    category_parts <- c(category_parts, append_rows(relevant[local_focal & relevant$statement_type == "suffix", , drop = FALSE]))
    category_parts <- category_parts[nzchar(category_parts)]
    if (length(category_parts)) {
      parts[[length(parts) + 1L]] <- category_parts
      part_subsections <- c(part_subsections, headings[[category]])
      assembled <- c(assembled, category_parts)
    }
  }

  # Diagnosis-wide suffixes.
  suffix_rows <- d[focal & d$statement_type == "suffix" &
                     d$specimen_category == "", , drop = FALSE]
  general_suffix <- append_rows(suffix_rows)
  assembled <- c(assembled, general_suffix)
  if (length(general_suffix)) {
    parts[[length(parts) + 1L]] <- general_suffix
    suffix_headings <- unique(suffix_rows$subsection[nzchar(suffix_rows$subsection)])
    if (length(suffix_headings) > 1L) {
      stop("Diagnosis-wide suffix rows have conflicting 'subsection' headings ",
           "for taxon '", taxon, "'.", call. = FALSE)
    }
    part_subsections <- c(part_subsections,
                          if (length(suffix_headings)) suffix_headings[1L] else "")
  }

  # BUILD taxRmatic_section OUTPUT -------------------------------------------
  # With separate_categories = TRUE, use optional CSV subsection headings for
  # categories, general prefixes, and general suffixes.
  # Keep the selected parts in their intended printing order.
  if (isTRUE(separate_categories)) {
    texts <- vapply(parts, function(x) {
      paste(x[nzchar(x)], collapse = " ")
    }, character(1L))
    subsections <- part_subsections
  } else {
    texts <- paste(assembled[nzchar(assembled)], collapse = " ")
    subsections <- ""
  }

  keep <- nzchar(texts)
  texts <- texts[keep]
  subsections <- subsections[keep]
  if (!length(texts)) {
    out <- empty_taxRmatic_section()
  } else {
    rows <- lapply(seq_along(texts), function(i) {
      new_taxRmatic_section_row(
        taxon = taxon,
        subsection = subsections[i],
        text = texts[i]
      )
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    class(out) <- c("taxRmatic_section", "data.frame")
  }
  attr(out, "subset") <- subset
  attr(out, "selected_rows") <- used
  attr(out, "coverage") <- if (length(coverage)) do.call(rbind, coverage) else data.frame()
  out
}

