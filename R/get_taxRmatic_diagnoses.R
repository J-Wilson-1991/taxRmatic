#' Generate diagnoses for all taxa
#'
#' Generates a diagnosis for every taxon in the diagnostics table. You can
#' optionally choose a named group of taxa using `subset`. The resulting
#' `taxRmatic_section` object can be included in `compile_taxon_texts()`.
#'
#' @details
#' Each distinct focal taxon/`taxon_subset` pair is processed once, in
#' first-appearance order. The `subsection` values used for printed category
#' headings are passed through unchanged to the singular generator. The
#' reader must have validated and prepared the diagnostics first. This function
#' delegates diagnosis selection and wording to
#' `get_taxRmatic_diagnosis()` rather than duplicating its logic.
#'
#' @inheritParams get_taxRmatic_diagnosis
#' @section Generating all diagnoses:
#' Use this after `read_taxRmatic_diagnostics()` to generate diagnoses for all focal taxa named in the diagnostic data. It applies the same iterative comparison-selection rules as `get_taxRmatic_diagnosis()` to each taxon. Review the resulting prose and any warnings about taxa without a suitable comparison.
#'
#' The output is a `taxRmatic_section` object ready for `compile_taxon_texts()`. Set `separate_categories = TRUE` to keep male, female or other evidence categories in separate paragraphs; the category headings are taken from the diagnostics CSV `subsection` column. Use `FALSE` for one combined diagnosis paragraph.
#'
#' @param subset Optional character vector of subsets to generate. By default,
#'   all subsets are included. If the `taxon_subset` column was entirely blank,
#'   the diagnoses are generated from that single implicit comparison pool.
#'
#' @return A `taxRmatic_section` data frame with one or more rows per taxon,
#'   depending on `separate_categories`. An attribute named `details` contains
#'   the subset, selected input rows, and comparison coverage for each taxon.
#'
#' @examples
#' \dontrun{
#' diagnostics <- read_taxRmatic_diagnostics("diagnostics.csv")
#' diagnoses <- get_taxRmatic_diagnoses(diagnostics)
#' diagnoses$text
#' }
#'
#' @export
get_taxRmatic_diagnoses <- function(diagnostics, figures = NULL, subset = NULL,
                                     warn_unresolved = TRUE,
                                     figure_reference_style = "compact",
                                     warn_unmatched_categories = TRUE,
                                     separate_categories = FALSE) {
  # CHECK INPUT ---------------------------------------------------------------

  if (!inherits(diagnostics, "taxRmatic_diagnostics")) {
    stop("`diagnostics` must be a taxRmatic_diagnostics object, ",
         "as returned by read_taxRmatic_diagnostics().", call. = FALSE)
  }
  # IDENTIFY FOCAL TAXA ------------------------------------------------------

  d <- as.data.frame(diagnostics, stringsAsFactors = FALSE)
  if (!is.null(subset)) d <- d[!is.na(d$taxon_subset) & d$taxon_subset %in% subset, , drop = FALSE]
  pairs <- unique(do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
    x <- d$taxon[[i]]
    if (!length(x)) return(NULL)
    data.frame(taxon = x, subset = as.character(d$taxon_subset[i]), stringsAsFactors = FALSE)
  })))
  if (is.null(pairs) || !nrow(pairs)) return(empty_taxRmatic_section())
  if (!is.null(figures) && warn_unmatched_categories) {
    if (!"specimen_category" %in% names(figures)) {
      stop("`figures` lacks the `specimen_category` column.", call. = FALSE)
    }
    used_categories <- unique(as.character(d$specimen_category))
    used_categories <- used_categories[!is.na(used_categories) & nzchar(trimws(used_categories))]
    figure_categories <- normalise_filter_values(figures$specimen_category)
    unmatched <- setdiff(used_categories, figure_categories)
    if (length(unmatched)) {
      warning("These diagnostic categories do not match a figure specimen_category: ",
              paste(unmatched, collapse = ", "),
              ". No automatic figure references will be attached for unmatched ",
              "categories.", call. = FALSE)
    }
  }
  # GENERATE EACH DIAGNOSIS --------------------------------------------------

  details <- lapply(seq_len(nrow(pairs)), function(i)
    get_taxRmatic_diagnosis(diagnostics, pairs$taxon[i], figures,
                            subset = pairs$subset[i], warn_unresolved = warn_unresolved,
                            figure_reference_style = figure_reference_style,
                            warn_unmatched_categories = FALSE,
                            separate_categories = separate_categories))
  # COMBINE taxRmatic_section OBJECTS ----------------------------------------

  out <- do.call(rbind, lapply(details, function(x) {
    # rbind drops custom attributes, so preserve reports separately.
    class(x) <- c("taxRmatic_section", "data.frame")
    x
  }))
  rownames(out) <- NULL
  class(out) <- c("taxRmatic_section", "data.frame")
  attr(out, "details") <- lapply(seq_along(details), function(i) {
    list(taxon = pairs$taxon[i], subset = pairs$subset[i],
         selected_rows = attr(details[[i]], "selected_rows"),
         coverage = attr(details[[i]], "coverage"))
  })
  out
}

