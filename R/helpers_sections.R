#' Create an empty taxRmatic section data frame
#'
#' Internal helper to initialise an empty `taxRmatic_section`
#' object with the standard columns.
#'
#' @return An empty data frame of class `taxRmatic_section`.
#' @keywords internal
empty_taxRmatic_section <- function() {

  df <- data.frame(
    taxon = character(0),
    subsection = character(0),
    text = character(0),
    stringsAsFactors = FALSE
  )

  class(df) <- c("taxRmatic_section", "data.frame")
  df
}


#' Create a single-row taxRmatic section entry
#'
#' Internal helper to construct a one-row `taxRmatic_section`
#' data frame with standardised column types.
#'
#' @param taxon Character. Taxon name.
#' @param subsection Character. Subsection name.
#' @param text Character. Text content.
#'
#' @return A single-row data frame of class `taxRmatic_section`.
#' @keywords internal
new_taxRmatic_section_row <- function(
    taxon,
    subsection,
    text
) {

  df <- data.frame(
    taxon = as.character(taxon),
    subsection = as.character(subsection),
    text = as.character(text),
    stringsAsFactors = FALSE
  )

  class(df) <- c("taxRmatic_section", "data.frame")
  df
}
