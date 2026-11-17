#' Finalise compiled taxRmatic text for export
#'
#' Applies the final clean-up steps to compiled taxRmatic text.
#'
#' This function is usually used after `compile_taxon_texts()`. It takes the
#' compiled manuscript text and can replace figure tags, format taxon names, add
#' new-species suffixes, and clean up punctuation and spacing.
#'
#' This function does not write a file. It returns the final text as a character
#' string, which can then be copied, saved, or passed to another export function.
#'
#' @details
#' ## What this function can do
#'
#' Depending on the arguments supplied, this function can:
#'
#' \itemize{
#'   \item replace figure tags such as `fg{...}` with formatted figure
#'   references
#'   \item format taxon names in the text
#'   \item add a suffix such as `"sp. nov."` to selected new species
#'   \item apply general punctuation and spacing clean-up
#' }
#'
#' The steps are applied in this order:
#'
#' \enumerate{
#'   \item figure tags are replaced
#'   \item taxon names are formatted
#'   \item punctuation and spacing are cleaned up
#' }
#'
#' This order is intentional. Figure tags and taxon names are handled before the
#' final clean-up step, so that spacing and punctuation can be tidied at the end.
#'
#' ## Figure tags
#'
#' If `taxRmatic_figures` is supplied and `replace_figure_tags = TRUE`, inline
#' figure tags are replaced using `replace_figure_tags()`.
#'
#' Figure tags are placeholders in the text, such as:
#'
#' `fg{taxon = Exemplomygale aculeatus & sex = male}`
#'
#' These are replaced with formatted figure references using information from a
#' `taxRmatic_figures` object.
#'
#' The figure-reference prefix can be controlled with
#' `figure_prefix_singular` and `figure_prefix_plural`. For example, use
#' `"Fig."` and `"Figs."` for abbreviated references, or `"Figure"` and
#' `"Figures"` for full-word references.
#'
#' ## Taxon names and new species
#'
#' If `species_list` is supplied and `format_taxon_names_in_text = TRUE`, the
#' names in `species_list` are formatted in the text using
#' `format_taxon_names_in_text()`.
#'
#' If some species should also receive a new-species suffix, list them in
#' `new_species` and provide the suffix with `new_species_suffix`. For example,
#' `new_species_suffix = "sp. nov."`.
#'
#' ## Text clean-up
#'
#' By default, taxRmatic applies its standard clean-up rules using
#' `clean_taxRmatic_text()`. These rules are intended to remove common spacing
#' and punctuation problems that can appear after text has been assembled from
#' several sources.
#'
#' Extra clean-up rules can be supplied with `replacements`.
#'
#' @section The final text step:
#' Run this **after** `compile_taxon_texts()`, before exporting. It can replace `fg{...}` placeholders with figure numbers by consulting the figure metadata and apply consistent formatting to scientific names.
#'
#' If you enable name formatting, provide `species_list` containing the scientific names that should be recognised in the text. Only names supplied in `new_species` should receive `new_species_suffix` (for example `sp. nov.`). Do not mark already described species as new. If a figure tag remains unresolved, check the placeholder and the corresponding figure-table column/value rather than replacing it manually in the output.
#'
#' @param text Character(1). Compiled text to finalise, usually the output of
#'   `compile_taxon_texts()`.
#' @param taxRmatic_figures Optional `taxRmatic_figures` object, usually created
#'   with `read_taxRmatic_figures()`. Used when replacing figure tags.
#' @param replace_figure_tags Logical. If `TRUE`, replace inline `fg{...}` figure
#'   tags. Defaults to `TRUE`.
#' @param figure_reference_style Character(1). Figure-reference style to use
#'   when replacing figure tags. Usually `"compact"` or `"full"`.
#' @param figure_prefix_singular Character string. Prefix used when a figure
#'   reference resolves to a single figure, e.g. `"Fig."` or `"Figure"`.
#' @param figure_prefix_plural Character string. Prefix used when a figure
#'   reference resolves to multiple figures, e.g. `"Figs."` or `"Figures"`.
#' @param format_taxon_names_in_text Logical. If `TRUE`, format taxon names in
#'   the text. Defaults to `TRUE`.
#' @param species_list Optional character vector of taxon names to format in the
#'   text. If `NULL`, taxon-name formatting is skipped.
#' @param new_species Optional character vector of species names that should
#'   receive `new_species_suffix`.
#' @param new_species_suffix Optional character(1). Suffix added after names in
#'   `new_species`, such as `"sp. nov."`.
#' @param replacements Optional named character vector or named list of
#'   additional text replacements to pass to `clean_taxRmatic_text()`.
#' @param use_default_replacements Logical. If `TRUE`, the default taxRmatic
#'   clean-up rules are applied. Defaults to `TRUE`.
#'
#' @return Character(1). The finalised text.
#'
#' @examples
#' \dontrun{
#' compiled_text <- compile_taxon_texts(
#'   taxRmatic_dataset = list(
#'     Description = description_section,
#'     "Material examined" = material_examined_section,
#'     Remarks = remarks_section
#'   )
#' )
#'
#' final_text <- finalise_taxRmatic_text(
#'   text = compiled_text,
#'   taxRmatic_figures = figures,
#'   figure_reference_style = "compact",
#'   species_list = c("Exemplomygale aculeatus", "Exemplomygale illustris"),
#'   new_species = c("Exemplomygale aculeatus", "Exemplomygale illustris"),
#'   new_species_suffix = "sp. nov."
#' )
#'
#' final_text <- finalise_taxRmatic_text(
#'   text = compiled_text,
#'   taxRmatic_figures = figures,
#'   figure_reference_style = "compact",
#'   figure_prefix_singular = "Figure",
#'   figure_prefix_plural = "Figures",
#'   species_list = c("Exemplomygale aculeatus", "Exemplomygale illustris"),
#'   new_species = c("Exemplomygale aculeatus", "Exemplomygale illustris"),
#'   new_species_suffix = "sp. nov."
#' )
#'
#' # Finalise text without replacing figure tags
#' final_text <- finalise_taxRmatic_text(
#'   text = compiled_text,
#'   replace_figure_tags = FALSE,
#'   species_list = c("Exemplomygale aculeatus", "Exemplomygale illustris"),
#'   new_species = c("Exemplomygale aculeatus", "Exemplomygale illustris"),
#'   new_species_suffix = "sp. nov."
#' )
#' }
#'
#' @export
finalise_taxRmatic_text <- function(
    text,
    taxRmatic_figures = NULL,
    replace_figure_tags = TRUE,
    figure_reference_style = c("full", "compact"),
    figure_prefix_singular = "Fig.",
    figure_prefix_plural = "Figs.",
    format_taxon_names_in_text = TRUE,
    species_list = NULL,
    new_species = NULL,
    new_species_suffix = NULL,
    replacements = NULL,
    use_default_replacements = TRUE
) {

  # MATCH ARGUMENTS ------------------------------------------------------------
  # Restrict figure reference style to supported values before any optional
  # figure-tag replacement is attempted.

  figure_reference_style <- match.arg(figure_reference_style)

  # INPUT VALIDATION -----------------------------------------------------------
  # Validate high-level wrapper inputs here, while leaving detailed validation
  # of figure-tag parsing, taxon-name formatting, and replacement rules to the
  # lower-level helper functions.

  if (!is.character(text) || length(text) != 1) {
    stop(
      "`text` must be a single character string, typically the output of ",
      "`compile_taxon_texts()`.",
      call. = FALSE
    )
  }

  if (!is.logical(replace_figure_tags) || length(replace_figure_tags) != 1) {
    stop("`replace_figure_tags` must be TRUE or FALSE.", call. = FALSE)
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

  if (!is.logical(format_taxon_names_in_text) ||
      length(format_taxon_names_in_text) != 1) {
    stop("`format_taxon_names_in_text` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(use_default_replacements) ||
      length(use_default_replacements) != 1) {
    stop(
      "`use_default_replacements` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.null(taxRmatic_figures) &&
      !inherits(taxRmatic_figures, "taxRmatic_figures")) {
    stop(
      "`taxRmatic_figures` must be NULL or an object of class ",
      "'taxRmatic_figures', as returned by `read_taxRmatic_figures()`.",
      call. = FALSE
    )
  }

  # INITIALISE OUTPUT ----------------------------------------------------------
  # Work on a local copy so the processing steps can be applied sequentially.

  out <- text

  # REPLACE FIGURE TAGS --------------------------------------------------------
  # Replace inline fg{...} tags with formatted figure references if requested
  # and if a figures table has been supplied.

  if (replace_figure_tags && !is.null(taxRmatic_figures)) {
    out <- replace_figure_tags(
      text = out,
      taxRmatic_figures = taxRmatic_figures,
      figure_reference_style = figure_reference_style,
      figure_prefix_singular = figure_prefix_singular,
      figure_prefix_plural = figure_prefix_plural
    )
  }

  # FORMAT TAXON NAMES ---------------------------------------------------------
  # Apply markdown italicisation, genus abbreviation logic, and optional
  # new-species suffixes if requested and if a species list has been supplied.

  if (format_taxon_names_in_text && !is.null(species_list)) {
    out <- format_taxon_names_in_text(
      text = out,
      species_list = species_list,
      new_species = new_species,
      new_species_suffix = new_species_suffix
    )
  }

  # CLEAN TEXT -----------------------------------------------------------------
  # Apply final punctuation and spacing clean-up rules after all other text
  # transformations have been completed.

  if (use_default_replacements || !is.null(replacements)) {
    out <- clean_taxRmatic_text(
      text = out,
      replacements = replacements,
      use_default_replacements = use_default_replacements
    )
  }

  out
}
