#' Write taxRmatic text to markdown or Word
#'
#' Writes final taxRmatic text to a file.
#'
#' This function is usually used at the end of a taxRmatic workflow, after text
#' has been assembled with `compile_taxon_texts()` and finalised with
#' `finalise_taxRmatic_text()`.
#'
#' By default, the function writes a markdown (`.md`) file. It can also render a
#' Word document (`.docx`) from that markdown file using `rmarkdown::render()`.
#'
#' This function does not edit or finalise the text. It only writes the supplied
#' text to disk, and optionally creates a Word version.
#'
#' @details
#' ## Output files
#'
#' The function always writes a markdown file first. This markdown file is used
#' as the source file if a Word document is also rendered.
#'
#' File names are handled as follows:
#'
#' \itemize{
#'   \item if `file` has no extension, `.md` is added for the markdown file
#'   \item if `file` ends in `.md`, that path is used for the markdown file
#'   \item if `file` ends in another extension, such as `.docx`, that extension
#'   is replaced with `.md` for the markdown file
#' }
#'
#' If `output_format = "word_document"` and `render = TRUE`, a `.docx` file is
#' also created using the same base file name.
#'
#' ## Markdown and Word output
#'
#' Use `output_format = "md"` to write only a markdown file.
#'
#' Use `output_format = "word_document"` to write a markdown file and render a
#' Word document.
#'
#' Word rendering requires the optional `rmarkdown` package and Pandoc.
#' The Markdown-only mode does not require either dependency.
#'
#' @section Saving the manuscript:
#' After compiling and finalising the text, choose an output filename and format. `output_format = "md"` saves the manuscript as Markdown, a plain-text format that retains headings and italicisation markers. `output_format = "word_document"` creates a Word file that can be opened in a word processor; it requires `rmarkdown` and Pandoc.
#'
#' The `file` argument is where the manuscript is saved. To see your current folder, run `getwd()` in RStudio. For a file in a particular folder, give a complete path using `file.path(...)`. Regenerating the file overwrites work at the same output path: make edits in your CSVs and script, not only in the exported manuscript.
#'
#' @param text Character(1). Final text to write to file, usually the output of
#'   `finalise_taxRmatic_text()`.
#' @param file Character(1). File name or path. This may be supplied with or
#'   without a file extension.
#' @param output_format Character(1). Output type to create. Use `"md"` to write
#'   markdown only, or `"word_document"` to write markdown and render a Word
#'   document.
#' @param render Logical. If `TRUE` and `output_format = "word_document"`, render
#'   the markdown file to a Word document. Defaults to `TRUE`. If `FALSE`, only
#'   the markdown file is written.
#' @param quiet Logical. Passed to `rmarkdown::render()` when rendering a Word
#'   document. If `TRUE`, rendering messages are suppressed where possible.
#'   Defaults to `TRUE`.
#' @param ... Additional arguments passed to `rmarkdown::render()` when rendering
#'   a Word document.
#'
#' @return Invisibly returns a named list with the file paths created:
#'   \describe{
#'     \item{md}{Path to the markdown file.}
#'     \item{docx}{Path to the Word document, or `NULL` if no Word document was
#'     rendered.}
#'   }
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
#' # Write markdown only
#' write_taxRmatic_output(
#'   text = final_text,
#'   file = "example_revision",
#'   output_format = "md"
#' )
#'
#' # Write markdown and render a Word document
#' write_taxRmatic_output(
#'   text = final_text,
#'   file = "example_revision",
#'   output_format = "word_document"
#' )
#'
#' # Supply a file extension if preferred
#' write_taxRmatic_output(
#'   text = final_text,
#'   file = "example_revision.docx",
#'   output_format = "word_document"
#' )
#' }
#'
#' @export
write_taxRmatic_output <- function(
    text,
    file,
    output_format = c("md", "word_document"),
    render = TRUE,
    quiet = TRUE,
    ...
) {

  # MATCH ARGUMENTS ------------------------------------------------------------
  # Restrict the output format to the supported options so downstream export
  # logic can assume a valid choice has already been made.

  output_format <- match.arg(output_format)

  # INPUT VALIDATION -----------------------------------------------------------
  # Validate the high-level inputs before any files are written. This function
  # expects finalised text and a single file base name or path.

  if (!is.character(text) || length(text) != 1 || is.na(text)) {
    stop(
      "`text` must be a single character string.",
      call. = FALSE
    )
  }

  if (!is.character(file) || length(file) != 1 || is.na(file) ||
      !nzchar(trimws(file))) {
    stop(
      "`file` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  if (!is.logical(render) || length(render) != 1L || is.na(render)) {
    stop(
      "`render` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop(
      "`quiet` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  # DERIVE OUTPUT PATHS --------------------------------------------------------
  # Standardise the supplied path so we always know:
  # - where to write the markdown source file
  # - where the rendered Word file should go (if requested)

  file <- trimws(file)

  if (grepl("\\.[^.]+$", file)) {
    base_path <- sub("\\.[^.]+$", "", file)
  } else {
    base_path <- file
  }

  md_file <- paste0(base_path, ".md")
  docx_file <- paste0(base_path, ".docx")

  # WRITE MARKDOWN -------------------------------------------------------------
  # Always write the markdown file first. This is the source document used for
  # optional rendering to Word.

  writeLines(text, con = md_file, useBytes = TRUE)

  message("Markdown written to: ", md_file)

  # OPTIONALLY RENDER WORD DOCUMENT --------------------------------------------
  # Only render if requested and if the selected output format includes a Word
  # document. Rendering is done from the just-written markdown file.

  rendered_docx <- NULL

  if (output_format == "word_document" && render) {

    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      stop(
        "Package 'rmarkdown' must be installed to render a Word document.",
        call. = FALSE
      )
    }

    rmarkdown::render(
      input = md_file,
      output_format = "word_document",
      output_file = basename(docx_file),
      output_dir = dirname(docx_file),
      quiet = quiet,
      envir = new.env(parent = globalenv()),
      ...
    )

    rendered_docx <- docx_file
    message("Word document written to: ", rendered_docx)
  }

  # RETURN OUTPUT PATHS --------------------------------------------------------
  # Invisibly return the file paths so the caller can reuse them programmatically
  # if needed.

  invisible(list(
    md = md_file,
    docx = rendered_docx
  ))
}
