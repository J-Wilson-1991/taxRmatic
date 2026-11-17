#' Format a text string
#'
#' Format a single character string by optionally changing its case and adding
#' prefix and/or suffix text.
#'
#' This function is used by taxRmatic functions that accept text-formatting
#' arguments, and can also be used directly to preview formatting styles. Styling
#' can be supplied either with the direct `case`, `prefix`, and `suffix`
#' arguments, or through a named `style` list containing those same arguments.
#'
#' @details
#' ## Case options
#'
#' The `case` argument accepts:
#'
#' \itemize{
#'   \item `"lowercase"`: convert the full string to lower case.
#'   \item `"uppercase"`: convert the full string to upper case.
#'   \item `"sentence"`: capitalise the first alphabetical character of the
#'     string and the first alphabetical character after sentence-ending
#'     punctuation.
#'   \item `"title"`: convert the string to title case.
#'   \item `"capitalise_first"`: capitalise only the first character of the
#'     string.
#' }
#'
#' The `"sentence"` option does not force all other characters to lower case.
#' This helps preserve taxon names, collection codes, gene names, abbreviations,
#' and other scientific text that may already contain meaningful capitalisation.
#'
#' ## Markdown-style formatting
#'
#' `format_text()` does not render Markdown. It simply returns a character string
#' that may contain Markdown-style characters. For example, adding `"*"` before
#' and after a string produces Markdown italics, while adding `"## "` before a
#' string produces a second-level Markdown heading.
#'
#' Common patterns include:
#'
#' \itemize{
#'   \item Italics: `prefix = "*"` and `suffix = "*"`.
#'   \item Bold: `prefix = "**"` and `suffix = "**"`.
#'   \item Bold italics: `prefix = "***"` and `suffix = "***"`.
#'   \item Line break: include `"\n"`.
#'   \item New paragraph: include `"\n\n"`.
#'   \item Headings: use `"# "`, `"## "`, or `"### "` as a prefix.
#' }
#'
#' Some Markdown renderers require two spaces before a newline to force a visible
#' line break within the same paragraph. In that case, use a suffix such as
#' `"  \n"`. For a paragraph break, use `"\n\n"`.
#'
#' ## Style lists
#'
#' The `style` argument is a named list containing any of `case`, `prefix`, and
#' `suffix`. If `style` is supplied, values in `style` override the corresponding
#' direct arguments.
#'
#' For example, `list(case = "title", prefix = "**", suffix = "**")` title-cases
#' the text and wraps it in Markdown bold markers.
#'
#' @section Formatting without editing every heading:
#' This helper is used by other taxRmatic functions to format heading labels and other text. A style is usually supplied as a list of options such as `list(prefix = "**", suffix = "**")`, where `**` makes text bold in Markdown.
#'
#' If you are learning the package, use the defaults first. Change the heading-format arguments of `compile_taxon_texts()` only when the basic manuscript is generating correctly.
#'
#' @param text Character(1). Text to format.
#' @param case Character(1) or NULL. Optional case transformation. Must be one of
#'   `"lowercase"`, `"uppercase"`, `"sentence"`, `"title"`, or
#'   `"capitalise_first"`.
#' @param prefix Character vector or NULL. Text to prepend to `text`. If a vector
#'   is supplied, its elements are collapsed with no separator before being
#'   prepended.
#' @param suffix Character vector or NULL. Text to append to `text`. If a vector
#'   is supplied, its elements are collapsed with no separator before being
#'   appended.
#' @param style Named list or NULL. Optional named list containing any of `case`,
#'   `prefix`, and `suffix`.
#'
#' @return Character(1). The formatted text string.
#'
#' @examples
#' format_text("material examined", case = "title")
#'
#' format_text("diagnosis", prefix = "**", suffix = "**")
#'
#' format_text("Exemplomygale", prefix = "*", suffix = "*")
#'
#' format_text("new species", prefix = "***", suffix = "***")
#'
#' format_text("remarks", prefix = "## ", suffix = "\n\n")
#'
#' format_text(
#'   "abdomen dark brown. legs yellow.",
#'   case = "sentence"
#' )
#'
#' heading_style <- list(
#'   case = "title",
#'   prefix = "## ",
#'   suffix = "\n\n"
#' )
#'
#' format_text("material examined", style = heading_style)
#'
#' paragraph_title_style <- list(
#'   case = "capitalise_first",
#'   prefix = "*",
#'   suffix = ".* "
#' )
#'
#' format_text("abdomen", style = paragraph_title_style)
#'
#' @export
format_text <- function(
    text,
    case = NULL,
    prefix = NULL,
    suffix = NULL,
    style = NULL
) {

  # APPLY STYLE ARGUMENT --------------------------------------------------------

  if (!is.null(style)) {
    if (!is.list(style)) {
      stop("`style` must be a named list.", call. = FALSE)
    }

    if (is.null(names(style)) || any(names(style) == "")) {
      stop("`style` must be a named list.", call. = FALSE)
    }

    valid_style_args <- c("case", "prefix", "suffix")
    invalid_style_args <- setdiff(names(style), valid_style_args)

    if (length(invalid_style_args) > 0) {
      stop(
        "`style` contains unsupported argument(s): ",
        paste(invalid_style_args, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    if ("case" %in% names(style)) case <- style$case
    if ("prefix" %in% names(style)) prefix <- style$prefix
    if ("suffix" %in% names(style)) suffix <- style$suffix
  }

  # VALIDATION -----------------------------------------------------------------

  if (!is.character(text) || length(text) != 1 || is.na(text)) {
    stop("`text` must be a single non-missing character string.", call. = FALSE)
  }

  if (!is.null(case)) {
    if (!is.character(case) ||
        length(case) != 1 ||
        is.na(case) ||
        trimws(case) == "") {
      stop(
        "`case` must be a single non-empty character string or NULL.",
        call. = FALSE
      )
    }

    case <- match.arg(
      case,
      c("lowercase", "uppercase", "sentence", "title", "capitalise_first")
    )
  }

  if (!is.null(prefix) && !is.character(prefix)) {
    stop("`prefix` must be a character vector or NULL.", call. = FALSE)
  }

  if (!is.null(suffix) && !is.character(suffix)) {
    stop("`suffix` must be a character vector or NULL.", call. = FALSE)
  }

  # INTERNAL HELPER -------------------------------------------------------------

  sentence_case <- function(x) {
    x <- sub(
      "^([[:space:]]*)([[:alpha:]])",
      "\\1\\U\\2",
      x,
      perl = TRUE
    )

    x <- gsub(
      "([.!?][[:space:]]+)([[:alpha:]])",
      "\\1\\U\\2",
      x,
      perl = TRUE
    )

    x
  }

  # APPLY CASE -----------------------------------------------------------------

  if (!is.null(case)) {
    text <- switch(
      case,
      lowercase = tolower(text),
      uppercase = toupper(text),
      sentence = sentence_case(text),
      title = tools::toTitleCase(tolower(text)),
      capitalise_first = paste0(
        toupper(substr(text, 1, 1)),
        substr(text, 2, nchar(text))
      )
    )
  }

  # PREFIX AND SUFFIX TEXT -----------------------------------------------------

  if (!is.null(prefix)) {
    text <- paste0(paste0(prefix, collapse = ""), text)
  }

  if (!is.null(suffix)) {
    text <- paste0(text, paste0(suffix, collapse = ""))
  }

  text
}
