#' Clean taxRmatic text using ordered regex replacements
#'
#' Internal helper used to standardise text by applying a series of regular
#' expression replacements in order. This function is intended to remove common
#' mechanical formatting issues introduced during text assembly, such as
#' duplicated punctuation, misplaced spacing, and leading or trailing
#' whitespace.
#'
#' A default set of conservative replacements is provided and is designed to be
#' compatible with Markdown-based formatting used elsewhere in \pkg{taxRmatic}.
#' Additional user-supplied replacements can also be applied after the defaults.
#'
#' @param text A character vector to clean.
#' @param replacements An optional named character vector or named list of
#'   additional replacements. Names must be regular expression patterns and
#'   values must be replacement strings. Replacements are applied in order.
#' @param use_default_replacements Logical. If \code{TRUE}, apply the built-in
#'   default replacement rules before any user-supplied replacements.
#'
#' @return A character vector of the same length as \code{text}, with all
#'   replacements applied.
#'
#' @details
#' Default replacements are applied first, followed by any user-supplied
#' replacements. Because replacements are applied sequentially, later rules may
#' affect the output of earlier ones.
#'
#' The default replacement set is intentionally conservative and focuses on
#' common human-caused formatting errors without altering intended Markdown
#' syntax.
#'
#' @keywords internal
clean_taxRmatic_text <- function(
    text,
    replacements = NULL,
    use_default_replacements = TRUE
) {

  # CHECK INPUT ----------------------------------------------------------------
  if (!is.character(text)) {
    stop("'text' must be a character vector.", call. = FALSE)
  }

  # DEFAULT REPLACEMENT RULES --------------------------------------------------
  # Names are regex patterns and values are replacement strings.
  # Rules are applied in order and are designed to conservatively clean common
  # punctuation and spacing errors without interfering with Markdown rendering.
  default_replacements <- c(

    # Normalise unusual Unicode whitespace
    "\u00A0" = " ",
    "\u202F" = " ",
    "\u2009" = " ",
    "\u200B" = "",

    # Remove invalid punctuation combinations
    ";[ \t]*\\." = ".",
    ",[ \t]*\\." = ".",
    "\\.[ \t]*\\." = ".",
    ";[ \t]*;" = "; ",
    ",[ \t]*," = ", ",

    # Trim leading and trailing spaces/tabs only
    "^[ \t]+" = "",
    "[ \t]+$" = "",

    # Trim spaces/tabs at start/end of each line
    "(?m)^[ \t]+" = "",
    "(?m)[ \t]+$" = "",

    # Standardise spacing within lines only
    "[ \t]{2,}" = " ",
    "[ \t]+([,.;:])" = "\\1",
    "([,;:])([ \t]*)([[:alnum:]])" = "\\1 \\3",
    "\\.([ \t]*)([[:alpha:]])" = ". \\2",

    # Standardise safe spacing around brackets
    ",[ \t]*\\(" = " (",
    "\\)([A-Za-z])" = ") \\1",

    # Ensure a space after colons in plain text contexts
    ":(?=[A-Za-z])" = ": "
  )

  # VALIDATE USER REPLACEMENTS -------------------------------------------------
  if (!is.null(replacements)) {

    if (is.list(replacements)) {
      replacements <- unlist(replacements, use.names = TRUE)
    }

    if (!is.character(replacements) || is.null(names(replacements))) {
      stop(
        "'replacements' must be a named character vector or named list, ",
        "with names = patterns and values = replacements.",
        call. = FALSE
      )
    }

    if (any(!nzchar(names(replacements)))) {
      stop(
        "All entries in 'replacements' must have non-empty names.",
        call. = FALSE
      )
    }
  }

  # COMBINE REPLACEMENT SETS ---------------------------------------------------
  # User-supplied replacements are appended after the defaults so that they can
  # further refine the cleaned text.
  final_replacements <- character()

  if (use_default_replacements) {
    final_replacements <- default_replacements
  }

  if (!is.null(replacements)) {
    final_replacements <- c(final_replacements, replacements)
  }

  # APPLY REPLACEMENTS ---------------------------------------------------------
  out <- text

  for (i in seq_along(final_replacements)) {
    pattern <- names(final_replacements)[i]
    replacement <- final_replacements[i]
    out <- gsub(pattern, replacement, out, perl = TRUE)
  }

  out
}
