#' Compile taxRmatic objects into taxon treatments
#'
#' Combines descriptions, diagnoses, material examined, figures and other sections
#' into manuscript text for each taxon. Sections appear in the order you supply.
#'
#' @details
#' \strong{Sections and headings}
#'
#' Supply the objects you want to include as a list. Each object must be of
#' class \code{taxRmatic_section} or \code{taxRmatic_figures}; you normally
#' obtain these by using taxRmatic's \code{read_} and \code{get_} functions.
#'
#' Give each section a unique name, for example:
#' \code{list(Diagnosis = diagnoses, Description = descriptions,
#' Remarks = remarks)}
#'
#' These names become section headings. Change the list order to change
#' the order of sections. Figure objects can be unnamed because they do not
#' produce a section heading. Figure objects are included to insert a list of
#' relevant figures for a taxon (normally after the taxon title). 
#' Use \code{omit_section_headings} to hide some or all headings.
#'
#' \strong{Paragraphs and subsection headings}
#'
#' Each row in a \code{taxRmatic_section} becomes a separate paragraph, printed
#' in the order it appears within that taxon and section. Rearrange the rows if
#' you want to change paragraph order.
#'
#' Consecutive rows with the same \code{subsection} value are given a title, rows
#' without a subsection are printed as a paragraph without a title.
#'
#' Use \code{omit_subsection_headings} to hide some or all subsection headings 
#' without changing paragraph order or boundaries.
#'
#' \strong{Figures and formatting}
#'
#' Include a \code{taxRmatic_figures} object to print a list of the figures
#' relevant to each taxon. This list is never given a heading.
#' 
#' The formatting arguments control the appearance
#' of taxon names, headings and figure references using markdown symbols.
#'
#' @section A complete treatment:
#' This is where you decide the **order of sections**. Supply a named list such as `list(Diagnosis = diagnoses, Description = descriptions, Remarks = sections$remarks)`; each object must have been produced by a taxRmatic `read_` or `get_` function and have class `taxRmatic_section` (or `taxRmatic_figures` for figures). The list names become headings, and changing the list order changes the treatment layout.
#'
#' Each row of a section produces one paragraph, in its existing order. If consecutive rows share a nonblank `subsection`, the heading prints once above those paragraphs; a blank subsection prints no heading. `omit_section_headings` and `omit_subsection_headings` hide labels **without removing text**.
#'
#' Use `taxa` to choose which taxa receive treatments and `taxon_order` to choose their printed order. Include the figures object in the list if you want a figure list beneath the taxon heading. Pass the compiled text to `finalise_taxRmatic_text()` before writing a manuscript file.
#'
#' @param taxRmatic_dataset A list of \code{taxRmatic_section} and/or
#'   \code{taxRmatic_figures} objects in the order they should appear within
#'   each taxon. Section objects need unique list names; figure objects may be unnamed.
#' @param taxa Optional vector of taxon names to include. By default, taxRmatic
#'   identifies taxa from the supplied objects, but this may include comparative
#'   taxa for which you don't actually want a treatment produced.
#' @param taxon_order \code{NULL} (default) or \code{"alphabetical"} prints taxa
#'   alphabetically. Alternatively, supply taxon names in your preferred order;
#'   any taxa in the sections supplied but not in your order list 
#'   appear afterwards in alphabetical order.
#' @param taxon_authorities Optional named vector containing authorities to add
#'   to taxon headings, with names matching the taxon names exactly.
#' @param format_taxon_name Formatting settings for taxon headings, supplied
#'   as a list accepted by \code{format_text()}. By default, headings are bold.
#' @param format_section_name Formatting settings for section headings,
#'   supplied as a list accepted by \code{format_text()}.
#' @param format_subsection_name Formatting settings for subsection headings,
#'   supplied as a list accepted by \code{format_text()}.
#' @param omit_section_headings Set to \code{TRUE} to hide every section heading,
#'   or give the names of the sections whose headings should be hidden.
#'   By default, headings are shown.
#' @param omit_subsection_headings Set to \code{TRUE} to hide every subsection
#'   heading, or give the names of sections in which to hide them.
#'   By default, subsection headings are shown.
#' @param figure_reference_style \code{"compact"} (default) or \code{"full"}.
#'   Controls figure references immediately below the taxon heading.
#' @param figure_prefix_singular Wording for a single figure reference, such as
#'   \code{"Fig."} (default) or \code{"Figure"}.
#' @param figure_prefix_plural Wording for multiple figure references, such as
#'   \code{"Figs."} (default) or \code{"Figures"}.
#' @param figure_parentheses Set to \code{TRUE} to put figure references below
#'   taxon headings in parentheses; the default is \code{FALSE}.
#'
#' @return A single character string containing the compiled manuscript text.
#'
#' @examples
#' # Normally, sections are produced using taxRmatic read/get functions.
#' # This small example shows how subsection headings work.
#' diagnosis <- data.frame(
#'   taxon = rep("Example species", 5),
#'   subsection = c("", "Male", "Male", "Female", ""),
#'   text = c(
#'     "Introductory diagnosis.",
#'     "First male character.", "Second male character.",
#'     "Female character.", "Concluding statement."
#'   )
#' )
#' class(diagnosis) <- c("taxRmatic_section", "data.frame")
#'
#' compile_taxon_texts(list(Diagnosis = diagnosis))
#'
#' # Keep the paragraphs but omit the Male/Female headings.
#' compile_taxon_texts(
#'   list(Diagnosis = diagnosis),
#'   omit_subsection_headings = "Diagnosis"
#' )
#'
#' @export
compile_taxon_texts <- function(
    taxRmatic_dataset,
    taxa = NULL,
    taxon_order = NULL,
    taxon_authorities = NULL,
    format_taxon_name = list(
      prefix = "# **", case = "capitalise_first", suffix = "**\n\n"
    ),
    format_section_name = list(
      prefix = "## **", case = "capitalise_first", suffix = "**\n\n"
    ),
    format_subsection_name = list(
      prefix = "### ", case = "capitalise_first", suffix = ".\n\n"
    ),
    omit_section_headings = NULL,
    omit_subsection_headings = NULL,
    figure_reference_style = c("compact", "full"),
    figure_prefix_singular = "Fig.",
    figure_prefix_plural = "Figs.",
    figure_parentheses = FALSE
) {

  # VALIDATE DATASET -----------------------------------------------------------
  figure_reference_style <- match.arg(figure_reference_style)

  if (!is.list(taxRmatic_dataset) || is.data.frame(taxRmatic_dataset)) {
    stop(
      "`taxRmatic_dataset` must be a list of taxRmatic objects. ",
      "Supply even a single section as a named list, e.g. ",
      "`list(Remarks = remarks_section)`.",
      call. = FALSE
    )
  }

  is_section <- vapply(
    taxRmatic_dataset, inherits, logical(1), what = "taxRmatic_section"
  )
  is_figures <- vapply(
    taxRmatic_dataset, inherits, logical(1), what = "taxRmatic_figures"
  )

  # Identify the actual list element, not just its class.
  # A frequent cause is a section that is missing from the imported CSV:
  # `sections$some_name` then evaluates to NULL in the compilation list.
  dataset_names <- names(taxRmatic_dataset)
  if (is.null(dataset_names)) {
    dataset_names <- rep("", length(taxRmatic_dataset))
  }

  invalid <- which(!(is_section | is_figures))
  if (length(invalid)) {
    labels <- vapply(invalid, function(i) {
      name <- dataset_names[i]
      if (is.na(name) || !nzchar(trimws(name))) {
        paste0("unnamed item at position ", i)
      } else {
        paste0("'", name, "' (position ", i, ")")
      }
    }, character(1))
    stop(
      "Invalid item(s) in `taxRmatic_dataset`: ",
      paste(labels, collapse = ", "), ". ",
      "Every item must inherit from 'taxRmatic_section' or ",
      "'taxRmatic_figures'. If a section was not present in your ",
      "additional-sections CSV, `sections$section_name` may be NULL. ",
      "Remove that list entry or supply the missing section.",
      call. = FALSE
    )
  }

  if (any(is_section & (is.na(dataset_names) | !nzchar(trimws(dataset_names))))) {
    stop(
      "Every `taxRmatic_section` must have a non-empty list name ",
      "(e.g. `list(Remarks = remarks_section)`). ",
      "This name is used as the section heading unless ",
      "the `omit_section_headings` argument is used.",
      call. = FALSE
    )
  }

  dataset_names <- trimws(dataset_names)
  section_names <- dataset_names[is_section]
  section_keys <- tolower(section_names)

  if (anyDuplicated(section_keys)) {
    stop(
      "Section names in `taxRmatic_dataset` must be unique ",
      "(ignoring case). Repeated sections should be combined into one ",
      "`taxRmatic_section` object.",
      call. = FALSE
    )
  }

  # Check required columns early, so malformed section objects produce a useful
  # message instead of failing later during compilation.
  for (i in which(is_section)) {
    section <- taxRmatic_dataset[[i]]
    required <- c("taxon", "subsection", "text")
    missing <- setdiff(required, names(section))

    if (!is.data.frame(section) || length(missing) > 0) {
      stop(
        "Section '", dataset_names[i], "' must be a data frame with columns: ",
        paste(required, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  # VALIDATE OPTIONS -----------------------------------------------------------
  for (arg in c("format_taxon_name", "format_section_name",
                "format_subsection_name")) {
    if (!is.list(get(arg))) {
      stop("`", arg, "` must be a style list.", call. = FALSE)
    }
  }

  for (arg in c("figure_prefix_singular", "figure_prefix_plural")) {
    value <- get(arg)
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      stop("`", arg, "` must be a single character string.", call. = FALSE)
    }
  }

  if (!is.logical(figure_parentheses) ||
      length(figure_parentheses) != 1L || is.na(figure_parentheses)) {
    stop("`figure_parentheses` must be TRUE or FALSE.", call. = FALSE)
  }

  # A section's list name is its internal identity, independently of whether
  # the heading is visible. Do not silently ignore misspelled omission names.
  normalise_omission <- function(x, arg) {
    if (is.null(x)) return(list(all = FALSE, names = character(0)))
    if (is.logical(x) && length(x) == 1L && !is.na(x)) {
      return(list(all = isTRUE(x), names = character(0)))
    }
    if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x)))) {
      stop(
        "`", arg, "` must be NULL, TRUE, FALSE, or a character vector ",
        "of non-empty section names.",
        call. = FALSE
      )
    }

    keys <- unique(tolower(trimws(x)))
    unknown <- setdiff(keys, section_keys)
    if (length(unknown)) {
      stop(
        "Unknown section name(s) in `", arg, "`: ",
        paste(unknown, collapse = ", "), ". Available sections: ",
        if (length(section_names)) paste(section_names, collapse = ", ") else "none",
        ".",
        call. = FALSE
      )
    }
    list(all = FALSE, names = keys)
  }

  omit_sections <- normalise_omission(
    omit_section_headings, "omit_section_headings"
  )
  omit_subsections <- normalise_omission(
    omit_subsection_headings, "omit_subsection_headings"
  )

  if (!is.null(taxon_authorities)) {
    authority_names <- names(taxon_authorities)
    if (!is.character(taxon_authorities) || is.null(authority_names) ||
        anyNA(authority_names) || any(!nzchar(trimws(authority_names))) ||
        anyDuplicated(authority_names) || anyNA(taxon_authorities)) {
      stop(
        "`taxon_authorities` must be a named character vector with unique, ",
        "non-empty names and no missing values.",
        call. = FALSE
      )
    }
  }

  # DETERMINE TAXA -------------------------------------------------------------
  if (!is.null(taxa)) {
    if (!is.character(taxa)) {
      stop("`taxa` must be a character vector.", call. = FALSE)
    }
    all_taxa <- unique(trimws(taxa))
    all_taxa <- all_taxa[!is.na(all_taxa) & nzchar(all_taxa)]
    if (!length(all_taxa)) {
      stop("`taxa` must contain at least one taxon name.", call. = FALSE)
    }
  } else {
    # Only single-string taxon values are candidates for treatment headings;
    # figure list-columns can contain multiple taxa within a single cell.
    all_taxa <- unique(unlist(lapply(taxRmatic_dataset, function(obj) {
      if (!"taxon" %in% names(obj)) return(character(0))
      values <- obj$taxon
      valid <- vapply(values, function(value) {
        is.character(value) && length(value) == 1L &&
          !is.na(value) && nzchar(trimws(value))
      }, logical(1))
      trimws(unlist(values[valid], use.names = FALSE))
    }), use.names = FALSE))
  }

  if (is.null(taxon_order) || identical(taxon_order, "alphabetical")) {
    ordered_taxa <- sort(all_taxa)
  } else {
    if (!is.character(taxon_order) || anyNA(taxon_order) ||
        any(!nzchar(trimws(taxon_order)))) {
      stop(
        "`taxon_order` must be NULL, 'alphabetical', or a character vector ",
        "of non-empty taxon names.",
        call. = FALSE
      )
    }
    requested_order <- unique(trimws(taxon_order))
    requested_order <- requested_order[requested_order %in% all_taxa]
    ordered_taxa <- c(requested_order, sort(setdiff(all_taxa, requested_order)))
  }

  # COMPILE TAXA ---------------------------------------------------------------
  result <- vector("list", length(ordered_taxa))

  for (taxon_index in seq_along(ordered_taxa)) {
    taxon <- ordered_taxa[taxon_index]
    heading <- taxon
    if (!is.null(taxon_authorities) && taxon %in% names(taxon_authorities)) {
      heading <- paste(taxon, unname(taxon_authorities[taxon]))
    }
    pieces <- format_text(heading, style = format_taxon_name)

    for (i in seq_along(taxRmatic_dataset)) {
      obj <- taxRmatic_dataset[[i]]

      # Figure objects are a special case: no section heading.
      if (is_figures[i]) {
        fig_text <- format_figure_reference_text(
          taxRmatic_figures = obj,
          filter = list(taxon = taxon),
          figure_reference_style = figure_reference_style,
          figure_prefix_singular = figure_prefix_singular,
          figure_prefix_plural = figure_prefix_plural,
          parentheses = figure_parentheses
        )
        if (length(fig_text) == 1L && !is.na(fig_text) && nzchar(fig_text)) {
          pieces <- c(pieces, fig_text)
        }
        next
      }

      # Avoid NA taxon comparisons and exclude empty text before adding headings.
      matching <- which(!is.na(obj$taxon) & obj$taxon == taxon)
      if (!length(matching)) next
      rows <- obj[matching, , drop = FALSE]
      has_text <- !is.na(rows$text) & nzchar(trimws(as.character(rows$text)))
      rows <- rows[has_text, , drop = FALSE]
      if (!nrow(rows)) next

      section_name <- dataset_names[i]
      section_key <- tolower(section_name)
      hide_section <- omit_sections$all || section_key %in% omit_sections$names
      hide_subsection <- omit_subsections$all ||
        section_key %in% omit_subsections$names

      if (!hide_section) {
        pieces <- c(pieces, format_text(section_name, style = format_section_name))
      }

      # Rows retain their input order. Every row is a separate paragraph,
      # including consecutive rows that share a subsection heading.

      subsection_values <- trimws(as.character(rows$subsection))
      subsection_values[is.na(subsection_values)] <- ""

      # Heading identity is independent of paragraph identity: print a named
      # subsection heading only at the start of a consecutive run, and never
      # merge the rows beneath it. Blank rows are unheaded and end the run.
      previous_subsection <- ""
      for (j in seq_len(nrow(rows))) {
        subsection <- subsection_values[j]
        if (nzchar(subsection) &&
            !identical(subsection, previous_subsection) &&
            !hide_subsection) {
          pieces <- c(pieces, format_text(
            subsection, style = format_subsection_name
          ))
        }
        pieces <- c(pieces, as.character(rows$text[j]))
        previous_subsection <- subsection
      }
    }
    result[[taxon_index]] <- pieces
  }

  # Keep all manuscript items separated by a paragraph break, as in v4.
  paste(unlist(result, use.names = FALSE), collapse = "\n\n")
}
