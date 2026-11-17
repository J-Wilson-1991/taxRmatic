#' Format and italicise taxon names within text
#'
#' Formats recognised taxon names in a character string. The function can:
#'
#' \itemize{
#'   \item optionally append a suffix to nominated new species names;
#'   \item italicise recognised binomials exactly as listed in `species_list`;
#'   \item italicise recognised genus names when they appear alone;
#'   \item abbreviate repeated recognised binomials within each paragraph.
#' }
#'
#' This is a lower-level text-formatting utility. It works on a character string,
#' not on a data frame. It does not infer taxon names automatically; names are
#' recognised only from `species_list`.
#'
#' Taxon names are recognised from `species_list`, which may contain either
#' genus-only entries, such as `"Aname"`, or binomials/multiword species names,
#' such as `"Aname barrema"` or `"Genus species subspecies"`.
#'
#' Genera represented by multiple names are abbreviated to one letter unless this
#' would cause a collision, in which case two letters are used.
#'
#' Paragraphs are treated as blocks separated by double line breaks. Within each
#' paragraph, the first recognised binomial for a genus retains its full genus
#' name, and recognised binomials at the start of a sentence are also not
#' abbreviated.
#'
#' @param text Character string containing the text to format.
#' @param species_list Character vector of recognised taxon names. Entries with
#'   one token are treated as genus names; entries with two or more tokens are
#'   treated as species-level names.
#' @param new_species Optional character vector of species names to which a
#'   suffix should be appended before formatting.
#' @param new_species_suffix Optional character string giving the suffix to
#'   append to each entry in `new_species`.
#'
#' @return Character string with taxon names formatted in Markdown italics and
#'   repeated recognised binomials abbreviated where appropriate.
#'
#' @examples
#' text <- paste(
#'   paste("Aname barrema is similar to Aname mellosa,",
#'         "but differs from all other Aname by the presence of large anterior eyes."),
#'   "Aname barrema differs from Aname mellosa in the shape of the spermathecae."
#' )
#'
#' format_taxon_names_in_text(
#'   text = text,
#'   species_list = c("Aname barrema", "Aname mellosa")
#' )
#'
#' format_taxon_names_in_text(
#'   text = "Exemplomygale valentinae occurs near Leinster.",
#'   species_list = c("Exemplomygale", "Exemplomygale valentinae"),
#'   new_species = "Exemplomygale valentinae",
#'   new_species_suffix = "sp. nov."
#' )
#'
#' @export
format_taxon_names_in_text <- function(text,
                                       species_list,
                                       new_species = NULL,
                                       new_species_suffix = NULL) {

  # INTERNAL REGEX HELPER ------------------------------------------------------

  escape_regex <- function(x) {
    gsub("([.\\^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", x, perl = TRUE)
  }

  # VALIDATION -----------------------------------------------------------------

  if (!is.character(text) ||
      length(text) != 1 ||
      is.na(text)) {
    stop(
      "`text` must be a single character string.",
      call. = FALSE
    )
  }

  if (!is.character(species_list)) {
    stop(
      "`species_list` must be a character vector of recognised taxon names.",
      call. = FALSE
    )
  }

  if (xor(is.null(new_species), is.null(new_species_suffix))) {
    stop(
      "Both `new_species` and `new_species_suffix` must be provided, ",
      "or both must be NULL.",
      call. = FALSE
    )
  }

  if (!is.null(new_species) && !is.character(new_species)) {
    stop(
      "`new_species` must be NULL or a character vector.",
      call. = FALSE
    )
  }

  if (!is.null(new_species_suffix) &&
      (!is.character(new_species_suffix) ||
       length(new_species_suffix) != 1 ||
       is.na(new_species_suffix) ||
       !nzchar(trimws(new_species_suffix)))) {
    stop(
      "`new_species_suffix` must be NULL or a single non-empty character string.",
      call. = FALSE
    )
  }

  # APPEND NEW-SPECIES SUFFIX --------------------------------------------------
  # If requested, append the supplied suffix to each nominated new species name
  # before any further formatting is applied.

  if (!is.null(new_species) && !is.null(new_species_suffix)) {
    new_species <- unique(trimws(new_species))
    new_species <- new_species[!is.na(new_species) & nzchar(new_species)]

    new_species_suffix <- trimws(new_species_suffix)

    for (sp in new_species) {
      text <- gsub(
        paste0("\\b", escape_regex(sp), "\\b"),
        paste0(sp, " ", new_species_suffix),
        text,
        perl = TRUE
      )
    }
  }

  # CLEAN AND CLASSIFY TAXON NAMES ---------------------------------------------
  # Remove duplicates, trim whitespace, and discard missing/empty entries.

  species_list <- unique(trimws(species_list))
  species_list <- species_list[!is.na(species_list) & nzchar(species_list)]

  if (!length(species_list)) {
    return(text)
  }

  # Split each entry into tokens.
  # One token = genus only.
  # Two or more tokens = species-level name or lower-rank multiword name.

  tokens <- strsplit(species_list, "\\s+")
  is_genus_only <- vapply(tokens, length, integer(1)) == 1

  genus_only <- unique(vapply(tokens[is_genus_only], `[`, character(1), 1))

  binomials <- species_list[!is_genus_only]
  binomials <- binomials[order(nchar(binomials), decreasing = TRUE)]

  # Extract genus and epithet portion from species-level names.

  if (length(binomials)) {
    bin_tokens <- strsplit(binomials, "\\s+")
    bin_genera <- vapply(bin_tokens, `[`, character(1), 1)
    bin_epithets <- vapply(
      bin_tokens,
      function(x) paste(x[-1], collapse = " "),
      character(1)
    )
  } else {
    bin_genera <- character(0)
    bin_epithets <- character(0)
  }

  # Define the full set of recognised genera from both genus-only and
  # species-level entries.

  genera <- unique(c(genus_only, bin_genera))

  if (!length(genera)) {
    return(text)
  }

  # Build a mapping from each genus to its recognised epithet strings.

  genus_to_epithets <- stats::setNames(
    vector("list", length(genera)),
    genera
  )

  for (g in genera) {
    genus_to_epithets[[g]] <- unique(bin_epithets[bin_genera == g])
  }

  # BUILD GENUS ABBREVIATIONS --------------------------------------------------
  # By default, genera are abbreviated to one letter plus a full stop. If
  # multiple genera share the same first letter, use two letters instead.

  first_letter <- substring(genera, 1, 1)

  colliding_letters <- unique(
    first_letter[
      duplicated(first_letter) | duplicated(first_letter, fromLast = TRUE)
    ]
  )

  genus_abbrev <- stats::setNames(vector("character", length(genera)), genera)

  for (g in genera) {
    if (substring(g, 1, 1) %in% colliding_letters) {
      ab <- substring(g, 1, min(2, nchar(g)))
    } else {
      ab <- substring(g, 1, 1)
    }

    genus_abbrev[[g]] <- paste0(ab, ".")
  }

  # SPLIT TEXT INTO PARAGRAPHS -------------------------------------------------
  # Paragraphs are separated by double line breaks and handled independently so
  # that abbreviation rules reset for each paragraph.

  paragraphs <- strsplit(text, "\n\n", fixed = TRUE)[[1]]

  paragraphs <- lapply(paragraphs, function(p) {

    # Split paragraph into sentence-like units so that abbreviations can avoid
    # sentence-initial taxon names.

    sentences <- unlist(strsplit(p, "(?<=[.!?])\\s+", perl = TRUE))

    if (!length(sentences)) {
      return(p)
    }

    # ITALICISE RECOGNISED BINOMIALS -------------------------------------------
    # Apply exact italicisation to all recognised species-level names first.
    # Longer names are handled first to avoid partially italicising multiword
    # species names.

    if (length(binomials)) {
      for (i in seq_along(sentences)) {
        s <- sentences[i]

        for (term in binomials) {
          s <- gsub(
            paste0("\\b", escape_regex(term), "\\b"),
            paste0("*", term, "*"),
            s,
            perl = TRUE
          )
        }

        sentences[i] <- s
      }
    }

    # ITALICISE GENERA APPEARING ALONE -----------------------------------------
    # Italicise recognised genus names when they are not immediately followed by
    # a recognised epithet for that genus.

    for (i in seq_along(sentences)) {
      s <- sentences[i]

      for (g in genera) {
        eps <- genus_to_epithets[[g]]
        escaped_g <- escape_regex(g)

        if (length(eps) > 0) {
          escaped_eps <- vapply(eps, escape_regex, character(1))
          avoid_regex <- paste0("(?:", paste(escaped_eps, collapse = "|"), ")")

          # Italicise the genus only when it is not starting a recognised
          # species-level name.
          pattern <- paste0("\\b", escaped_g, "\\b(?!\\s+", avoid_regex, "\\b)")
        } else {
          # If the genus has no recognised epithets, italicise every occurrence
          # of the genus token.
          pattern <- paste0("\\b", escaped_g, "\\b")
        }

        s <- gsub(pattern, paste0("*", g, "*"), s, perl = TRUE)
      }

      sentences[i] <- s
    }

    # ABBREVIATE REPEATED GENERA IN RECOGNISED BINOMIALS -----------------------
    # Rules:
    # - only abbreviate recognised binomials;
    # - never abbreviate the first recognised binomial for a genus in a paragraph;
    # - never abbreviate a recognised binomial at the start of a sentence.

    for (g in genera) {
      eps <- genus_to_epithets[[g]]

      # Genera with no recognised epithets cannot form recognised binomials.

      if (!length(eps)) {
        next
      }

      repl <- genus_abbrev[[g]]
      escaped_g <- escape_regex(g)
      escaped_eps <- vapply(eps, escape_regex, character(1))

      first_binomial_for_genus_remaining <- TRUE

      for (i in seq_along(sentences)) {
        s <- sentences[i]

        matches <- gregexpr(
          paste0("\\b", escaped_g, "\\b"),
          s,
          perl = TRUE
        )[[1]]

        if (matches[1] == -1) {
          sentences[i] <- s
          next
        }

        # Work backwards through matches so character positions remain stable
        # after replacement.

        for (m in rev(matches)) {
          after_genus <- substring(s, m + nchar(g) + 1)

          in_recognised_binomial <- any(vapply(
            escaped_eps,
            function(ep) {
              grepl(paste0("^", ep, "\\b"), after_genus, perl = TRUE)
            },
            logical(1)
          ))

          if (!in_recognised_binomial) {
            next
          }

          before_genus <- substring(s, 1, m - 1)

          at_sentence_start <- grepl(
            "^\\s*\\*?$",
            before_genus,
            perl = TRUE
          )

          is_first_binomial_for_genus <- first_binomial_for_genus_remaining

          if (is_first_binomial_for_genus) {
            first_binomial_for_genus_remaining <- FALSE
          }

          if (at_sentence_start || is_first_binomial_for_genus) {
            next
          }

          s <- paste0(
            substring(s, 1, m - 1),
            repl,
            substring(s, m + nchar(g))
          )
        }

        sentences[i] <- s
      }
    }

    paste(sentences, collapse = " ")
  })

  paste(unlist(paragraphs), collapse = "\n\n")
}
