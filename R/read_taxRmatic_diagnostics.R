#' Read taxRmatic diagnostic statements from CSV
#'
#' Reads a CSV containing the statements used to build comparative diagnoses.
#' Each row supplies a comparison, introductory statement or concluding
#' statement. Pass the result to `get_taxRmatic_diagnosis()` for one taxon
#' or `get_taxRmatic_diagnoses()` for all taxa.
#'
#' @details
#' Each CSV row supplies one diagnostic statement. The ten required column
#' names are listed below. Cells in `subsection`, `figure_filter`, and some
#' other columns may be blank, but all columns must be present.
#'
#' - `taxon_subset`: comparison pool, e.g. `south` or `north`. Leave every
#'   value blank for one implicit pool, or fill it for every row.
#' - `subsection`: **printed heading** for a diagnostic category, e.g.
#'   `Male morphology` or `Females`. Leave blank to omit the heading. For a
#'   particular `taxon_subset` and `specimen_category`, nonblank headings must
#'   agree; the heading needs to be entered only once in that group.
#' - `specimen_category`: functional category, e.g. `male`, `female`, or `COI`.
#'   Used to group statements, determine comparison pools, and filter figures
#'   using the figures table's `specimen_category` field. It is **not** used
#'   as the printed heading. Leave blank for diagnosis-wide prefixes/suffixes.
#' - `statement_type`: `prefix`, `comparison`, or `suffix`.
#' - `starting_text`: optional introductory wording, e.g. `males of`.
#' - `taxon`: comma-separated full names of taxa covered by the statement.
#' - `comparison_text`: phrase such as `can be distinguished from those of`.
#' - `comparison_taxon`: comma-separated names of taxa being distinguished.
#'   In a comparison row, `all` (alone, case-insensitive) means every other
#'   taxon explicitly represented in comparison rows of the same subset and
#'   specimen category.
#' - `figure_filter`: optional extra filter expression in taxRmatic syntax.
#' - `statement`: diagnostic wording, including any figure tags.
#'
#' For `comparison` rows, `specimen_category`, `comparison_text`, and
#' `comparison_taxon` are required. Every row requires `taxon`,
#' `statement_type`, and `statement`; prefixes and suffixes may have a blank
#' specimen category. A diagnosis-wide prefix or suffix can also have its own
#' `subsection` heading, although normally these rows are left unheaded.
#'
#' The `subsection` column affects **presentation only**. With
#' `separate_categories = TRUE` in the generation functions, category paragraphs
#' receive the category's chosen heading, or no heading if it was left blank.
#' With `separate_categories = FALSE`, the entire diagnosis is one unheaded
#' paragraph. Changing `subsection` never changes figure filtering.
#'
#' The reader splits `taxon` and `comparison_taxon` into list-columns and
#' expands the `all` comparison shortcut within each `taxon_subset` and
#' `specimen_category` pool. Expansion depends on taxa explicitly named in
#' comparison rows; please review the reported pool sizes. Row order is
#' preserved, and the first occurrence of each category sets its default
#' order in generated diagnoses. Figure-filter expressions are validated with
#' [parse_figure_filter()]. Save CSV files in UTF-8.
#'
#' @section Planning diagnostic comparisons:
#' Write each diagnostic comparison as a row identifying the focal `taxon`, the `comparison_taxon` or taxa, and the distinguishing `statement`. The `starting_text` and `comparison_text` fields provide the words connecting those names to the statement. The generator assembles the sentence and can add matching figure references.
#'
#' Use `specimen_category` (for example `male` or `female`) to keep different comparison pools separate. The optional `subsection` column is the **printed heading**, not the figure-matching category. `taxon_subset` limits comparisons to a chosen group; leave it blank throughout to use one group. `comparison_taxon = all` considers only taxa explicitly represented in comparison rows for that subset and category.
#'
#' Choose `statement_type = prefix` or `suffix` for introductory or concluding statements, and `comparison` for a distinguishing character. The reader checks the table; `get_taxRmatic_diagnosis()` or `get_taxRmatic_diagnoses()` then selects and assembles comparisons. The *Preparing CSV data* vignette explains the iterative selection step by step.
#'
#' @param file Path to the diagnostics CSV.
#' @param fileEncoding File encoding, default `"UTF-8"`.
#' @param ... Additional arguments passed through to `read_taxRmatic_csv()`.
#'
#' @return A data frame of class `taxRmatic_diagnostics` and `data.frame`, with
#'   all input columns retained in their original row order; `taxon` and
#'   `comparison_taxon` are character-vector list-columns.
#'
#' @examples
#' \dontrun{
#' diagnostics <- read_taxRmatic_diagnostics("diagnostics.csv")
#' diagnosis <- get_taxRmatic_diagnosis(diagnostics, "Exemplomygale mainae")
#' }
#'
#' @export
read_taxRmatic_diagnostics <- function(file, fileEncoding = "UTF-8", ...) {

  # READ AND STANDARDISE -------------------------------------------------------
  df <- read_taxRmatic_csv(
    file = file,
    fileEncoding = fileEncoding,
    message_label = "Diagnostic statements",
    ...
  )
  df <- lowercase_column_names(df, inform = FALSE)

  required_cols <- c(
    "taxon_subset", "subsection", "specimen_category", "starting_text", "taxon", "statement_type",
    "comparison_text", "comparison_taxon", "figure_filter", "statement"
  )
  check_required_columns(
    df, required_cols,
    function_name = "read_taxRmatic_diagnostics",
    inform = FALSE
  )

  # Trim identifiers, but do not alter the wording of statements or text parts.
  structural_cols <- c(
    "taxon_subset", "subsection", "specimen_category", "taxon", "statement_type", "comparison_taxon"
  )
  df <- trim_columns(df, cols = structural_cols, inform = FALSE)
  df$statement_type <- tolower(df$statement_type)

  if (nrow(df) == 0L) {
    stop("The diagnostics CSV contains no diagnostic statements.", call. = FALSE)
  }

  # CHECK REQUIRED VALUES -----------------------------------------------------
  # A category is optional for diagnosis-wide prefixes and suffixes.
  for (col in c("taxon", "statement_type", "statement")) {
    bad <- which(is_missing_or_empty(df[[col]]))
    if (length(bad)) {
      stop("Empty '", col, "' in diagnostics row(s): ",
           paste(bad, collapse = ", "), ".", call. = FALSE)
    }
  }

  # SUBSETS: EITHER ALL FILLED OR ALL BLANK -----------------------------------
  # An entirely blank subset column means one implicit comparison pool. Mixed
  # blank and filled values are ambiguous and therefore rejected.
  blank_subset <- is_missing_or_empty(df$taxon_subset)
  if (any(blank_subset) && !all(blank_subset)) {
    stop(
      "The 'taxon_subset' column must either be entirely blank or contain a ",
      "value in every row. Please fill the missing values or leave all blank.",
      call. = FALSE
    )
  }

  valid_types <- c("prefix", "comparison", "suffix")
  bad_types <- which(!df$statement_type %in% valid_types)
  if (length(bad_types)) {
    stop("Invalid 'statement_type' in diagnostics row(s): ",
         paste(bad_types, collapse = ", "), ". Use prefix, comparison, or suffix.",
         call. = FALSE)
  }

  comparison_rows <- which(df$statement_type == "comparison")
  for (col in c("specimen_category", "comparison_text", "comparison_taxon")) {
    bad <- comparison_rows[is_missing_or_empty(df[[col]][comparison_rows])]
    if (length(bad)) {
      stop("Comparison rows require '", col, "'. Missing in row(s): ",
           paste(bad, collapse = ", "), ".", call. = FALSE)
    }
  }

  # Check for accidental empty items in comma-separated taxon lists.
  for (col in c("taxon", "comparison_taxon")) {
    values <- df[[col]]
    nonempty <- which(!is_missing_or_empty(values))
    bad <- nonempty[vapply(values[nonempty], function(x) {
      parts <- strsplit(x, ",", fixed = TRUE)[[1]]
      any(!nzchar(trimws(parts))) || grepl(",\\s*$", x)
    }, logical(1))]
    if (length(bad)) {
      stop("Empty taxon within '", col, "' in row(s): ",
           paste(bad, collapse = ", "), ".", call. = FALSE)
    }
  }

  # SPLIT TAXA ONCE AT IMPORT -------------------------------------------------
  # Work with character-vector list-columns downstream rather than repeatedly
  # parsing comma-separated cells when generating each species diagnosis.
  df$taxon <- lapply(df$taxon, split_and_trim, split_char = ",")
  df$comparison_taxon <- lapply(df$comparison_taxon, split_and_trim,
                                split_char = ",")

  # VALIDATE HEADING CONSISTENCY ----------------------------------------------
  # A category has at most one printed heading within a comparison pool.
  # It may be specified in any of the rows for that category; blank values
  # mean "no heading specified here", not a conflicting title.
  category_rows <- which(!is_missing_or_empty(df$specimen_category))
  if (length(category_rows)) {
    grouping <- interaction(df$taxon_subset[category_rows],
                            df$specimen_category[category_rows], drop = TRUE)
    for (rows in split(category_rows, grouping)) {
      headings <- unique(df$subsection[rows])
      headings <- headings[!is_missing_or_empty(headings)]
      if (length(headings) > 1L) {
        stop("Conflicting 'subsection' headings for taxon_subset '",
             df$taxon_subset[rows[1L]], "' and specimen_category '",
             df$specimen_category[rows[1L]], "': ",
             paste(shQuote(headings), collapse = ", "), ". ",
             "Use one heading per category (or leave it blank).",
             call. = FALSE)
      }
    }
  }

  # EXPAND 'ALL' COMPARISONS -------------------------------------------------
  # Work only from explicitly named taxa in comparison rows. In particular,
  # 'all' cannot itself establish which taxa have specimens for a category.
  # Using both taxon columns also includes comparison-only species.
  is_all <- vapply(df$comparison_taxon, function(x) {
    length(x) == 1L && tolower(x) == "all"
  }, logical(1L))

  # 'all' must stand alone; it is not valid in a prefix/suffix or alongside
  # separately listed names. The shortcut applies to comparison_taxon only.
  mentions_all <- vapply(df$comparison_taxon, function(x) {
    any(tolower(x) == "all")
  }, logical(1L))
  invalid_all <- which(mentions_all & (!is_all | df$statement_type != "comparison"))
  if (length(invalid_all)) {
    stop(
      "In 'comparison_taxon', 'all' must appear alone and only in comparison ",
      "rows. Invalid row(s): ", paste(invalid_all, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (any(is_all)) {
    # Pool key is the subset/category pair. Empty subsets are valid and treated
    # as one shared subset, while category remains specific.
    all_groups <- unique(df[is_all, c("taxon_subset", "specimen_category"), drop = FALSE])
    message("Expanding 'all' comparison taxa from the diagnostic CSV:")

    for (g in seq_len(nrow(all_groups))) {
      group_subset <- all_groups$taxon_subset[g]
      group_category <- all_groups$specimen_category[g]
      in_group <- which(
        df$statement_type == "comparison" &
        df$taxon_subset == group_subset &
        df$specimen_category == group_category
      )

      # Only explicitly listed taxa establish category availability.
      named_comparisons <- unlist(df$comparison_taxon[in_group], use.names = FALSE)
      named_comparisons <- named_comparisons[tolower(named_comparisons) != "all"]
      pool <- unique(c(
        unlist(df$taxon[in_group], use.names = FALSE),
        named_comparisons
      ))
      pool <- pool[nzchar(pool)]

      expand_rows <- in_group[is_all[in_group]]
      for (i in expand_rows) {
        df$comparison_taxon[[i]] <- setdiff(pool, df$taxon[[i]])
      }

      message(
        "  - Subset: ", if (nzchar(group_subset)) group_subset else "(all taxa)",
        "; category: ", group_category,
        "; inferred taxa: ", length(pool),
        "; 'all' rows: ", length(expand_rows)
      )
    }
    message("Please check that these inferred taxon groups are complete.")
  }

  # VALIDATE FIGURE-FILTER SYNTAX ----------------------------------------------
  # Keep the original strings because the current generators parse them later.
  filter_rows <- which(!is_missing_or_empty(df$figure_filter))
  if (length(filter_rows)) {
    for (i in filter_rows) {
      tryCatch(
        parse_figure_filter(df$figure_filter[i]),
        error = function(e) stop(
          "Invalid figure_filter in diagnostics row ", i, ": ",
          conditionMessage(e), call. = FALSE
        )
      )
    }
  }

  class(df) <- c("taxRmatic_diagnostics", "data.frame")

  message(
    "\nread_taxRmatic_diagnostics() completed successfully.\n",
    "  - Statements: ", nrow(df), "\n",
    "  - Subsets: ", length(unique(df$taxon_subset)),
    if (all(blank_subset)) " (implicit)" else "", "\n",
    "  - Categories: ", length(unique(df$specimen_category[
      !is_missing_or_empty(df$specimen_category)])), "\n",
    "  - Comparisons: ", length(comparison_rows), "\n",
    "  - Expanded all rows: ", sum(is_all), "\n",
    "  - Prefixes: ", sum(df$statement_type == "prefix"), "\n",
    "  - Suffixes: ", sum(df$statement_type == "suffix"), "\n",
    "  - Figure filters: ", length(filter_rows)
  )

  df
}
