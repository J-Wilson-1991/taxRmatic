#' Sort Values According to a Specified Ordering Scheme
#'
#' Internal helper for ordering countries, states, or other labels. A
#' preferred ordering can be incomplete: values not named in it are appended
#' alphabetically, rather than being silently discarded.
#'
#' @param values Character vector of values to order.
#' @param order_type `"alphabetical"`, `NULL`, or a character vector giving
#'   preferred values first. Other observed values are appended alphabetically.
#'
#' @return Character vector of ordered unique values.
#' @keywords internal
sort_levels <- function(values, order_type) {
  values <- unique(as.character(values))
  values <- values[!is.na(values)]

  if (is.null(order_type) || identical(order_type, "alphabetical")) {
    return(sort(values))
  }
  if (!is.character(order_type) || anyNA(order_type) ||
      any(!nzchar(trimws(order_type)))) {
    stop("`order_type` must be 'alphabetical' or a character vector of ",
         "non-empty preferred values.", call. = FALSE)
  }

  preferred <- unique(order_type[order_type %in% values])
  c(preferred, sort(setdiff(values, preferred)))
}

################################################################################
# MAIN FUNCTION
################################################################################
#
#' Build material examined sections from specimen data
#'
#' Creates formatted material examined text from a specimen data table.
#'
#' This function is usually used after specimen data have been read with
#' read_taxRmatic_specimen_data() and, where needed, after individual fields
#' have been prepared with helper functions such as format_specimen_counts(),
#' format_date_ranges(), format_coordinates(), or
#' format_custom_fields().
#'
#' It groups the specimens by taxon, type status, country, and, where present,
#' state or region. It then combines the selected specimen fields into material
#' examined text that can be used in a taxonomic manuscript.
#'
#' The output is returned as a taxRmatic_section object, so it can be combined
#' with descriptions, additional sections, figure references, and other
#' taxRmatic output.
#'
#' @details
#' ## How the function works
#'
#' Each row of taxRmatic_specimen_data should represent one specimen or
#' specimen lot. The function first separates the data by taxon. Within each
#' taxon, it then groups specimens by:
#'
#' \itemize{
#' \item `type_status`, for actual types such as holotypes and paratypes.
#'   Blank values identify non-type specimens; *Other material examined* is
#'   an output heading, not a value entered in this column.
#' \item country
#' \item state, if state or regional information is present
#' }
#'
#' The columns listed in fields_to_concat are then joined together for each
#' specimen record. For example, you might choose columns such as specimen count,
#' locality, date, collector, and specimen code.
#'
#' Empty values are skipped automatically, so missing information does not create
#' extra commas.
#'
#' ## Type status groups
#'
#' The type_status column controls the subsection labels in the output. For
#' example, holotype records are placed under a holotype subsection, and paratype
#' records are placed under a paratype or paratypes subsection.
#'
#' Leave `type_status` blank (`""` or `NA`) for all non-type specimens.
#' Those records automatically appear under the heading *Other material
#' examined*. Do **not** enter `"other material examined"` in `type_status`;
#' it is not a type designation.
#'
#' By default, actual types are ordered as holotype, neotype, syntype,
#' lectotype, allotype, and paratype, followed by non-type material.
#' Use `type_status_order` to change the preferred order of **type categories**.
#' Unlisted type categories are appended, and non-type material is always last.
#'
#' ## Country and state labels
#'
#' Countries are printed before the specimen records belonging to that country.
#' By default, country labels are converted to uppercase and followed by ": ".
#'
#' If state or regional information is present for a country, specimens are also
#' grouped by state within that country. By default, state labels are printed in
#' bold Markdown formatting.
#'
#' Country and state label formatting can be changed with
#' country_label_format and state_label_format, this is done using
#' `format_text()` style lists. The default settings use Markdown-style headings.
#'
#' ## Ordering specimens
#'
#' Use order_specimens_by to sort specimen records before they are printed.
#' For example, specimens can be ordered by sex, specimen code, locality, or any
#' other column in the specimen data.
#'
#' ## Compressing repeated data
#'
#' If several consecutive specimen records share some of the same collection data, such
#' as locality, date, and collector, compress_same_data = TRUE can be used to
#' shorten the output.
#'
#' In this mode, each specimen is compared with the specimen immediately before
#' it. If the columns listed in same_data_compare_fields are the same, the
#' repeated information is replaced with same_data_text, which defaults to
#' "same data".
#'
#' This is useful for material examined lists where several specimens were
#' collected from the same place at the same time.
#'
#' @section Building a material examined list:
#' First use `read_taxRmatic_specimen_data()` to import your records. The `fields_to_concat` argument is the sequence of column names to print for each specimen. These names must match the imported data; you do not need to delete columns you do not intend to print.
#'
#' The function groups material by taxon and type status, then country and state. Choose `order_specimens_by` only if you need additional ordering. If you enable `compress_same_data`, examine `same_data_compare_fields` carefully: they must identify the fields that are truly shared by consecutive records. A shorter list may incorrectly suggest that two distinct collecting events have identical data.
#'
#' The complete example shows printable registration numbers, coordinates and a compressed specimen list. Those formatting helpers are optional if your source data already look the way you want.
#'
#' @param taxRmatic_specimen_data A taxRmatic_specimen_data object, usually
#' created with read_taxRmatic_specimen_data().
#' @param fields_to_concat Character vector. Column names to print for each
#' specimen record, in the order they should appear.
#' @param field_separator Character(1). Text used to separate fields within each
#' specimen entry. Defaults to ", ".
#' @param specimen_separator Character(1). Text used to separate specimen
#' entries from one another within each country or state group. Defaults to
#' "; ".
#' @param order_specimens_by Optional character vector. Column names used to sort
#' specimen records before formatting.
#' @param compress_same_data Logical. If TRUE, repeated collection data can be
#' replaced with same_data_text. Defaults to FALSE.
#' @param same_data_compare_fields Character vector. Column names used to decide
#' whether the current specimen has the same data as the previous specimen.
#' Required when compress_same_data = TRUE.
#' @param same_data_prefix_fields Optional character vector. Column names to keep
#' before same_data_text when repeated data are compressed.
#' @param same_data_suffix_fields Optional character vector. Column names to keep
#' after same_data_text when repeated data are compressed.
#' @param same_data_text Character(1). Text used when repeated data are
#' compressed. Defaults to "same data".
#' @param type_status_order Character vector. Preferred order for type-status
#' groups.
#' @param country_order Either "alphabetical" or a character vector giving the
#' preferred order of countries.
#' @param country_label_format Named list controlling country-label formatting.
#' Passed to format_text(). Defaults to uppercase country labels followed by
#' ": ".
#' @param country_separator Character(1). Text used to separate country groups
#' within the same type-status entry. Defaults to ". ".
#' @param state_order Either "alphabetical" or a character vector giving the
#' preferred order of states or regions within each country.
#' @param state_label_format Named list controlling state-label formatting.
#' Passed to format_text(). Defaults to bold Markdown state labels followed
#' by ".** ".
#' @param state_separator Character(1). Text used to separate state groups within
#' the same country. Defaults to ". ".
#'
#' @return A taxRmatic_section object with one row for each taxon and
#' type-status group. The subsection column contains the type-status label,
#' and the text column contains the formatted material examined text.
#'
#' @examples
#' # Example specimen data
#' specimens <- data.frame(
#' taxon = c(
#' "Exemplomygale exampleus",
#' "Exemplomygale exampleus",
#' "Exemplomygale exampleus"
#' ),
#' type_status = c("holotype", "paratype", ""),
#' country = c("Australia", "Australia", "Australia"),
#' state = c("Western Australia", "Western Australia", "Western Australia"),
#' count = c("1 male", "1 female", "2 juveniles"),
#' locality = c("Perth", "Perth", "Dryandra Woodland"),
#' date = c("12 January 2024", "12 January 2024", "March 2024"),
#' specimen_code = c("WAM T123456", "WAM T123457", "WAM T123458"),
#' stringsAsFactors = FALSE
#' )
#'
#' class(specimens) <- c("taxRmatic_specimen_data", class(specimens))
#'
#' # Basic material examined section
#' get_taxRmatic_material_examined(
#' taxRmatic_specimen_data = specimens,
#' fields_to_concat = c("count", "locality", "date", "specimen_code")
#' )
#'
#' # Order specimen records before printing
#' specimens$print_order <- c(1, 3, 2)
#'
#' get_taxRmatic_material_examined(
#' taxRmatic_specimen_data = specimens,
#' fields_to_concat = c("count", "locality", "date", "specimen_code"),
#' order_specimens_by = "print_order"
#' )
#'
#' # Use "same data" for repeated collection details
#' get_taxRmatic_material_examined(
#' taxRmatic_specimen_data = specimens,
#' fields_to_concat = c("count", "locality", "date", "specimen_code"),
#' compress_same_data = TRUE,
#' same_data_compare_fields = c("locality", "date"),
#' same_data_prefix_fields = "count",
#' same_data_suffix_fields = "specimen_code"
#' )
#'
#' # Change how country and state labels are printed
#' get_taxRmatic_material_examined(
#' taxRmatic_specimen_data = specimens,
#' fields_to_concat = c("count", "locality", "date", "specimen_code"),
#' country_label_format = list(case = "title", suffix = ": "),
#' state_label_format = list(prefix = "", suffix = ". ")
#' )
#'
#' @export
get_taxRmatic_material_examined <- function(
    taxRmatic_specimen_data,
    fields_to_concat,
    field_separator = ", ",
    specimen_separator = "; ",
    order_specimens_by = NULL,
    compress_same_data = FALSE,
    same_data_compare_fields = NULL,
    same_data_prefix_fields = NULL,
    same_data_suffix_fields = NULL,
    same_data_text = "same data",
    type_status_order = c("holotype", "neotype", "syntype", "lectotype",
                          "allotype", "paratype"),
    country_order = "alphabetical",
    country_label_format = list(case = "uppercase", suffix = ": "),
    country_separator = ". ",
    state_order = "alphabetical",
    state_label_format = list(prefix = "**", suffix = ".** "),
    state_separator = ". "
) {

  # VALIDATE INPUT -------------------------------------------------------------

  if (!inherits(taxRmatic_specimen_data, "taxRmatic_specimen_data")) {
    stop(
      "`taxRmatic_specimen_data` must be an object of class ",
      "'taxRmatic_specimen_data'.",
      call. = FALSE
    )
  }

  # REPLACE NAS JUST IN CASE ---------------------------------------------------

  taxRmatic_specimen_data$state[is.na(taxRmatic_specimen_data$state)] <- ""
  taxRmatic_specimen_data$country[is.na(taxRmatic_specimen_data$country)] <- ""
  taxRmatic_specimen_data$type_status[is.na(taxRmatic_specimen_data$type_status)] <- ""
  taxRmatic_specimen_data$type_status <- trimws(as.character(
    taxRmatic_specimen_data$type_status
  ))
  invalid_non_type <- which(tolower(taxRmatic_specimen_data$type_status) ==
                              "other material examined")
  if (length(invalid_non_type)) {
    stop("Do not enter 'other material examined' in the 'type_status' column. ",
         "Leave type_status blank for non-type specimens (row(s): ",
         paste(invalid_non_type, collapse = ", "), ").", call. = FALSE)
  }

  # DEFINE INTERNAL HELPERS ----------------------------------------------------

  pluralise <- function(word) {
    if (grepl("(type)$", word, ignore.case = TRUE)) {
      paste0(word, "s")
    } else {
      word
    }
  }

  # SPLIT DATA BY TAXON --------------------------------------------------------

  all_rows <- list()
  taxon_list <- split(taxRmatic_specimen_data, taxRmatic_specimen_data$taxon)

  for (taxon_name in names(taxon_list)) {

    taxon_data <- taxon_list[[taxon_name]]

    # DETERMINE TYPE-STATUS GROUPS ---------------------------------------------

    type_vals <- taxon_data$type_status
    # Only actual types appear in type_status. Empty cells automatically
    # become the display group "other material examined" below.
    known_types <- unique(type_vals[nzchar(type_vals)])
    unknown_data <- taxon_data[!nzchar(type_vals), , drop = FALSE]

    ordered_types <- type_status_order[type_status_order %in% known_types]
    unordered_types <- setdiff(known_types, type_status_order)

    types <- c(
      ordered_types,
      unordered_types,
      if (nrow(unknown_data) > 0) "other material examined"
    )

    for (type in types) {

      # SUBSET DATA FOR CURRENT TYPE STATUS ------------------------------------

      type_data <- if (type == "other material examined") {
        unknown_data
      } else {
        taxon_data[taxon_data$type_status == type, , drop = FALSE]
      }

      if (nrow(type_data) == 0) next

      # GROUP CURRENT TYPE STATUS BY COUNTRY -----------------------------------

      country_strings <- character()
      countries <- sort_levels(type_data$country, country_order)

      for (country in countries) {

        country_data <- type_data[type_data$country == country, , drop = FALSE]

        country_name <- format_text(
          country,
          style = country_label_format
        )

        has_state <- nzchar(country_data$state[1])

        if (!has_state) {

          material_string <- get_material_string(
            data_frame = country_data,
            fields_to_concat = fields_to_concat,
            field_separator = field_separator,
            specimen_separator = specimen_separator,
            order_specimens_by = order_specimens_by,
            compress_same_data = compress_same_data,
            same_data_compare_fields = same_data_compare_fields,
            same_data_prefix_fields = same_data_prefix_fields,
            same_data_suffix_fields = same_data_suffix_fields,
            same_data_text = same_data_text
          )

          country_strings <- c(
            country_strings,
            paste0(country_name, material_string)
          )

        } else {

          # GROUP CURRENT COUNTRY BY STATE --------------------------------------

          state_strings <- character()
          states <- sort_levels(country_data$state, state_order)

          for (state in states) {

            state_data <- country_data[country_data$state == state, , drop = FALSE]

            state_name <- format_text(
              state,
              style = state_label_format
            )

            material_string <- get_material_string(
              data_frame = state_data,
              fields_to_concat = fields_to_concat,
              field_separator = field_separator,
              specimen_separator = specimen_separator,
              order_specimens_by = order_specimens_by,
              compress_same_data = compress_same_data,
              same_data_compare_fields = same_data_compare_fields,
              same_data_prefix_fields = same_data_prefix_fields,
              same_data_suffix_fields = same_data_suffix_fields,
              same_data_text = same_data_text
            )

            state_strings <- c(
              state_strings,
              paste0(state_name, material_string)
            )
          }

          country_strings <- c(
            country_strings,
            paste0(
              country_name,
              paste(state_strings, collapse = state_separator)
            )
          )
        }
      }

      # BUILD OUTPUT ROW FOR CURRENT TYPE STATUS -------------------------------

      type_entry <- paste(country_strings, collapse = country_separator)

      type_label <- if (nrow(type_data) > 1) {
        pluralise(type)
      } else {
        type
      }

      all_rows[[length(all_rows) + 1]] <- new_taxRmatic_section_row(
        taxon = taxon_name,
        subsection = type_label,
        text = type_entry
      )

    }
  }

  # RETURN OUTPUT --------------------------------------------------------------

  if (!length(all_rows)) {
    return(empty_taxRmatic_section())
  }

  out <- do.call(rbind, all_rows)
  rownames(out) <- NULL
  out
}
