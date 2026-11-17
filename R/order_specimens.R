#' Order specimen rows using user-supplied rules
#'
#' Sort specimen records or description rows by selected columns. For example,
#' you can put males before females or choose an order for type specimens.
#' If you have already arranged the rows in your spreadsheet, you generally
#' do not need to use this function.
#'
#' @details
#' `specimen_order` must be a named list. Each name must match a column in
#' `sub_df`, and each value defines how that column should be ordered.
#'
#' Ordering rules are applied in the order they appear in `specimen_order`.
#' The first rule is used as the primary sort key, the second as the secondary
#' sort key, and so on.
#'
#' Supported named rules are:
#'
#' \itemize{
#'   \item `"alphabetical"`: sort values alphabetically from A to Z.
#'   \item `"alphabetical_desc"`: sort values alphabetically from Z to A.
#'   \item `"numeric"`: sort values numerically from smallest to largest.
#'   \item `"numeric_desc"`: sort values numerically from largest to smallest.
#' }
#'
#' Alternatively, a custom ordering vector can be supplied. This is useful for
#' biological or taxonomic categories that should not be sorted alphabetically.
#' For example:
#'
#' ```
#' list(sex = c("male", "female", "juvenile"))
#' ```
#'
#' Custom ordering vectors can also contain only one value. For example:
#'
#' ```
#' list(type_status = c("holotype"))
#' ```
#'
#' places records with `type_status == "holotype"` first, with all other
#' non-missing values placed afterwards.
#'
#' Values not found in a custom ordering vector are placed after the explicitly
#' ordered values and a warning is emitted. If additional ordering rules are
#' supplied, those later rules are used to order the unmatched values. If no
#' additional ordering rules are supplied, unmatched values retain their input
#' order. Missing or empty values are also placed at the end.
#'
#' Columns used for ordering must be single-value columns. List-columns, such as
#' automatically split specimen-code columns, cannot be used for ordering.
#'
#' The strings `"alphabetical"`, `"alphabetical_desc"`, `"numeric"`, and
#' `"numeric_desc"` are reserved as named ordering rules when supplied as a
#' single character value. If a column genuinely contains one of these values and
#' you need to use it as a custom order, include at least one additional custom
#' value in the vector.
#'
#' @param sub_df Data frame to reorder.
#' @param specimen_order Named list of ordering rules. Names must match columns
#'   in `sub_df`. Values may be one of the supported named rules
#'   (`"alphabetical"`, `"alphabetical_desc"`, `"numeric"`,
#'   `"numeric_desc"`) or a custom ordering vector.
#'
#' @return A reordered data frame with the same columns as `sub_df`.
#'
#' @examples
#' specimens <- data.frame(
#'   sex = c("female", "male", "juvenile"),
#'   type_status = c("paratype", "holotype", ""),
#'   regno = c("T123", "T121", "T122"),
#'   stringsAsFactors = FALSE
#' )
#'
#' order_specimens(
#'   specimens,
#'   specimen_order = list(
#'     sex = c("male", "female", "juvenile"),
#'     regno = "alphabetical"
#'   )
#' )
#'
#' order_specimens(
#'   specimens,
#'   specimen_order = list(
#'     type_status = c("holotype")
#'   )
#' )
#'
#' order_specimens(
#'   specimens,
#'   specimen_order = list(
#'     type_status = c("holotype"),
#'     regno = "alphabetical"
#'   )
#' )
#'
#' @export
order_specimens <- function(sub_df, specimen_order) {

  # CHECK INPUTS ----------------------------------------------------------------

  if (!is.data.frame(sub_df)) {
    stop("`sub_df` must be a data frame.", call. = FALSE)
  }

  if (is.null(specimen_order) || length(specimen_order) == 0) {
    return(sub_df)
  }

  if (!is.list(specimen_order) || is.null(names(specimen_order))) {
    stop(
      "`specimen_order` must be a named list, with names matching columns ",
      "in `sub_df`.",
      call. = FALSE
    )
  }

  if (any(names(specimen_order) == "")) {
    stop(
      "All elements of `specimen_order` must be named.",
      call. = FALSE
    )
  }

  named_rules <- c(
    "alphabetical",
    "alphabetical_desc",
    "numeric",
    "numeric_desc"
  )

  # BUILD ORDER KEYS ------------------------------------------------------------

  order_keys <- lapply(names(specimen_order), function(col) {

    if (!col %in% names(sub_df)) {
      stop("Column not found in `sub_df`: '", col, "'.", call. = FALSE)
    }

    vals <- sub_df[[col]]
    rule <- specimen_order[[col]]

    if (is.list(vals)) {
      stop(
        "Column '", col, "' cannot be used for ordering because it is a ",
        "list-column. Use a single-value column for ordering instead.",
        call. = FALSE
      )
    }

    if (is.null(rule)) {
      return(seq_along(vals))
    }

    vals_chr <- trimws(as.character(vals))
    missing_vals <- is.na(vals_chr) | !nzchar(vals_chr)

    # NAMED ORDERING RULES ------------------------------------------------------

    if (is.character(rule) && length(rule) == 1 && rule %in% named_rules) {

      if (rule == "alphabetical") {
        vals_chr[missing_vals] <- NA_character_
        return(vals_chr)
      }

      if (rule == "alphabetical_desc") {
        vals_chr[missing_vals] <- NA_character_
        return(-xtfrm(vals_chr))
      }

      if (rule == "numeric") {
        numeric_vals <- suppressWarnings(as.numeric(vals_chr))
        numeric_vals[missing_vals] <- NA_real_
        return(numeric_vals)
      }

      if (rule == "numeric_desc") {
        numeric_vals <- suppressWarnings(as.numeric(vals_chr))
        numeric_vals[missing_vals] <- NA_real_
        return(-numeric_vals)
      }
    }

    # CUSTOM ORDERING VECTOR ----------------------------------------------------

    if (is.atomic(rule)) {
      rule_chr <- trimws(as.character(rule))
      rule_chr <- rule_chr[!is.na(rule_chr) & nzchar(rule_chr)]

      if (!length(rule_chr)) {
        stop(
          "Custom ordering vector for column '", col,
          "' must contain at least one non-missing value.",
          call. = FALSE
        )
      }

      match_idx <- match(vals_chr, rule_chr)
      match_idx[missing_vals] <- NA_integer_

      unmatched <- unique(vals_chr[is.na(match_idx) & !missing_vals])

      if (length(unmatched)) {
        warning(
          "Unmatched values in column '", col, "': ",
          paste(unmatched, collapse = ", "),
          ". These will be placed after explicitly ordered values.",
          call. = FALSE
        )
      }

      return(match_idx)
    }

    stop(
      "Invalid ordering specification for column '", col, "'.",
      call. = FALSE
    )
  })

  # APPLY ORDER ----------------------------------------------------------------
  # Add the original row position as a final tie-breaker. This ensures that
  # unmatched values, or any other ties not resolved by the supplied ordering
  # rules, retain their input order.

  ord <- do.call(
    order,
    c(
      order_keys,
      list(seq_len(nrow(sub_df))),
      list(na.last = TRUE)
    )
  )

  sub_df[ord, , drop = FALSE]
}
