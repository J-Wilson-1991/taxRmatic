#' Generate figure captions from figure metadata
#'
#' Creates figure captions from your figures table. Choose which columns
#' provide the main caption text, the descriptions of figure parts and,
#' optionally, scale-bar information. The result can be a table of captions
#' or one combined text string.
#'
#' @details
#' Supply figure metadata as a data frame, typically from
#' [read_taxRmatic_figures()]. Each row represents a figure or figure part.
#' The default grouping is by `figure_number`. You can group several figures
#' under another column with `group_field`; in that case, the individual figure
#' numbers are used as part labels. Within each group, unique `title_fields`
#' values form the shared caption title, while `part_fields` describe each row.
#' Multiple values in a shared title field produce a warning and are joined.
#'
#' Formatting arguments accept style lists used by [format_text()], such as
#' `list(prefix = "*", suffix = "*")`. No files are written by this function.
#'
#' @section An optional step:
#' Captions are not required for the basic treatment workflow. If you want to generate them, first prepare a figure table using `read_taxRmatic_figures()`. Choose `title_fields` for information shared by the figure group and `part_fields` for the text associated with each illustrated part; these must name actual columns in the figure table.
#'
#' Review the resulting captions, especially for irregular figures or plates containing several taxa. Use `output = "dataframe"` to inspect the caption table before choosing a combined string.
#'
#' @param taxRmatic_figures Figure metadata data frame, normally returned by
#'   [read_taxRmatic_figures()]. Requires `figure_number` and `figure_part`.
#' @param group_field Character(1). Column defining caption groups; default
#'   `"figure_number"`.
#' @param figure_label_singular,figure_label_plural Labels before one or more
#'   figure numbers (defaults `"Figure"` and `"Figures"`).
#' @param format_figure_label Optional [format_text()] style for the figure label.
#' @param title_fields Character vector of column names to include once as the
#'   title for each caption group.
#' @param title_separator Separator between title components.
#' @param format_title Optional style for the shared title.
#' @param part_fields Character vector of columns describing each figure part.
#' @param part_separator Separator between part-field values.
#' @param format_part_titles Optional style for the figure-part labels.
#' @param format_part_text Optional style for descriptive text of each part.
#' @param scale_field Optional column containing scale-bar labels or values.
#' @param scale_label Text introducing scale information (default `"Scales:"`).
#' @param format_scale_part_titles Optional style for part labels in scales;
#'   defaults to `format_part_titles` when unspecified.
#' @param output Either `"dataframe"` (default) or `"string"`. The string
#'   output separates captions with two newline characters.
#'
#' @return A data frame with columns `caption_group`, `figure_number`, and
#'   `caption`; or a single character string if `output = "string"`.
#'
#' @examples
#' figs <- data.frame(
#'   figure_number = c("1", "1", "2"),
#'   figure_part = c("A", "B", ""),
#'   taxon = c("Aname examplea", "Aname examplea", "Aname exampleb"),
#'   view = c("male palp, prolateral", "male palp, retrolateral", "habitus"),
#'   scale_bar = c("0.5 mm", "0.5 mm", "2 mm")
#' )
#' get_taxRmatic_figure_captions(
#'   figs,
#'   title_fields = "taxon",
#'   part_fields = "view",
#'   scale_field = "scale_bar"
#' )
#'
#' @seealso [read_taxRmatic_figures()], [format_figure_reference_text()]
#' @export
get_taxRmatic_figure_captions <- function(
    taxRmatic_figures,
    group_field = "figure_number",
    figure_label_singular = "Figure",
    figure_label_plural = "Figures",
    format_figure_label = NULL,
    title_fields,
    title_separator = ", ",
    format_title = NULL,
    part_fields,
    part_separator = ", ",
    format_part_titles = NULL,
    format_part_text = NULL,
    scale_field = NULL,
    scale_label = "Scales:",
    format_scale_part_titles = NULL,
    output = c("dataframe", "string")
) {

  output <- match.arg(output)

  for (nm in c("group_field", "figure_label_singular", "figure_label_plural",
               "title_separator", "part_separator", "scale_label")) {
    value <- get(nm)
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      stop("`", nm, "` must be one non-missing character string.",
           call. = FALSE)
    }
  }
  for (nm in c("title_fields", "part_fields")) {
    value <- get(nm)
    if (!is.character(value) || anyNA(value) ||
        any(!nzchar(trimws(value)))) {
      stop("`", nm, "` must be a character vector of column names.",
           call. = FALSE)
    }
  }
  if (!is.null(scale_field) &&
      (!is.character(scale_field) || length(scale_field) != 1L ||
       is.na(scale_field) || !nzchar(trimws(scale_field)))) {
    stop("`scale_field` must be NULL or one column name.", call. = FALSE)
  }

  if (is.null(format_scale_part_titles)) {
    format_scale_part_titles <- format_part_titles
  }

  if (!is.data.frame(taxRmatic_figures)) {
    stop("'taxRmatic_figures' must be a data.frame.", call. = FALSE)
  }

  required_cols <- c("figure_number", "figure_part", group_field, title_fields, part_fields)

  if (!is.null(scale_field)) {
    required_cols <- c(required_cols, scale_field)
  }

  missing_cols <- setdiff(required_cols, names(taxRmatic_figures))

  if (length(missing_cols) > 0) {
    stop(
      "The following required columns are missing from 'taxRmatic_figures': ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  clean_values <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    trimws(x)
  }

  collapse_unique <- function(x, group_id, field) {
    x <- clean_values(x)
    x <- x[nzchar(x)]
    ux <- unique(x)

    if (length(ux) > 1) {
      warning(
        "Multiple values found in title field '", field,
        "' for caption group ", group_id, ": ",
        paste(ux, collapse = ", "),
        call. = FALSE
      )
    }

    if (length(ux) == 0) {
      return("")
    }

    paste(ux, collapse = title_separator)
  }

  build_figure_label <- function(df_group) {
    fig_nums <- sort(unique(df_group$figure_number))

    if (length(fig_nums) == 0) {
      return("")
    }

    if (length(fig_nums) == 1) {
      out <- paste0(figure_label_singular, " ", fig_nums)
    } else {
      out <- paste0(figure_label_plural, " ", collapse_ranges(fig_nums))
    }

    if (!is.null(format_figure_label)) {
      out <- format_text(out, style = format_figure_label)
    }

    out
  }

  build_title <- function(df_group, group_id) {
    bits <- vapply(
      title_fields,
      function(field) collapse_unique(df_group[[field]], group_id, field),
      character(1)
    )

    bits <- bits[nzchar(bits)]

    if (length(bits) == 0) {
      return("")
    }

    out <- paste(bits, collapse = title_separator)

    if (!is.null(format_title)) {
      out <- format_text(out, style = format_title)
    }

    out
  }

  build_row_label <- function(row, use_figure_numbers_as_labels, label_style = NULL) {
    if (use_figure_numbers_as_labels) {
      label <- clean_values(row[["figure_number"]])
    } else {
      label <- clean_values(row[["figure_part"]])
    }

    if (!is.null(label_style) && nzchar(label)) {
      label <- format_text(label, style = label_style)
    }

    label
  }

  build_part <- function(row, use_figure_numbers_as_labels) {

    row_label <- build_row_label(
      row,
      use_figure_numbers_as_labels = use_figure_numbers_as_labels,
      label_style = format_part_titles
    )

    current_part_fields <- part_fields

    if (!is.null(scale_field)) {
      current_part_fields <- setdiff(current_part_fields, scale_field)
    }

    bits <- vapply(
      current_part_fields,
      function(field) {
        val <- clean_values(row[[field]])
        if (!nzchar(val)) "" else val
      },
      character(1)
    )

    bits <- bits[nzchar(bits)]
    part_text <- paste(bits, collapse = part_separator)

    if (!is.null(format_part_text) && nzchar(part_text)) {
      part_text <- format_text(part_text, style = format_part_text)
    }

    if (nzchar(row_label) && nzchar(part_text)) {
      return(paste0(row_label, " ", part_text))
    }

    if (nzchar(row_label)) {
      return(row_label)
    }

    if (nzchar(part_text)) {
      return(part_text)
    }

    ""
  }

  build_scales <- function(df_group, use_figure_numbers_as_labels) {

    if (is.null(scale_field)) {
      return("")
    }

    scale_vals <- clean_values(df_group[[scale_field]])

    row_labels <- vapply(
      seq_len(nrow(df_group)),
      function(i) {
        build_row_label(
          df_group[i, , drop = FALSE],
          use_figure_numbers_as_labels = use_figure_numbers_as_labels,
          label_style = format_scale_part_titles
        )
      },
      character(1)
    )

    keep <- nzchar(scale_vals)

    if (!any(keep)) {
      return("")
    }

    scale_vals <- scale_vals[keep]
    row_labels <- row_labels[keep]

    split_labels <- split(row_labels, scale_vals)

    scale_strings <- vapply(
      names(split_labels),
      function(scale_val) {
        labels <- unique(split_labels[[scale_val]])
        labels <- labels[nzchar(labels)]

        if (length(labels) > 0) {
          paste0(scale_val, " (", paste(labels, collapse = ", "), ")")
        } else {
          scale_val
        }
      },
      character(1)
    )

    scale_strings <- scale_strings[nzchar(scale_strings)]

    if (length(scale_strings) == 0) {
      return("")
    }

    paste0(scale_label, " ", paste(scale_strings, collapse = "; "), ".")
  }

  if (!nrow(taxRmatic_figures)) {
    empty <- data.frame(caption_group = character(), figure_number = character(),
                        caption = character(), stringsAsFactors = FALSE)
    if (output == "string") return("")
    return(empty)
  }
  if (anyNA(taxRmatic_figures[[group_field]]) ||
      any(!nzchar(trimws(as.character(taxRmatic_figures[[group_field]]))))) {
    stop("Caption grouping field `", group_field,
         "` contains blank or missing values.", call. = FALSE)
  }

  group_ids <- unique(taxRmatic_figures[[group_field]])

  out_list <- lapply(group_ids, function(group_id) {

    df_group <- taxRmatic_figures[
      taxRmatic_figures[[group_field]] == group_id,
      ,
      drop = FALSE
    ]

    use_figure_numbers_as_labels <- group_field != "figure_number"

    fig_label  <- build_figure_label(df_group)
    title_text <- build_title(df_group, group_id)
    scale_text <- build_scales(df_group, use_figure_numbers_as_labels)

    part_texts <- vapply(
      seq_len(nrow(df_group)),
      function(i) build_part(df_group[i, , drop = FALSE], use_figure_numbers_as_labels),
      character(1)
    )

    part_texts <- part_texts[nzchar(part_texts)]

    pieces <- c(fig_label, title_text, part_texts, scale_text)
    pieces <- pieces[nzchar(pieces)]

    data.frame(
      caption_group = group_id,
      figure_number = paste(sort(unique(df_group$figure_number)), collapse = ", "),
      caption = paste(pieces, collapse = " "),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, out_list)
  rownames(out) <- NULL

  if (output == "string") {
    return(paste(out$caption, collapse = "\n\n"))
  }

  out
}
