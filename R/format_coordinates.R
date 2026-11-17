################################################################################
# Helper function: count decimals
################################################################################

#' Count decimal places in a numeric string
#'
#' Internal helper used to determine the number of digits after the decimal
#' point in a numeric string.
#'
#' @param x A character or numeric string.
#'
#' @return An integer giving the number of digits after the decimal point.
#'
#' @keywords internal
#' @noRd
count_decimals <- function(x) {

  if (grepl("\\.", x)) {
    nchar(strsplit(x, "\\.")[[1]][2])
  } else {
    0
  }
}


################################################################################
# Helper function: parse DMS
################################################################################

#' Parse DMS coordinates
#'
#' Internal helper used to parse degrees-minutes-seconds (DMS) coordinates with
#' a hemisphere indicator.
#'
#' @param dms_str A character string containing a DMS coordinate, for example
#'   a latitude of 34 degrees, 12 minutes, 30 seconds south.
#'
#' @return A list containing:
#'   \itemize{
#'     \item `dd`: signed decimal degrees
#'     \item `deg`: degree component
#'     \item `min`: minute component
#'     \item `sec`: second component
#'     \item `hemi`: hemisphere letter
#'     \item `input_precision`: the input precision, one of `"deg"`, `"min"`,
#'       or `"sec"`
#'   }
#'
#' @keywords internal
#' @noRd
parse_dms <- function(dms_str) {

  # REMOVE SPACES ---------------------------------------------------------------

  dms_str <- gsub("\\s+", "", dms_str)

  # EXTRACT COMPONENTS ----------------------------------------------------------

  matches <- regmatches(
    dms_str,
    regexec("^(\\d+)\u00B0(\\d+)'(?:([0-9.]+)\")?([NSEW])$", dms_str)
  )[[1]]

  if (length(matches) == 0) {
    stop("Invalid DMS format or missing hemisphere.", call. = FALSE)
  }

  deg  <- as.integer(matches[2])
  min  <- as.integer(matches[3])
  sec  <- ifelse(is.na(matches[4]), NA, as.numeric(matches[4]))
  hemi <- toupper(matches[5])

  # CONVERT TO DECIMAL DEGREES --------------------------------------------------

  dd <- deg + min / 60 + ifelse(is.na(sec), 0, sec / 3600)

  if (hemi %in% c("S", "W")) {
    dd <- -dd
  }

  # DETERMINE INPUT PRECISION ---------------------------------------------------

  input_precision <- if (is.na(sec)) {
    "min"
  } else {
    "sec"
  }

  list(
    dd = dd,
    deg = deg,
    min = min,
    sec = sec,
    hemi = hemi,
    input_precision = input_precision
  )
}


################################################################################
# Helper function: parse DD with hemisphere
################################################################################

#' Parse decimal degrees with hemisphere
#'
#' Internal helper used to parse decimal degree coordinates supplied with a
#' hemisphere indicator.
#'
#' @param dd_str A character string containing a decimal degree coordinate with
#'   hemisphere, for example `"34.2S"`.
#'
#' @return A list containing:
#'   \itemize{
#'     \item `dd`: signed decimal degrees
#'     \item `hemi`: hemisphere letter
#'     \item `input_decimals`: the number of decimal places in the input
#'   }
#'
#' @keywords internal
#' @noRd
parse_dd_hemisphere <- function(dd_str) {

  # REMOVE SPACES ---------------------------------------------------------------

  dd_str <- gsub("\\s+", "", dd_str)

  # SEPARATE VALUE AND HEMISPHERE -----------------------------------------------

  matches <- regmatches(
    dd_str,
    regexec("^(-?[0-9.]+)([NSEW])$", dd_str, ignore.case = TRUE)
  )[[1]]

  if (length(matches) == 0) {
    stop(
      "dd_hemisphere format requires a value and hemisphere, e.g. '34.2S'.",
      call. = FALSE
    )
  }

  dd_val <- matches[2]
  hemi   <- toupper(matches[3])

  # CONVERT TO SIGNED DECIMAL DEGREES -------------------------------------------

  dd <- as.numeric(dd_val)

  if (!(hemi %in% c("N", "S", "E", "W"))) {
    stop("Invalid hemisphere indicator.", call. = FALSE)
  }

  if (hemi %in% c("S", "W")) {
    dd <- -abs(dd)
  } else {
    dd <- abs(dd)
  }

  input_decimals <- count_decimals(dd_val)

  list(
    dd = dd,
    hemi = hemi,
    input_decimals = input_decimals
  )
}


################################################################################
# Helper function: format decimal degrees
################################################################################

#' Format decimal degrees as a character string
#'
#' Internal helper used to format decimal degrees either as signed values or as
#' positive values with a hemisphere indicator.
#'
#' @param dd Numeric decimal degrees.
#' @param with_hemi Logical. Should a hemisphere letter be appended? Defaults to
#'   `FALSE`.
#' @param hemi_type Character. Hemisphere type to use when `with_hemi = TRUE`.
#'   Must be either `"lat"` for latitude (`N`/`S`) or `"lon"` for longitude
#'   (`E`/`W`).
#' @param precision_out Numeric. Number of decimal places to use in the output.
#' @param input_decimals Optional numeric. Maximum number of decimal places
#'   allowed based on the input precision.
#' @param output_style Character. One of `"compact"` or `"spaced"`. Controls
#'   whether a space is inserted before the hemisphere letter.
#'
#' @return A character string representing the formatted decimal degree value.
#'
#' @keywords internal
#' @noRd
to_dd_string <- function(dd,
                         with_hemi = FALSE,
                         hemi_type = c("lat", "lon"),
                         precision_out = NULL,
                         input_decimals = NULL,
                         output_style = c("compact", "spaced")) {

  hemi_type <- match.arg(hemi_type)
  output_style <- match.arg(output_style)

  # DETERMINE OUTPUT DECIMALS ---------------------------------------------------

  dec <- if (!is.null(precision_out)) {
    if (is.null(input_decimals)) {
      precision_out
    } else {
      min(input_decimals, precision_out)
    }
  } else {
    if (is.null(input_decimals)) {
      5
    } else {
      input_decimals
    }
  }

  dd_rounded <- round(dd, dec)

  # FORMAT OUTPUT ---------------------------------------------------------------

  if (with_hemi) {

    hemi <- if (hemi_type == "lat") {
      ifelse(dd < 0, "S", "N")
    } else {
      ifelse(dd < 0, "W", "E")
    }

    hemi_sep <- if (output_style == "spaced") " " else ""

    sprintf(
      paste0("%.", dec, "f%s%s"),
      abs(dd_rounded),
      hemi_sep,
      hemi
    )

  } else {
    sprintf(paste0("%.", dec, "f"), dd_rounded)
  }
}


################################################################################
# Helper function: format decimal degrees as DMS
################################################################################

#' Format decimal degrees as DMS
#'
#' Internal helper used to format decimal degree values as DMS coordinates,
#' rounding according to either the original input precision or a user-supplied
#' output precision.
#'
#' @param dd Numeric decimal degrees.
#' @param orig Optional parsed coordinate list. Used to preserve hemisphere type
#'   and input precision where relevant.
#' @param hemi_type Character. Hemisphere type to use when `orig` does not
#'   contain hemisphere information. Must be either `"lat"` or `"lon"`.
#' @param precision_out Character. Output precision for DMS coordinates. Must be
#'   one of `"deg"`, `"min"`, or `"sec"`. If `NULL`, the original input
#'   precision is used where available.
#' @param output_style Character. One of `"compact"` or `"spaced"`. Controls
#'   whether spaces are inserted between DMS components.
#'
#' @return A character string containing the formatted DMS coordinate.
#'
#' @keywords internal
#' @noRd
to_dms_string <- function(dd,
                          orig = NULL,
                          hemi_type = c("lat", "lon"),
                          precision_out = NULL,
                          output_style = c("compact", "spaced")) {

  hemi_type <- match.arg(hemi_type)
  output_style <- match.arg(output_style)

  abs_dd <- abs(dd)

  # DETERMINE OUTPUT PRECISION --------------------------------------------------

  prec <- if (!is.null(precision_out)) {
    precision_out
  } else {
    if (!is.null(orig) && !is.null(orig$input_precision)) {
      orig$input_precision
    } else {
      "sec"
    }
  }

  # EXTRACT COMPONENTS ----------------------------------------------------------

  deg <- floor(abs_dd)
  min <- floor((abs_dd - deg) * 60)
  sec <- (abs_dd - deg - min / 60) * 3600

  # ROUND COMPONENTS ------------------------------------------------------------

  if (prec == "sec") {

    sec <- round(sec)

    if (sec == 60) {
      sec <- 0
      min <- min + 1
    }

    if (min == 60) {
      min <- 0
      deg <- deg + 1
    }

  } else if (prec == "min") {

    if (sec >= 30) {
      min <- min + 1
    }

    sec <- NULL

    if (min == 60) {
      min <- 0
      deg <- deg + 1
    }

  } else if (prec == "deg") {

    if ((min + sec / 60) >= 30) {
      deg <- deg + 1
    }

    min <- NULL
    sec <- NULL
  }

  # DETERMINE HEMISPHERE --------------------------------------------------------

  hemi <- if (!is.null(orig) && !is.null(orig$hemi)) {
    if (dd < 0) {
      if (orig$hemi %in% c("N", "S")) "S" else "W"
    } else {
      if (orig$hemi %in% c("N", "S")) "N" else "E"
    }
  } else {
    if (dd < 0) {
      if (hemi_type == "lat") "S" else "W"
    } else {
      if (hemi_type == "lat") "N" else "E"
    }
  }

  # DETERMINE SPACING -----------------------------------------------------------

  sep <- if (output_style == "spaced") " " else ""

  # FORMAT OUTPUT ---------------------------------------------------------------

  if (prec == "deg") {
    sprintf("%d\u00B0%s%s", deg, sep, hemi)
  } else if (prec == "min") {
    sprintf("%d\u00B0%s%02d'%s%s", deg, sep, min, sep, hemi)
  } else {
    sprintf("%d\u00B0%s%02d'%s%02d\"%s%s", deg, sep, min, sep, sec, sep, hemi)
  }
}


################################################################################
# Main function: format coordinates
################################################################################

#' Format coordinates between common coordinate styles
#'
#' Converts coordinates from one common coordinate style to another. This is
#' useful when specimen data, labels, publications, or collection databases use
#' different coordinate formats.
#'
#' The function can read and write three coordinate styles:
#'
#' \itemize{
#'   \item \code{"dms"} - degrees, minutes, and seconds, such as 34 degrees, 12 minutes, 30 seconds south.
#'   \item \code{"dd_signed"} - signed decimal degrees, such as \code{-34.2083}.
#'   \item \code{"dd_hemisphere"} - decimal degrees with a hemisphere letter,
#'     such as \code{34.2083S}.
#' }
#'
#' Use \code{input_format} to tell the function what style your coordinates are
#' currently in, and \code{output_format} to tell it what style you want
#' returned.
#'
#' The \code{precision} argument controls how detailed the output should be. For
#' decimal degree output, give the number of decimal places, such as \code{4}.
#' For degrees-minutes-seconds output, use \code{"deg"}, \code{"min"}, or
#' \code{"sec"}.
#'
#' If you are converting between degrees-minutes-seconds and decimal degrees,
#' you should supply \code{precision}, because the function cannot always know
#' how much detail you want in the new format. If you are converting within the
#' same general style, the function will preserve the input precision where
#' possible. It will not add extra precision beyond what was present in the
#' input.
#'
#' The \code{output_style} argument controls spacing in formatted coordinates.
#' For DMS output, \code{"compact"} places the degree, minute and second
#' symbols immediately beside their numbers, while \code{"spaced"} inserts
#' spaces. For decimal degrees with a hemisphere letter, compact output looks
#' like \code{34.2083S}, and spaced output looks like \code{34.2083 S}.
#' This only affects \code{"dms"} and \code{"dd_hemisphere"} output.
#'
#' If you are converting from signed decimal degrees to an output style that
#' uses hemisphere letters, you must also supply \code{coordinate_type}. Use
#' \code{"latitude"} for north/south coordinates and \code{"longitude"} for
#' east/west coordinates. This is needed because a value such as \code{-34.2083}
#' shows direction by sign, but does not say whether the coordinate is a latitude
#' or longitude.
#'
#' Missing or invalid values are handled according to \code{on_error}. By
#' default, invalid values are returned unchanged and an informative warning is
#' issued. This allows specimen datasets with a mixture of valid coordinates,
#' missing coordinates, and legacy values to be processed without stopping the
#' whole workflow.
#'
#' @section An optional preparation step:
#' This helper converts coordinate text to the display format your material-examined list needs. It does not change which specimen records are included or decide whether the coordinates themselves are accurate.
#'
#' Use it only when your collection data need conversion; if coordinates already have the required formatting, use those values directly. The complete example converts signed decimal degrees to degrees and minutes before compiling the material list.
#'
#' @param coords Character vector. The coordinates to format.
#' @param input_format Character. The current coordinate style. Must be one of
#'   \code{"dms"}, \code{"dd_signed"}, or \code{"dd_hemisphere"}.
#' @param output_format Character. The coordinate style to return. Must be one
#'   of \code{"dms"}, \code{"dd_signed"}, or \code{"dd_hemisphere"}.
#' @param precision Output precision. For decimal degree output, use a
#'   non-negative number giving the number of decimal places, such as \code{4}.
#'   For degrees-minutes-seconds output, use \code{"deg"}, \code{"min"}, or
#'   \code{"sec"}.
#' @param output_style Character. Either \code{"compact"} or \code{"spaced"}.
#'   Controls spacing in \code{"dms"} and \code{"dd_hemisphere"} output.
#'   Ignored for \code{"dd_signed"} output.
#' @param coordinate_type Character. Either \code{"latitude"} or
#'   \code{"longitude"}. Required only when converting from
#'   \code{"dd_signed"} to \code{"dms"} or \code{"dd_hemisphere"}.
#' @param on_error Character. How to handle missing or invalid coordinates.
#'   Use \code{"keep"} to return the original input value unchanged,
#'   \code{"NA"} to return \code{NA_character_}, or \code{"stop"} to stop with
#'   an error. Defaults to \code{"keep"}.
#'
#' @return A character vector containing the coordinates in the requested output
#'   format. Missing or invalid values are returned according to
#'   \code{on_error}.
#'
#' @examples
#' # Convert degrees, minutes, and seconds to signed decimal degrees
#' format_coordinates(
#'   coords = c("34\u00B012'30\"S", "31\u00B045'00\"S"),
#'   input_format = "dms",
#'   output_format = "dd_signed",
#'   precision = 4
#' )
#'
#' # Keep invalid values unchanged while converting valid values
#' format_coordinates(
#'   coords = c("34\u00B012'30\"S", "", "not recorded"),
#'   input_format = "dms",
#'   output_format = "dd_signed",
#'   precision = 4
#' )
#'
#' @export
format_coordinates <- function(coords,
                               input_format = c("dms", "dd_signed", "dd_hemisphere"),
                               output_format = c("dms", "dd_signed", "dd_hemisphere"),
                               precision = NULL,
                               output_style = c("compact", "spaced"),
                               coordinate_type = NULL,
                               on_error = c("keep", "stop", "NA")) {

  # CHECK ARGUMENTS -------------------------------------------------------------

  input_format  <- match.arg(input_format)
  output_format <- match.arg(output_format)
  output_style  <- match.arg(output_style)
  on_error      <- match.arg(on_error)

  if (!is.character(coords)) {
    stop(
      "`coords` must be a character vector of coordinates.",
      call. = FALSE
    )
  }

  if (!is.null(coordinate_type)) {
    coordinate_type <- match.arg(
      coordinate_type,
      choices = c("latitude", "longitude")
    )
  }

  if (output_format == "dd_signed") {
    warning(
      "`output_style` has no effect when `output_format = 'dd_signed'` and will be ignored.",
      call. = FALSE
    )
  }

  # DETERMINE WHETHER COORDINATE TYPE IS REQUIRED -------------------------------

  needs_coordinate_type <- (
    input_format == "dd_signed" &&
      output_format %in% c("dms", "dd_hemisphere")
  )

  if (needs_coordinate_type && is.null(coordinate_type)) {
    stop(
      "`coordinate_type` must be specified as 'latitude' or 'longitude' when ",
      "formatting from `dd_signed` to `dms` or `dd_hemisphere`, because signed ",
      "decimal degrees do not encode hemisphere type.",
      call. = FALSE
    )
  }

  # CONVERT EACH COORDINATE -----------------------------------------------------

  problems <- character(0)

  result <- vapply(
    seq_along(coords),
    FUN = function(i) {

      tryCatch({

        coord <- coords[[i]]

        if (is.na(coord) || !nzchar(trimws(coord))) {
          stop("Coordinate is missing or empty.", call. = FALSE)
        }

        coord_clean <- gsub("\\s+", "", coord)

        parsed <- switch(
          input_format,
          dms = parse_dms(coord_clean),
          dd_signed = {
            val <- as.numeric(coord_clean)

            if (is.na(val)) {
              stop("Invalid signed decimal degree format.", call. = FALSE)
            }

            input_dec <- count_decimals(coord_clean)

            list(
              dd = val,
              input_decimals = input_dec,
              hemi = NULL
            )
          },
          dd_hemisphere = parse_dd_hemisphere(coord_clean)
        )

        formats_dd <- c("dd_signed", "dd_hemisphere")

        diff_format_type <- (input_format %in% formats_dd) !=
          (output_format %in% formats_dd)

        if (diff_format_type && is.null(precision)) {
          stop(
            "Precision must be specified when formatting between DMS and decimal degree formats.",
            call. = FALSE
          )
        }

        precision_out <- precision

        # ENFORCE PRECISION RULES ------------------------------------------------

        if (!is.null(precision_out)) {

          if (output_format == "dms") {

            allowed <- c("deg", "min", "sec")

            if (!(precision_out %in% allowed)) {
              stop(
                "For DMS output, `precision` must be 'deg', 'min', or 'sec'.",
                call. = FALSE
              )
            }

            if (input_format == "dms") {
              prec_levels <- c("deg" = 1, "min" = 2, "sec" = 3)
              input_level <- prec_levels[parsed$input_precision]
              out_level   <- prec_levels[precision_out]

              if (out_level > input_level) {
                precision_out <- parsed$input_precision
              }
            }

          } else {

            if (!is.numeric(precision_out) ||
                length(precision_out) != 1 ||
                precision_out < 0) {
              stop(
                "For decimal degree output, `precision` must be a non-negative number.",
                call. = FALSE
              )
            }

            if (input_format %in% c("dd_signed", "dd_hemisphere")) {
              input_dec <- parsed$input_decimals

              if (!is.null(input_dec) && precision_out > input_dec) {
                precision_out <- input_dec
              }
            }
          }

        } else {

          if (!diff_format_type) {
            if (output_format == "dms") {
              precision_out <- parsed$input_precision
            } else {
              precision_out <- parsed$input_decimals

              if (is.null(precision_out)) {
                precision_out <- 5
              }
            }
          }
        }

        # DETERMINE HEMISPHERE TYPE --------------------------------------------

        hemi_type <- if (!is.null(parsed$hemi)) {
          if (parsed$hemi %in% c("N", "S")) {
            "lat"
          } else {
            "lon"
          }
        } else {
          if (coordinate_type == "latitude") "lat" else "lon"
        }

        # CONVERT TO OUTPUT FORMAT ---------------------------------------------

        switch(
          output_format,
          dms = to_dms_string(
            dd = parsed$dd,
            orig = if (input_format %in% c("dms", "dd_hemisphere")) parsed else NULL,
            hemi_type = hemi_type,
            precision_out = precision_out,
            output_style = output_style
          ),
          dd_signed = to_dd_string(
            dd = parsed$dd,
            with_hemi = FALSE,
            precision_out = precision_out,
            input_decimals = parsed$input_decimals,
            output_style = output_style
          ),
          dd_hemisphere = to_dd_string(
            dd = parsed$dd,
            with_hemi = TRUE,
            hemi_type = hemi_type,
            precision_out = precision_out,
            input_decimals = parsed$input_decimals,
            output_style = output_style
          )
        )

      }, error = function(e) {

        msg <- sprintf(
          "Coordinate %d was not converted: %s Input was: %s",
          i,
          e$message,
          ifelse(is.na(coords[[i]]), "NA", shQuote(coords[[i]]))
        )

        if (on_error == "stop") {
          stop(msg, call. = FALSE)
        }

        problems <<- c(problems, msg)

        if (on_error == "NA") {
          NA_character_
        } else {
          coords[[i]]
        }
      })
    },
    FUN.VALUE = character(1)
  )

  if (length(problems) > 0) {
    warning(
      length(problems),
      " coordinate value(s) could not be converted and were ",
      if (on_error == "NA") {
        "returned as NA."
      } else {
        "returned unchanged."
      },
      "\n",
      paste(problems, collapse = "\n"),
      call. = FALSE
    )
  }

  result
}
