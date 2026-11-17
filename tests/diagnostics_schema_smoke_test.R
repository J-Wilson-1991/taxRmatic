# Run from the package project after devtools::load_all():
# source("tests/diagnostics_schema_smoke_test.R")

library(taxRmatic)

input <- data.frame(
  taxon_subset = rep("south", 5L),
  subsection = c("", "Male morphology", "", "Female morphology", ""),
  specimen_category = c("", "male", "male", "female", ""),
  statement_type = c("prefix", "comparison", "suffix", "comparison", "suffix"),
  starting_text = c("", "Males of", "", "Females of", ""),
  taxon = rep("Exemplomygale alpha", 5L),
  comparison_text = c("", "can be distinguished from those of", "",
                      "can be distinguished from those of", ""),
  comparison_taxon = c("", "Exemplomygale beta", "", "Exemplomygale beta", ""),
  figure_filter = rep("", 5L),
  statement = c("An Australian trapdoor spider", "by a curved embolus",
                "Mature males occur in spring", "by the shape of spermathecae",
                "Known from Western Australia"),
  stringsAsFactors = FALSE
)

path <- tempfile(fileext = ".csv")
write.csv(input, path, row.names = FALSE, na = "")
diag_data <- suppressMessages(read_taxRmatic_diagnostics(path))
stopifnot(
  inherits(diag_data, "taxRmatic_diagnostics"),
  identical(diag_data$taxon_subset[1L], "south"),
  identical(diag_data$subsection[2L], "Male morphology")
)

# Names of categories must not be printed as headings. Category 'male' is
# deliberately different from the display heading 'Male morphology'.
separated <- get_taxRmatic_diagnosis(
  diag_data, "Exemplomygale alpha", separate_categories = TRUE,
  warn_unresolved = FALSE
)
stopifnot(identical(as.character(separated$subsection),
                    c("", "Male morphology", "Female morphology", "")))
stopifnot(nrow(separated) == 4L)
combined <- get_taxRmatic_diagnosis(
  diag_data, "Exemplomygale alpha", separate_categories = FALSE,
  warn_unresolved = FALSE
)
stopifnot(nrow(combined) == 1L, identical(combined$subsection, ""))

all_generated <- get_taxRmatic_diagnoses(
  diag_data, separate_categories = TRUE, warn_unresolved = FALSE
)
stopifnot(identical(as.character(all_generated$subsection),
                    as.character(separated$subsection)))

# The heading is optional, while a specimen category remains required for
# comparison rows. If no heading is provided, the paragraph is still retained.
input$subsection[input$specimen_category == "male"] <- ""
write.csv(input, path, row.names = FALSE, na = "")
no_male_heading <- suppressMessages(read_taxRmatic_diagnostics(path))
out <- get_taxRmatic_diagnosis(
  no_male_heading, "Exemplomygale alpha", separate_categories = TRUE,
  warn_unresolved = FALSE
)
stopifnot(identical(as.character(out$subsection),
                    c("", "", "Female morphology", "")))

# Different non-blank headings for a single category within a subset are
# ambiguous, so the reader must report an error.
input$subsection[2L] <- "Male morphology"
input$subsection[3L] <- "Male characters"
write.csv(input, path, row.names = FALSE, na = "")
stopifnot(inherits(try(suppressMessages(read_taxRmatic_diagnostics(path)),
                       silent = TRUE), "try-error"))

# Changing the printed heading must not change which figure categories match.
input$subsection[3L] <- "Male morphology"
write.csv(input, path, row.names = FALSE, na = "")
fig_input <- suppressMessages(read_taxRmatic_diagnostics(path))
figs <- data.frame(
  taxon = c("Exemplomygale alpha", "Exemplomygale alpha"),
  specimen_category = c("male", "female"),
  figure_number = c("1", "2"),
  figure_part = c("A", "A"),
  stringsAsFactors = FALSE
)
with_figures <- get_taxRmatic_diagnosis(
  fig_input, "Exemplomygale alpha", figures = figs,
  separate_categories = TRUE, warn_unresolved = FALSE
)
stopifnot(grepl("Fig", with_figures$text[2L], fixed = TRUE))
stopifnot(!grepl("Fig. 2", with_figures$text[2L], fixed = TRUE))

unlink(path)
message("Diagnostic schema smoke tests passed.")
