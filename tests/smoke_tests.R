# Stand-alone base-R smoke tests; run by R CMD check or devtools::test().
# Do not depend on files in the developer's working directory.
library(taxRmatic)

# Each row is a paragraph. A heading prints only at the start of a named run.
sec <- data.frame(
  taxon = rep("Exemplomygale alba", 6),
  subsection = c("", "Male", "Male", "Female", "", "Male"),
  text = c("Introduction.", "First male paragraph.",
           "Second male paragraph.", "Female paragraph.",
           "An unheaded note.", "Another male paragraph."),
  stringsAsFactors = FALSE
)
class(sec) <- c("taxRmatic_section", "data.frame")
compiled <- compile_taxon_texts(list(Diagnosis = sec))
stopifnot(
  length(gregexpr("### Male.", compiled, fixed = TRUE)[[1]][
    gregexpr("### Male.", compiled, fixed = TRUE)[[1]] > 0
  ]) == 2L,
  grepl("First male paragraph.\n\nSecond male paragraph.", compiled, fixed = TRUE)
)
# Reordering input rows also reorders output paragraphs.
reordered <- sec[c(4, 1, 2, 3, 5, 6), , drop = FALSE]
reordered_text <- compile_taxon_texts(list(Diagnosis = reordered))
stopifnot(
  regexpr("Female paragraph.", reordered_text, fixed = TRUE)[1] <
    regexpr("Introduction.", reordered_text, fixed = TRUE)[1]
)

unheaded <- compile_taxon_texts(
  list(Diagnosis = sec), omit_subsection_headings = "Diagnosis"
)
stopifnot(
  !grepl("### Male.", unheaded, fixed = TRUE),
  grepl("First male paragraph.\n\nSecond male paragraph.", unheaded, fixed = TRUE)
)

# Section reader retains named + blank subsection rows in spreadsheet order.
f <- tempfile(fileext = ".csv")
write.csv(data.frame(
  taxon = c("Exemplomygale alba", "Exemplomygale alba", "Exemplomygale alba"),
  section = c("Remarks", "Remarks", "Remarks"),
  subsection = c("", "Male", "Male"),
  text = c("Unheaded.", "Male note one.", "Male note two.")
), f, row.names = FALSE)
sections <- suppressMessages(read_taxRmatic_sections(f))
unlink(f)
stopifnot("Remarks" %in% names(sections))
stopifnot(identical(as.character(sections$Remarks$text),
                    c("Unheaded.", "Male note one.", "Male note two.")))

# Type designations are for types only; blanks are non-type material.
sp <- data.frame(
  taxon = rep("Exemplomygale alba", 3),
  type_status = c("holotype", "", ""),
  country = rep("Australia", 3),
  state = c("Western Australia", "Victoria", "Western Australia"),
  count = c("1 male", "1 female", "1 juvenile"),
  locality = c("Perth", "Melbourne", "Geraldton"),
  stringsAsFactors = FALSE
)
class(sp) <- c("taxRmatic_specimen_data", "data.frame")
mat <- get_taxRmatic_material_examined(
  sp, fields_to_concat = c("count", "locality"),
  state_order = c("Western Australia")
)
stopifnot(any(grepl("other material examined", mat$subsection, fixed = TRUE)))
stopifnot(any(grepl("Victoria", mat$text, fixed = TRUE)))
stopifnot(any(grepl("Western Australia", mat$text, fixed = TRUE)))
sp$type_status[2] <- "other material examined"
bad_type <- try(get_taxRmatic_material_examined(
  sp, fields_to_concat = c("count", "locality")
), silent = TRUE)
stopifnot(inherits(bad_type, "try-error"))

# The CSV reader enforces the same rule for non-type material.
sp$type_status[2] <- ""
f <- tempfile(fileext = ".csv")
write.csv(as.data.frame(sp), f, row.names = FALSE)
read_sp <- suppressMessages(read_taxRmatic_specimen_data(f))
stopifnot(identical(as.character(read_sp$type_status), c("holotype", "", "")))
sp$type_status[2] <- "other material examined"
write.csv(as.data.frame(sp), f, row.names = FALSE)
bad_read <- try(suppressMessages(read_taxRmatic_specimen_data(f)), silent = TRUE)
stopifnot(inherits(bad_read, "try-error"))
unlink(f)

# General diagnosis statements remain unheaded and category groups are named.
f <- tempfile(fileext = ".csv")
write.csv(data.frame(
  taxon_subset = rep("", 4),
  subsection = c("", "Male morphology", "Female morphology", ""),
  specimen_category = c("", "male", "female", ""),
  starting_text = c("", "Males of", "Females of", ""),
  taxon = rep("Exemplomygale alba", 4),
  statement_type = c("prefix", "comparison", "comparison", "suffix"),
  comparison_text = c("", "can be distinguished from those of",
                      "can be distinguished from those of", ""),
  comparison_taxon = c("", "Exemplomygale beta", "Exemplomygale gamma", ""),
  figure_filter = rep("", 4),
  statement = c("A distinctive species", "by a strongly curved embolus",
                "by the shape of the spermathecae", "Known from Western Australia")
), f, row.names = FALSE)
dat <- suppressMessages(read_taxRmatic_diagnostics(f))
unlink(f)
diag <- get_taxRmatic_diagnosis(
  dat, taxon = "Exemplomygale alba", separate_categories = TRUE,
  warn_unresolved = FALSE
)
stopifnot(identical(as.character(diag$subsection), c("", "Male morphology", "Female morphology", "")))
coverage <- attr(diag, "coverage")
stopifnot(nrow(coverage) == 2L, all(coverage$total == 1L))

# Caption generator must be publicly exported and support a simple table.
figs <- data.frame(figure_number = c("1", "1"), figure_part = c("A", "B"),
                   taxon = c("Exemplomygale alba", "Exemplomygale alba"),
                   view = c("dorsal", "lateral"),
                   scale = c("1 mm", "1 mm"))
captions <- get_taxRmatic_figure_captions(
  figs, title_fields = "taxon", part_fields = "view", scale_field = "scale"
)
stopifnot(nrow(captions) == 1L, grepl("Figure 1", captions$caption, fixed = TRUE))

# Markdown export does not require Pandoc or an external document converter.
out_file <- tempfile(fileext = ".md")
paths <- suppressMessages(write_taxRmatic_output("Test text.", out_file))
stopifnot(file.exists(paths$md), identical(readLines(paths$md), "Test text."))
unlink(paths$md)
