# taxRmatic: complete introductory example
# -----------------------------------------------------------------------------
# PURPOSE: Generate a manuscript containing five fictional species treatments.
# The example is intended to be read from top to bottom while working through
# the vignette called "Preparing CSV data for taxRmatic".
#
# You need not know how to write R functions to follow this script. Lines
# beginning with # are explanatory comments: R ignores them.
# A name such as 'figures' or 'descriptions' is an R object: a temporary place
# to keep information for the next step. The arrow <- means "store the result
# under this name". An argument such as separate_categories = TRUE is a
# setting for a function; TRUE means yes, FALSE means no.
#
# BEFORE CHANGING ANYTHING:
#   1. Install taxRmatic using the README instructions.
#   2. Open this script in RStudio. Run it from the Console with
#      source(system.file("examples", "01_basic_workflow.R",
#                         package = "taxRmatic", mustWork = TRUE))
#   3. Find the resulting manuscript by running getwd() in the Console.
#   4. Follow the Preparing CSV data vignette to copy the script and CSVs into
#      your own folder. Edit those copies, not the installed package files.
#
# All species names, specimen IDs, places, collecting events, measurements,
# descriptions and diagnostic comparisons below are INVENTED. The figure
# spreadsheet contains references and metadata, not image files. This example
# does not constitute nomenclatural acts or real biodiversity records.

# 1. LOAD THE PACKAGE AND LOCATE THE EXAMPLE FILES ----------------------------
# library() makes the package's functions available to this R session.
library(taxRmatic)

# system.file() finds a file or folder inside the installed package. The
# original script uses the five CSVs that came with taxRmatic. After copying
# them to your own project folder, change ONLY the data_dir line to the path
# of your new data folder (see the Preparing CSV data vignette).
data_dir <- system.file("extdata", "basic", package = "taxRmatic", mustWork = TRUE)

# This little helper finds one file inside data_dir. You do not need to
# understand how it works to follow the rest of the example.
input <- function(filename) {
  p <- file.path(data_dir, filename)
  if (!file.exists(p)) stop("Example file not found: ", p)
  p
}

# 2. READ THE FIVE EXAMPLE SPREADSHEETS --------------------------------------
# The read_ functions import CSV files and check their structure. They do not
# generate manuscript paragraphs yet. The five R objects below hold the
# imported data so we can use them in the next steps.
#
# The character matrix contains observations plus instruction rows such as
# print, paragraph, sentence, prefix, suffix and figure_filter. Every character
# marked 'y' in the print row must have paragraph and sentence instructions.
# The figure table lists figure numbers and descriptions of what is pictured;
# it does NOT contain photographs. An overview map may have no taxon or
# specimen code, so the reader may report this as a note rather than an error.
characters <- read_taxRmatic_character_state_matrix(
  input("character_state_matrix.csv")
)
figures <- read_taxRmatic_figures(input("figures.csv"))
specimens <- read_taxRmatic_specimen_data(input("specimens.csv"))
sections <- read_taxRmatic_sections(input("sections.csv"))
diagnostics <- read_taxRmatic_diagnostics(input("diagnostics.csv"))

# 3. GENERATE DIAGNOSES -------------------------------------------------------
# The diagnostics spreadsheet already contains YOUR diagnostic comparisons.
# The generator chooses statements that distinguish each species from the
# other relevant species in its comparison group. It does NOT discover new
# diagnostic differences. Review the resulting prose critically.
# separate_categories = TRUE produces separate male/female paragraphs when
# appropriate. Their headings come from the diagnostics CSV.
diagnoses <- get_taxRmatic_diagnoses(
  diagnostics, figures = figures, separate_categories = TRUE
)

# 4. PREPARE SPECIMEN INFORMATION ---------------------------------------------
# Importing the specimen table did not require special formatting. Here we
# create some additional columns to match the conventions of our manuscript.
# The raw columns remain unchanged; these new columns are only for printing.
#
# format_custom_fields() joins text and column values for each specimen.
# prefix() and suffix() are included ONLY when the linked value is present.
# The example builds printable registration numbers from synthetic IDs.
specimens$registration <- format_custom_fields(
  specimens, list(prefix("(DEMO "), column("regno"), suffix(")"))
)

# Append the elevation unit (metres), but only where elevation is present.
specimens$`elevation (formatted)` <- format_custom_fields(
  specimens, list(column("elevation (m)"), suffix(" m"))
)

# Convert signed decimal degrees to degrees and minutes for the manuscript.
# You can omit this if your collection data already use your preferred format.
specimens$latitude_final <- format_coordinates(
  specimens$`latitude dd`, input_format = "dd_signed",
  output_format = "dms", coordinate_type = "latitude",
  precision = "min", output_style = "compact"
)
specimens$longitude_final <- format_coordinates(
  specimens$`longitude dd`, input_format = "dd_signed",
  output_format = "dms", coordinate_type = "longitude",
  precision = "min", output_style = "compact"
)
specimens$coordinates <- format_custom_fields(
  specimens,
  list(column("latitude_final"), column("longitude_final")),
  separator = ", "
)

# 5. GENERATE DESCRIPTIONS AND MATERIAL EXAMINED ------------------------------
# Descriptions are built from the character values and their writing
# instructions; the figure table supplies matching illustration references.
# Material examined draws on the imported specimen table and the printable
# columns created in Step 4. The order of names in fields_to_concat is the
# order in which the fields appear within each specimen record.
descriptions <- get_taxRmatic_descriptions(characters, figures)

material_examined <- get_taxRmatic_material_examined(
  taxRmatic_specimen_data = specimens,
  fields_to_concat = c("specimen counts", "exact location", "coordinates",
                       "elevation (formatted)", "microhabitat", "collection date",
                       "collectors", "collection method", "registration"),
  order_specimens_by = "latitude dd",
  # Enable "same data" only after deciding which collecting details may be
  # abbreviated. Adjust the comparison fields to suit your own records.
  compress_same_data = TRUE,
  same_data_compare_fields = c("exact location", "collection date", "collectors"),
  same_data_prefix_fields = "specimen counts",
  same_data_suffix_fields = "registration",
  same_data_text = "same data"
)

# 6. COMPILE COMPLETE TAXON TREATMENTS ---------------------------------------
# c() creates a set of values in R. We list the five taxa here explicitly so
# no additional comparison-only taxon gets its own species treatment.
all_taxa <- c(
  "Exemplomygale silvatica", "Exemplomygale spinosa",
  "Exemplomygale longipes", "Exemplomygale rotundata",
  "Exemplomygale angulata"
)

# list() brings the different generated sections together. The order shown
# below is the order they will appear in each taxon treatment. The names on
# the left (Diagnosis, Description, etc.) are the printed section headings.
# Each item is a taxRmatic_section object, apart from the figure table.
# To see which extra prose sections were imported, run names(sections).
# Do NOT include a missing section (for example sections$distribution if that
# section does not exist); simply leave that line out of the list.
# To rearrange or hide headings, see the Customising taxRmatic vignette.
compiled <- compile_taxon_texts(
  list(Figures = figures,
       "Material examined" = material_examined,
       Diagnosis = diagnoses,
       Description = descriptions,
       Etymology = sections$etymology,
       Remarks = sections$remarks),
  taxa = all_taxa,
  taxon_order = all_taxa
)

# 7. FINALISE FIGURE REFERENCES AND TAXON NAMES -------------------------------
# The text is compiled, but some labels need a final pass. This step replaces
# fg{...} figure placeholders and formats scientific names consistently.
# species_list contains names to recognise; new_species specifies the names
# that should receive "sp. nov.". These lists happen to be identical in the
# fictional example, but should differ if your revision includes known taxa.
final_text <- finalise_taxRmatic_text(
  compiled,
  replace_figure_tags = TRUE,
  taxRmatic_figures = figures,
  format_taxon_names_in_text = TRUE,
  species_list = all_taxa,
  new_species = all_taxa,
  new_species_suffix = "sp. nov."
)

# Verify that none of the figure-filter placeholders remain unresolved.
stopifnot(length(final_text) == 1L, nzchar(final_text))
if (grepl("fg\\{", final_text)) {
  warning("One or more figure tags were not replaced; check figures.csv.")
}

# 8. WRITE THE MANUSCRIPT -----------------------------------------------------
# A Markdown file is plain text using characters such as # and * to identify
# headings and italics. To find the output folder, run getwd() in RStudio.
# The last line prints the file location when the script finishes.
output_path <- file.path(getwd(), "taxRmatic_basic_example.md")
write_taxRmatic_output(final_text, file = output_path, output_format = "md")
message("Example manuscript written to: ", normalizePath(output_path))

# Optional: create a Word document from the same final text. Work in your
# own COPY of this script and remove the # at the start of the two lines below
# when you are ready to try it. This requires rmarkdown and Pandoc.
# write_taxRmatic_output(final_text,
#                       file = file.path(getwd(), "taxRmatic_basic_example.docx"),
#                       output_format = "word_document")
