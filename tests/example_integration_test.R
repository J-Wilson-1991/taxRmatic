# Run the single complete example from the INSTALLED package, not load_all().
# This protects the tutorial against changes in function arguments and data.
library(taxRmatic)
example <- system.file("examples", "01_basic_workflow.R",
                       package = "taxRmatic", mustWork = TRUE)
old_wd <- getwd()
tmp <- tempfile("taxRmatic-example-")
dir.create(tmp)
tryCatch({
  setwd(tmp)
  env <- new.env(parent = globalenv())
  source(example, local = env)
  outfile <- file.path(tmp, "taxRmatic_basic_example.md")
  stopifnot(file.exists(outfile), file.info(outfile)$size > 100L)
  txt <- paste(readLines(outfile, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  stopifnot(all(vapply(env$all_taxa, function(taxon) grepl(taxon, txt, fixed = TRUE),
                       logical(1))),
            !grepl("fg\\{", txt),
            grepl("Material examined", txt, fixed = TRUE),
            grepl("Diagnosis", txt, fixed = TRUE),
            grepl("Description", txt, fixed = TRUE))
}, finally = {
  setwd(old_wd)
  unlink(tmp, recursive = TRUE)
})
