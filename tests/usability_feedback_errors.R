# Regression checks for actionable error messages raised by beta testers.
# Executed by R CMD check as a base-R test script.
library(taxRmatic)

# 1. The compiler should name a missing section, not just report a bad class.
remarks <- data.frame(taxon = "Exemplomygale silvatica",
                      subsection = "", text = "Fictional example.",
                      stringsAsFactors = FALSE)
class(remarks) <- c("taxRmatic_section", "data.frame")
missing_section <- list(Remarks = remarks, "COI barcode sequence" = NULL)
msg <- tryCatch(compile_taxon_texts(missing_section),
                error = function(e) conditionMessage(e))
stopifnot(is.character(msg),
          grepl("COI barcode sequence", msg, fixed = TRUE),
          grepl("position 2", msg, fixed = TRUE),
          grepl("NULL", msg, fixed = TRUE))

# Valid inputs must continue to work unchanged.
ok <- compile_taxon_texts(list(Remarks = remarks))
stopifnot(is.character(ok), grepl("Fictional example", ok, fixed = TRUE))

# 2. A character marked for printing must have sentence/paragraph metadata;
# errors must identify the exact character column. Other columns may be blank.
file <- system.file("extdata", "basic", "character_state_matrix.csv",
                    package = "taxRmatic", mustWork = TRUE)
dat <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
print_row <- which(dat$row_type == "print")
selected <- setdiff(names(dat)[vapply(dat[print_row, , drop = FALSE],
                                     function(x) tolower(trimws(x)) == "y",
                                     logical(1))],
                    "row_type")
stopifnot(length(selected) > 0L)
character <- selected[[1L]]
tmp <- tempfile(fileext = ".csv")

# Missing sentence: error must identify the column and remind user about print.
dat[dat$row_type == "sentence", character] <- ""
write.csv(dat, tmp, row.names = FALSE, na = "")
msg <- tryCatch(suppressMessages(read_taxRmatic_character_state_matrix(tmp)),
                error = function(e) conditionMessage(e))
stopifnot(is.character(msg),
          grepl(character, msg, fixed = TRUE),
          grepl("sentence", msg, fixed = TRUE),
          grepl("print", msg, fixed = TRUE))

# Missing paragraph: the analogous message must name the same column.
dat <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
dat[dat$row_type == "paragraph", character] <- ""
write.csv(dat, tmp, row.names = FALSE, na = "")
msg <- tryCatch(suppressMessages(read_taxRmatic_character_state_matrix(tmp)),
                error = function(e) conditionMessage(e))
stopifnot(is.character(msg),
          grepl(character, msg, fixed = TRUE),
          grepl("paragraph", msg, fixed = TRUE))
unlink(tmp)
