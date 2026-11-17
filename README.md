# taxRmatic

**Generate taxonomic treatments from data prepared in spreadsheets.**

taxRmatic is an R package for assembling character-based descriptions, comparative diagnoses, material examined lists, figure references, etymologies, remarks and other sections into manuscript text. You make the taxonomic decisions and enter the data; taxRmatic takes care of much of the repetitive writing and formatting. You can regenerate a treatment after correcting a measurement, rearranging figures or changing the section layout.

**Status:** Development version 0.1.0. The main workflow is available for use and testing; documentation and features may continue to improve. Please check the generated text before using it in a manuscript.

## New to R? Start here

You do **not** need a GitHub account, Git knowledge or experience writing R functions to try taxRmatic. You will need **R** and **RStudio** installed on your computer, plus some familiarity with spreadsheets. R is the software that runs the commands; RStudio gives you a convenient place to write and run them.

If you have never used R before, install **R first** from [CRAN](https://cran.r-project.org/), then install the free **RStudio Desktop** from [Posit](https://posit.co/download/rstudio-desktop/). Open RStudio once both are installed. You should see a **Console** pane containing a `>` prompt. That is where you enter the commands in this README. You can learn the package by copying the examples; you do not need to learn R programming first.

The recommended learning order is:

1. **Install taxRmatic** using the instructions below.
2. **Run the included example** once without changing anything.
3. **Read the “Preparing CSV data” vignette** while opening the five example spreadsheets. It shows how to make your own copies, edit them and understand the rules used to generate text.
4. **Read the “Customising taxRmatic” vignette** and try changing one output setting at a time.
5. **Use the help pages** (`?function_name`) when you need the full options for an individual function.

The fictional example is the quickest way to learn. **Do not start by constructing your own large revision from scratch.**

## 1. Install from GitHub

Open **RStudio** and click in the **Console** (the pane showing the `>` prompt). Copy each command below, press **Enter**, and allow it to finish before running the next one. Do not type the `>` prompt itself.

**First time only**, install the packages needed to download taxRmatic and build its tutorials:

```r
install.packages(c("remotes", "knitr", "rmarkdown"))
```

Then install taxRmatic from GitHub:

```r
remotes::install_github("YOUR_USERNAME/taxRmatic", build_vignettes = TRUE)
```

**Replace `YOUR_USERNAME`** with the account name in the actual GitHub address for taxRmatic. For example, a repository at `https://github.com/example-user/taxRmatic` would use `"example-user/taxRmatic"`. This example is not the project's actual address.

When installation is finished, load the package:

```r
library(taxRmatic)
```

If that runs without an error, taxRmatic is available in your current R session. Run `library(taxRmatic)` again whenever you begin a new R session.

**Updating later:** Run the same `remotes::install_github(...)` command again to install the latest GitHub version. Installing a new version does not automatically change CSV files in your own project folder.

**If installation fails:** Check that the GitHub account name is correct and that your computer has internet access. If R reports a missing package, install the named package with `install.packages("package_name")`, then retry. On some systems, building a package from source requires additional development tools. Share the *complete error message* if you need help; avoid changing your data files in response to an installation error.

## 2. Run the complete example

The package includes a ready-to-run workflow and five CSV spreadsheets describing five **entirely fictional** species of *Exemplomygale*. The specimens, localities, measurements and taxonomic statements are invented. Figure records contain metadata, not photographs. The example is a demonstration, not a source of taxonomic data or nomenclatural acts.

After loading the package, run the following in the RStudio Console:

```r
example_script <- system.file(
  "examples", "01_basic_workflow.R",
  package = "taxRmatic", mustWork = TRUE
)
file.edit(example_script)
```

The example script opens in RStudio. Its lines beginning with `#` are explanations; R does not execute those lines. Read its numbered steps. Then run the **whole example**:

```r
source(example_script, echo = TRUE)
```

The example should write a file called **`taxRmatic_basic_example.md`**. This is a Markdown manuscript: it contains text and simple symbols for headings and italics, which can be converted to a Word document. To find the folder where the file was saved, run:

```r
getwd()
```

The example also contains an optional, commented-out command for Word output. You can try that after the basic Markdown example works. Word output requires `rmarkdown` and Pandoc (normally included with RStudio).

**Keep the installed example unchanged.** When you are ready to experiment, the first vignette explains how to copy the script and data into your own folder. That way, you always have the original working example to return to.

## 3. Read the step-by-step guides

Run either of these commands in the Console to open a guide:

```r
vignette("preparing-csv-data", package = "taxRmatic")
vignette("customising-taxrmatic", package = "taxRmatic")
```

Or open the guide index:

```r
browseVignettes("taxRmatic")
```

**Preparing CSV data** explains the five spreadsheets, how character values and instructions become descriptions, how iterative diagnoses select comparisons, how figures are matched, and how to prepare your own editable copies.

**Customising taxRmatic** shows what to change when you want different paragraph headings, diagnostic comparisons, figure references, specimen strings, treatment order or output formats. Its code examples are changes to make to your copy of the main workflow—not a second complete script.

If `browseVignettes()` reports no vignettes, reinstall using `build_vignettes = TRUE` as shown above. If your computer cannot build vignettes, the `.Rmd` source files can also be read in the repository's `vignettes/` folder.

## 4. Look up a particular function

Every main function has a help page. In the Console, use `?` followed by its name:

```r
?read_taxRmatic_character_state_matrix
?read_taxRmatic_diagnostics
?get_taxRmatic_diagnoses
?compile_taxon_texts
```

You can see all documented topics with:

```r
help(package = "taxRmatic")
```

A few R terms used in the guides: a **CSV** is a plain-text table saved from a spreadsheet; an **R script** is a series of commands saved in a `.R` file; an **object** is data held in R, such as `figures` or `diagnoses`; an **argument** is a setting supplied inside a function call, for example `separate_categories = TRUE`. `TRUE` means yes/on, and `FALSE` means no/off.

## How taxRmatic works

The usual workflow is **read the CSVs → generate manuscript sections → arrange the treatments → finalise names and figure references → export**. The five input tables serve different purposes:

| Spreadsheet | Used for |
|:--|:--|
| `character_state_matrix.csv` | Observations and the instructions for writing descriptions |
| `diagnostics.csv` | Diagnostic statements and the taxa they distinguish |
| `figures.csv` | Metadata used to identify and cite figures or figure parts |
| `specimens.csv` | Specimen records used for material examined |
| `sections.csv` | Already-written paragraphs such as etymology and remarks |

You can use your own morphological characters, group-specific terminology and specimen fields. The examples illustrate one possible taxonomic treatment, not a required style.

**If a generated treatment is wrong, correct the source data or settings and run the script again.** Manual edits to the generated document will not be retained the next time you regenerate it.

## Feedback

If you encounter a problem, please report what you were trying to do, the command you used, the error message, and (where possible) a small fictional or non-sensitive example. GitHub Issues in the taxRmatic repository are a suitable place for reproducible bug reports and documentation suggestions.
