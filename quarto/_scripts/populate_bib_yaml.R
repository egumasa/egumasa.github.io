#!/usr/bin/env Rscript
# Pre-render script to populate YAML frontmatter from Zotero bib file
# Run before quarto render: Rscript _scripts/populate_bib_yaml.R

library(bibtex)

# Configuration
BIB_FILE <- "/Users/eguchi/Zotero/exports/references.bib"
PUBLICATIONS_DIR <- "publications"

# Read the global bib file
message("Reading bib file: ", BIB_FILE)
bib <- read.bib(BIB_FILE)

# Find all index.qmd files in publications
qmd_files <- list.files(
  PUBLICATIONS_DIR,
  pattern = "index\\.qmd$",
  recursive = TRUE,
  full.names = TRUE
)

message("Found ", length(qmd_files), " publication files")

# Function to format authors as string for listing
format_authors_string <- function(authors) {
  formatted <- sapply(authors, function(a) {
    family <- paste(a$family, collapse = " ")
    given <- paste(a$given, collapse = " ")
    initials <- paste0(substr(strsplit(given, " ")[[1]], 1, 1), ".", collapse = " ")
    paste0(family, ", ", initials)
  })
  paste(formatted, collapse = ", ")
}

# Function to clean string for YAML (remove LaTeX escapes)
clean_for_yaml <- function(str) {
  if (is.null(str)) return("")
  str <- gsub("\\\\&", "&", str)  # \& -> &
  str <- gsub("\\{|\\}", "", str)  # Remove braces
  str <- gsub('"', '\\"', str, fixed = TRUE)  # Escape quotes
  str <- gsub("\\\\", "", str)  # Remove remaining backslashes
  return(str)
}

# Function to parse month to number
month_to_num <- function(month) {
  if (is.null(month) || is.na(month)) return("01")
  month_map <- c(
    jan = "01", feb = "02", mar = "03", apr = "04",
    may = "05", jun = "06", jul = "07", aug = "08",
    sep = "09", oct = "10", nov = "11", dec = "12"
  )
  m <- tolower(substr(as.character(month), 1, 3))
  if (m %in% names(month_map)) {
    return(month_map[[m]])
  }
  # Try numeric
  if (grepl("^\\d+$", month)) {
    return(sprintf("%02d", as.integer(month)))
  }
  return("01")
}

# Process each qmd file
for (qmd_file in qmd_files) {
  message("\nProcessing: ", qmd_file)

  # Read the file
  lines <- readLines(qmd_file, warn = FALSE)
  content <- paste(lines, collapse = "\n")

  # Check if file has citekey param
  if (!grepl("citekey:", content)) {
    message("  Skipping - no citekey found")
    next
  }

  # Extract YAML frontmatter
  yaml_match <- regmatches(content, regexpr("^---[\\s\\S]*?\\n---", content, perl = TRUE))
  if (length(yaml_match) == 0) {
    message("  Skipping - no YAML frontmatter found")
    next
  }

  # Parse YAML
  yaml_content <- gsub("^---\\n|\\n---$", "", yaml_match)

  # Extract citekey using regex (avoid parsing R code in YAML)
  citekey_match <- regmatches(yaml_content, regexpr('citekey:\\s*"([^"]+)"', yaml_content, perl = TRUE))
  if (length(citekey_match) == 0) {
    message("  Skipping - could not extract citekey")
    next
  }
  citekey <- gsub('citekey:\\s*"|"', "", citekey_match)
  message("  Citekey: ", citekey)

  # Look up in bib
  if (!(citekey %in% names(bib))) {
    message("  WARNING: Citekey '", citekey, "' not found in bib file")
    next
  }

  entry <- bib[[citekey]]

  # Extract fields
  title <- entry$title
  journal <- entry$journal
  year <- entry$year
  month <- month_to_num(entry$month)
  doi <- entry$doi
  abstract <- entry$abstract
  authors <- entry$author

  # Format date
  date_str <- paste0(year, "-", month, "-01")

  # Format authors
  author_str <- format_authors_string(authors)

  # Build new YAML
  # Keep categories and other custom fields from original
  categories_match <- regmatches(yaml_content, regexpr("categories:\\s*\\n(\\s+-[^\\n]+\\n?)+", yaml_content, perl = TRUE))
  categories_block <- if (length(categories_match) > 0) categories_match else "categories:\n  - \"\""

  # Extract nocite
  nocite_match <- regmatches(yaml_content, regexpr("nocite:\\s*\\|\\n\\s+@[^\\n]+", yaml_content, perl = TRUE))
  nocite_block <- if (length(nocite_match) > 0) nocite_match else paste0("nocite: |\n  @", citekey)

  # Build new YAML frontmatter
  new_yaml <- paste0(
    "---\n",
    "# Auto-populated from bib file - do not edit title/author/date/doi manually\n",
    "params:\n",
    "  citekey: \"", citekey, "\"\n",
    "  bibfile: \"", BIB_FILE, "\"\n",
    "title: \"", clean_for_yaml(title), "\"\n",
    "subtitle: \"", clean_for_yaml(journal), "\"\n",
    "date: \"", date_str, "\"\n",
    "doi: \"", if (!is.null(doi)) doi else "", "\"\n",
    "author: \"", author_str, "\"\n",
    nocite_block, "\n",
    categories_block, "\n",
    "---"
  )

  # Get content after YAML
  body_content <- sub("^---[\\s\\S]*?\\n---\\n?", "", content, perl = TRUE)

  # Check if abstract section exists, if not add it
  if (!grepl("## Abstract", body_content)) {
    abstract_block <- paste0(
      "\n## Abstract\n\n",
      if (!is.null(abstract)) gsub("\\\\&", "&", abstract) else "",
      "\n\n"
    )
    body_content <- paste0(abstract_block, body_content)
  }

  # Combine and write
  new_content <- paste0(new_yaml, "\n", body_content)

  writeLines(new_content, qmd_file)
  message("  Updated successfully")
}

message("\n✓ Done! Run 'quarto render' to build the site.")
