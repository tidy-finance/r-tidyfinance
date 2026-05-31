# This script helps regenerate the hardcoded tibbles used in the helpers
# list_supported_datasets_ff() and list_supported_datasets_ff_legacy()
# inside R/list_supported_datasets.R. Run it when Kenneth French publishes
# new datasets; reconcile its output with the existing tribbles by hand.
#
# The registries are hand-maintained: each row carries a clean, human-readable
# `dataset_name` and the `file_url` of the source ZIP archive (relative to the
# Kenneth French data library base URL). This script scrapes the data library
# index page for the available `_CSV.zip` links and the bold description that
# precedes each one, and writes them to a CSV so a maintainer can diff against
# the existing registry and spot newly added or renamed archives. The
# descriptive names still need manual tidying into the conventions used in the
# registry (clean spacing, correct `type` codes, etc.).
#
# Requires: httr2, dplyr

library(dplyr)

base_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/"
index_url <- paste0(base_url, "data_library.html")

html <- httr2::request(index_url) |>
  httr2::req_perform() |>
  httr2::resp_body_string()

# Each downloadable archive appears as an <a href="..._CSV.zip"> link. Capture
# the href and the nearest preceding bold-text description as a rough name.
matches <- gregexpr(
  "(?s)<b>(.*?)</b>.*?href=\"([^\"]*?_CSV\\.zip)\"",
  html,
  perl = TRUE,
  ignore.case = TRUE
)
captured <- regmatches(html, matches)[[1]]

file_url <- sub(
  "(?s).*href=\"([^\"]*?_CSV\\.zip)\".*",
  "\\1",
  captured,
  perl = TRUE
)
raw_name <- sub("(?s)<b>(.*?)</b>.*", "\\1", captured, perl = TRUE)
raw_name <- gsub("<[^>]*>", "", raw_name)
# Collapse embedded newlines, tabs, and repeated spaces to single spaces. The
# bold descriptions on the data library page often span multiple lines.
raw_name <- trimws(gsub("\\s+", " ", raw_name))

data_sets_raw <- tibble(dataset_name = raw_name, file_url = file_url)

# Sanity check: the data library page lists several hundred archives. A much
# smaller count means the page layout changed and the scrape silently failed.
stopifnot(nrow(data_sets_raw) > 200)

# Write the scraped datasets to a CSV next to this script. Diff it against the
# `dataset_name` and `file_url` columns of the registry in
# R/list_supported_datasets.R to reconcile any additions or renames by hand.
out_file <- file.path("data-raw", "list_supported_datasets_ff_scraped.csv")
utils::write.csv(data_sets_raw, out_file, row.names = FALSE)
cli::cli_alert_success("Wrote {nrow(data_sets_raw)} datasets to {out_file}")
