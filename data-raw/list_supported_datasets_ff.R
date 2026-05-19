# nolint start: commented_code_linter.
# This script generates the hardcoded tibble used in the helper
# list_supported_datasets_ff inside R/list_supported_datasets.R. Run it
# when the list of available Fama-French datasets needs updating; the
# output of datapasta::tribble_paste should replace the existing
# tribble call in that helper.
#
# Requires: frenchdata, dplyr, datapasta
# nolint end

library(dplyr)

data_sets_raw <- frenchdata::get_french_data_list()$files_list

data_sets_types <- data_sets_raw |>
  mutate(
    type = gsub("Fama/French ", "", name),
    type = gsub(" Factors", "", type),
    type = gsub("Portfolios Formed on ", "", type),
    type = gsub(" \\(Mom\\)", "", type),
    type = gsub("[[:punct:]]", "", type),
    type = gsub(" ", "_", type),
    type = tolower(type),
    type = if_else(
      !grepl("weekly|daily", type),
      paste0(type, "_monthly"),
      type),
    type = gsub("booktomarket", "bm", type),
    type = gsub("operating_profitability", "op", type),
    type = gsub("ex_dividend", "exdividend", type),
    type = gsub("investment", "inv", type),
    type = gsub("\t", "_", type),
    type = paste0("factors_ff_", type)
  ) |>
  select(type, dataset_name = name)

datapasta::dp_set_max_rows(300)
datapasta::tribble_paste(data_sets_types)
