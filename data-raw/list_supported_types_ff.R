# This script generates the hardcoded tibble used in list_supported_types_ff().
# Run this script if the list of available Fama-French datasets needs to be
# updated. The output of datapasta::tribble_paste() should replace the
# tribble() call in R/list_supported_types.R.
#
# Requires: frenchdata, dplyr, datapasta

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
