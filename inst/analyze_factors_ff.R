devtools::load_all()

data_sets_raw <- frenchdata::get_french_data_list()$files_list

out <- list()

for (j in 1:nrow(data_sets_raw)) {
  data_set <- data_sets_raw$name[j]
  tryCatch({
    raw_data <- suppressMessages(frenchdata::download_french_data(data_set)$subsets$data[[1]])
    out[[j]] <- tibble(
      data_set = data_set,
      columns = colnames(raw_data)
    )
    cat(j, "done\n")
  }, error = function(e) {
    out[[j]] <- tibble(
      data_set = data_set,
      columns = NA
    )
    cat(j, "error\n")
  })
}

data_sets_columns <- bind_rows(out)

bind_rows(out) |>
  count(columns) |>
  arrange(-n) |>
  mutate(columns_cleaned = gsub("rf", "risk_free", gsub("-rf", "_excess", gsub(" ", "_", tolower(columns))))) |>
  View()

# For instance this does not work, seems that CSV has a differenent format
frenchdata::download_french_data("100 Portfolios Formed on Size and Operating Profitability (10 x 10) [ex. Dividends]")

# Here the columns are also messed up and include > and <=
frenchdata::download_french_data("BE/ME Breakpoints")$subsets$data[[1]]
