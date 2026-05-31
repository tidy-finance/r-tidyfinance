# Set global variables ------------------------------------------------
# Column references use the `.data[["x"]]` / `"x"` / `by = "x"` idioms,
# and inequality `join_by()` predicates declare their columns as `NULL`
# locals inside the respective functions. Only two names remain here:
#   * `closest`, the dplyr `join_by()` helper used in inequality joins,
#     which is recognised by `join_by()` but is not an ordinary binding; and
#   * `url`, referenced in `download_data_factors_q()`.
utils::globalVariables(
  c(
    "closest",
    "url"
  )
)
