# estimate_capm <- function(data, min_obs = 1) {
#   if (nrow(data) < min_obs) {
#     beta <- as.numeric(NA)
#   } else {
#     fit <- lm(ret_excess ~ mkt_excess, data = data)
#     beta <- as.numeric(coefficients(fit)[2])
#   }
#   return(beta)
# }
#
# roll_capm_estimation <- function(data, months, min_obs) {
#   data <- data |>
#     arrange(month)
#
#   betas <- slide_period_vec(
#     .x = data,
#     .i = data$month,
#     .period = "month",
#     .f = ~ estimate_capm(., min_obs),
#     .before = months - 1,
#     .complete = FALSE
#   )
#
#   return(tibble(
#     month = unique(data$month),
#     beta = betas
#   ))
# }
