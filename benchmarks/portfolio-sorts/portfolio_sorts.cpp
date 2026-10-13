// Rcpp kernels for the portfolio-sort benchmark.
//
// The kernels reproduce the arithmetic of the R code they replace bit for bit:
// type-7 quantiles as in stats::quantile(), findInterval(all.inside = TRUE),
// sum() and mean() with long double accumulation as in R's summary.c, and
// weighted.mean() as sum((x * w)[w != 0]) / sum(w). Rows are processed in
// their original order within each group, which is the order dplyr uses.

#include <Rcpp.h>

#include <algorithm>
#include <cfloat>
#include <cmath>
#include <vector>

using namespace Rcpp;

typedef long double LDOUBLE;

// Grouping -----------------------------------------------------------------

// Row indices bucketed by a 1-based group id. Group k occupies
// rows[start[k - 1]] to rows[start[k] - 1], in original row order. Rows with
// an NA group id are skipped.
struct Buckets {
  std::vector<R_xlen_t> start;
  std::vector<R_xlen_t> rows;
};

static Buckets bucket_rows(const IntegerVector& group, int n_groups) {
  Buckets b;
  b.start.assign(n_groups + 1, 0);
  const R_xlen_t n = group.size();
  for (R_xlen_t i = 0; i < n; ++i) {
    if (group[i] != NA_INTEGER) b.start[group[i]]++;
  }
  for (int k = 1; k <= n_groups; ++k) b.start[k] += b.start[k - 1];
  b.rows.resize(b.start[n_groups]);
  std::vector<R_xlen_t> cursor(b.start.begin(), b.start.end() - 1);
  for (R_xlen_t i = 0; i < n; ++i) {
    if (group[i] != NA_INTEGER) b.rows[cursor[group[i] - 1]++] = i;
  }
  return b;
}

// R arithmetic -------------------------------------------------------------

// stats::quantile(x, p, type = 7) for an already sorted vector without NAs.
static double quantile7(const std::vector<double>& sorted, double p) {
  const R_xlen_t n = sorted.size();
  if (n == 0) return NA_REAL;
  const double index = 1 + (double)(n - 1) * p;
  const double lo = std::floor(index);
  const double hi = std::ceil(index);
  double qs = sorted[(R_xlen_t)lo - 1];
  const double x_hi = sorted[(R_xlen_t)hi - 1];
  if (index > lo && x_hi != qs) {
    const double h = index - lo;
    qs = (1 - h) * qs + h * x_hi;
  }
  return qs;
}

static void quantiles7(const std::vector<double>& sorted,
                       const NumericVector& probs, std::vector<double>& out) {
  out.resize(probs.size());
  for (R_xlen_t k = 0; k < probs.size(); ++k) {
    out[k] = quantile7(sorted, probs[k]);
  }
}

// Conversion of a long double sum to double as in R's rsum().
static double ld_to_double(LDOUBLE s) {
  if (s > DBL_MAX) return R_PosInf;
  if (s < -DBL_MAX) return R_NegInf;
  return (double)s;
}

// mean() of a double vector, following real_mean() in R's summary.c.
static double r_mean(const std::vector<double>& x) {
  const R_xlen_t n = x.size();
  LDOUBLE s = 0.0;
  for (R_xlen_t k = 0; k < n; ++k) s += x[k];
  bool finite_s = R_FINITE((double)s);
  if (finite_s) {
    s /= n;
  } else {
    s = 0.;
    for (R_xlen_t k = 0; k < n; ++k) s += x[k] / n;
  }
  if (finite_s && R_FINITE((double)s)) {
    LDOUBLE t = 0.0;
    for (R_xlen_t k = 0; k < n; ++k) t += (x[k] - s);
    s += t / n;
  } else if (R_FINITE((double)s)) {
    LDOUBLE t = 0.0;
    for (R_xlen_t k = 0; k < n; ++k) t += (x[k] - s) / n;
    s += t;
  }
  return (double)s;
}

// Return statistics of one portfolio-date cell, with the same NA rules as
// summarise_portfolio_returns().
struct CellReturns {
  int n;
  double vw, ew, vw_capped;
};

static double weighted_mean(const NumericVector& x, const NumericVector& w,
                            const R_xlen_t* rows, R_xlen_t n) {
  LDOUBLE num = 0.0, den = 0.0;
  for (R_xlen_t k = 0; k < n; ++k) {
    const R_xlen_t i = rows[k];
    if (w[i] != 0) {
      const double xw = x[i] * w[i];
      num += xw;
    }
    den += w[i];
  }
  const double sum_w = ld_to_double(den);
  if (sum_w == 0) return NA_REAL;
  return ld_to_double(num) / sum_w;
}

static CellReturns cell_returns(const NumericVector& ret,
                                const NumericVector& w,
                                const NumericVector& w_capped,
                                const R_xlen_t* rows, R_xlen_t n,
                                double min_size, std::vector<double>& buf) {
  CellReturns out;
  out.n = (int)n;
  if (n < min_size) {
    out.vw = out.ew = out.vw_capped = NA_REAL;
    return out;
  }
  buf.resize(n);
  for (R_xlen_t k = 0; k < n; ++k) buf[k] = ret[rows[k]];
  out.ew = r_mean(buf);
  out.vw = weighted_mean(ret, w, rows, n);
  out.vw_capped = weighted_mean(ret, w_capped, rows, n);
  return out;
}

// Capped weights -------------------------------------------------------------

// pmin(w, quantile(w, cap_weight, na.rm = TRUE)) per date, with missing
// market caps set to zero afterwards.
// [[Rcpp::export]]
NumericVector cap_weights_cpp(NumericVector w, IntegerVector group,
                              int n_groups, double cap_weight) {
  const Buckets b = bucket_rows(group, n_groups);
  NumericVector out(w.size(), NA_REAL);
  std::vector<double> values;
  for (int g = 0; g < n_groups; ++g) {
    values.clear();
    for (R_xlen_t k = b.start[g]; k < b.start[g + 1]; ++k) {
      const double v = w[b.rows[k]];
      if (!ISNAN(v)) values.push_back(v);
    }
    std::sort(values.begin(), values.end());
    const double cap = quantile7(values, cap_weight);
    for (R_xlen_t k = b.start[g]; k < b.start[g + 1]; ++k) {
      const R_xlen_t i = b.rows[k];
      if (ISNAN(w[i])) {
        out[i] = 0;
      } else if (ISNAN(cap)) {
        out[i] = NA_REAL;
      } else {
        out[i] = std::min(w[i], cap);
      }
    }
  }
  return out;
}

// Portfolio assignment -------------------------------------------------------

// Equivalent of assign_portfolio() with compute_breakpoints() for every group.
// `exchange_keep` (rows eligible for breakpoints by exchange) and `mktcap`
// (size threshold) are empty when the respective option is not used.
// [[Rcpp::export]]
List assign_portfolios_cpp(NumericVector x, IntegerVector group, int n_groups,
                           LogicalVector exchange_keep, NumericVector mktcap,
                           double size_threshold, NumericVector probs,
                           bool smooth_bunching, bool use_percentiles,
                           NumericVector probs_both_edges,
                           NumericVector probs_one_edge) {
  const Buckets b = bucket_rows(group, n_groups);
  const bool by_exchange = exchange_keep.size() > 0;
  const bool by_size = mktcap.size() > 0;
  const int n_portfolios = probs.size() - 1;

  IntegerVector portfolio(x.size(), NA_INTEGER);
  int n_constant = 0, n_empty = 0, n_missing = 0, n_clusters = 0,
      n_smooth_percentiles = 0;

  std::vector<double> values, sorted, bp, bp_new, mc;
  std::vector<char> used;

  for (int g = 0; g < n_groups; ++g) {
    const R_xlen_t from = b.start[g], to = b.start[g + 1];
    if (from == to) continue;

    // Constant sorting variable: a single portfolio.
    bool has_value = false, constant = true;
    double first = 0;
    for (R_xlen_t k = from; k < to; ++k) {
      const double v = x[b.rows[k]];
      if (ISNAN(v)) continue;
      if (!has_value) {
        first = v;
        has_value = true;
      } else if (v != first) {
        constant = false;
        break;
      }
    }
    if (constant) {
      n_constant++;
      for (R_xlen_t k = from; k < to; ++k) portfolio[b.rows[k]] = 1;
      continue;
    }

    // Size threshold among the (exchange-filtered) reference stocks.
    double size_cutoff = NA_REAL;
    if (by_size) {
      mc.clear();
      for (R_xlen_t k = from; k < to; ++k) {
        const R_xlen_t i = b.rows[k];
        if ((!by_exchange || exchange_keep[i] == TRUE) && !ISNAN(mktcap[i])) {
          mc.push_back(mktcap[i]);
        }
      }
      std::sort(mc.begin(), mc.end());
      size_cutoff = quantile7(mc, size_threshold);
    }

    // Values used for the breakpoints (NAs are kept for the length check).
    // As in R, an NA size cutoff turns the subset index into NA, which
    // contributes an NA value.
    values.clear();
    for (R_xlen_t k = from; k < to; ++k) {
      const R_xlen_t i = b.rows[k];
      if (by_exchange && exchange_keep[i] != TRUE) continue;
      if (by_size) {
        if (ISNAN(mktcap[i])) continue;
        if (ISNAN(size_cutoff)) {
          values.push_back(NA_REAL);
          continue;
        }
        if (!(mktcap[i] > size_cutoff)) continue;
      }
      values.push_back(x[i]);
    }
    if (values.empty()) {
      n_empty++;
      n_missing++;
      continue;
    }

    sorted.clear();
    for (double v : values) {
      if (!ISNAN(v)) sorted.push_back(v);
    }
    std::sort(sorted.begin(), sorted.end());
    quantiles7(sorted, probs, bp);

    if (smooth_bunching) {
      const int n = n_portfolios;
      const bool lower_edge = bp[0] == bp[1];
      const bool upper_edge = bp[n - 1] == bp[n];
      if (lower_edge || upper_edge) {
        if (use_percentiles) n_smooth_percentiles++;
        const double lower = bp[0], upper = bp[n];
        const double upper_inner = bp[n - 1];
        std::vector<double> inner;
        for (double v : sorted) {
          if (lower_edge && upper_edge) {
            if (v > lower && v < upper) inner.push_back(v);
          } else if (lower_edge) {
            if (v > lower) inner.push_back(v);
          } else if (v < upper_inner) {
            inner.push_back(v);
          }
        }
        if (lower_edge && upper_edge) {
          quantiles7(inner, probs_both_edges, bp_new);
          bp_new[n - 2] = bp_new[n - 2] + 1e-15;
          bp.assign(1, lower);
          bp.insert(bp.end(), bp_new.begin(), bp_new.end());
          bp.push_back(upper);
        } else if (lower_edge) {
          quantiles7(inner, probs_one_edge, bp_new);
          bp.assign(1, lower);
          bp.insert(bp.end(), bp_new.begin(), bp_new.end());
        } else {
          quantiles7(inner, probs_one_edge, bp_new);
          bp_new[n - 1] = bp_new[n - 1] + 1e-15;
          bp = bp_new;
          bp.push_back(upper);
        }
      }
    }

    for (int k = 1; k <= n_portfolios; ++k) bp[k] = bp[k] + 1e-20;

    bool any_na = false;
    for (double v : bp) any_na = any_na || ISNAN(v);
    if (any_na) {
      n_missing++;
      continue;
    }
    for (size_t k = 1; k < bp.size(); ++k) {
      if (bp[k] < bp[k - 1]) {
        stop("'vec' must be sorted non-decreasingly and not contain NAs");
      }
    }

    // findInterval(x, bp, all.inside = TRUE)
    const int n_intervals = bp.size() - 1;
    used.assign(n_intervals + 1, 0);
    int n_used = 0;
    for (R_xlen_t k = from; k < to; ++k) {
      const R_xlen_t i = b.rows[k];
      if (ISNAN(x[i])) continue;
      int p = std::upper_bound(bp.begin(), bp.end(), x[i]) - bp.begin();
      p = std::max(1, std::min(p, n_intervals));
      portfolio[i] = p;
      if (!used[p]) {
        used[p] = 1;
        n_used++;
      }
    }
    if (n_used != n_intervals) n_clusters++;
  }

  return List::create(
      _["portfolio"] = portfolio,
      _["warnings"] = IntegerVector::create(
          _["constant"] = n_constant, _["empty"] = n_empty,
          _["missing"] = n_missing, _["clusters"] = n_clusters,
          _["smooth_percentiles"] = n_smooth_percentiles));
}

// Annual rebalancing -----------------------------------------------------------

// For each row, the index of the rebalancing row of the same firm with the
// latest date on or before the row's date, provided the row's date does not
// exceed that rebalancing row's upper bound. Equivalent to the inequality join
// in join_rebalanced_portfolios(). Ids are integer codes in 1..n_ids.
// [[Rcpp::export]]
IntegerVector carry_forward_cpp(IntegerVector id, NumericVector date,
                                IntegerVector rebal_id,
                                NumericVector rebal_date,
                                NumericVector rebal_upper, int n_ids) {
  const Buckets b = bucket_rows(rebal_id, n_ids);
  // Sort each firm's rebalancing rows by date.
  std::vector<R_xlen_t> rows = b.rows;
  for (int g = 0; g < n_ids; ++g) {
    std::sort(rows.begin() + b.start[g], rows.begin() + b.start[g + 1],
              [&](R_xlen_t a, R_xlen_t c) {
                return rebal_date[a] < rebal_date[c];
              });
  }
  IntegerVector out(id.size(), NA_INTEGER);
  for (R_xlen_t i = 0; i < id.size(); ++i) {
    if (id[i] == NA_INTEGER || ISNAN(date[i])) continue;
    const R_xlen_t from = b.start[id[i] - 1], to = b.start[id[i]];
    // Latest rebalancing date <= date[i].
    R_xlen_t lo = from, hi = to;
    while (lo < hi) {
      const R_xlen_t mid = lo + (hi - lo) / 2;
      if (rebal_date[rows[mid]] <= date[i]) {
        lo = mid + 1;
      } else {
        hi = mid;
      }
    }
    for (R_xlen_t k = lo; k-- > from;) {
      const double upper = rebal_upper[rows[k]];
      if (ISNAN(upper)) continue;
      if (date[i] <= upper) out[i] = rows[k] + 1;
      break;
    }
  }
  return out;
}

// Aggregation -------------------------------------------------------------------

// Univariate sorts: returns per (portfolio, date) cell. Cells are laid out
// portfolio-major, i.e. cell (p, d) is at (p - 1) * n_dates + (d - 1).
// [[Rcpp::export]]
List aggregate_univariate_cpp(IntegerVector portfolio, IntegerVector date,
                              int n_portfolios, int n_dates,
                              NumericVector ret, NumericVector w,
                              NumericVector w_capped, double min_size) {
  const R_xlen_t n = portfolio.size();
  const int n_cells = n_portfolios * n_dates;
  IntegerVector cell(n, NA_INTEGER);
  for (R_xlen_t i = 0; i < n; ++i) {
    if (portfolio[i] == NA_INTEGER || date[i] == NA_INTEGER) continue;
    cell[i] = (portfolio[i] - 1) * n_dates + date[i];
  }
  const Buckets b = bucket_rows(cell, n_cells);

  IntegerVector n_obs(n_cells);
  NumericVector vw(n_cells, NA_REAL), ew(n_cells, NA_REAL),
      vw_capped(n_cells, NA_REAL);
  std::vector<double> buf;
  for (int c = 0; c < n_cells; ++c) {
    const R_xlen_t size = b.start[c + 1] - b.start[c];
    if (size == 0) continue;
    const CellReturns r = cell_returns(ret, w, w_capped, &b.rows[b.start[c]],
                                       size, min_size, buf);
    n_obs[c] = r.n;
    vw[c] = r.vw;
    ew[c] = r.ew;
    vw_capped[c] = r.vw_capped;
  }
  return List::create(_["n"] = n_obs, _["ret_excess_vw"] = vw,
                      _["ret_excess_ew"] = ew,
                      _["ret_excess_vw_capped"] = vw_capped);
}

// mean(x, na.rm = TRUE), with NaN (no values) mapped to NA as in
// aggregate_bivariate_returns().
static double mean_na_rm(const std::vector<double>& x,
                         std::vector<double>& buf) {
  buf.clear();
  for (double v : x) {
    if (!ISNAN(v)) buf.push_back(v);
  }
  const double m = r_mean(buf);
  return (ISNAN(m) && !R_IsNA(m)) ? NA_REAL : m;
}

// Bivariate sorts: cell returns over (main, secondary, date) without a size
// threshold, averaged over the secondary portfolios (NA secondary last), with
// min_size applied to the firm count per (main, date).
// [[Rcpp::export]]
List aggregate_bivariate_cpp(IntegerVector main, IntegerVector secondary,
                             IntegerVector date, int n_main, int n_secondary,
                             int n_dates, NumericVector ret, NumericVector w,
                             NumericVector w_capped, double min_size) {
  const R_xlen_t n = main.size();
  const int n_slots = n_secondary + 1;  // last slot: NA secondary portfolio
  const int n_cells = n_main * n_dates * n_slots;
  IntegerVector cell(n, NA_INTEGER);
  for (R_xlen_t i = 0; i < n; ++i) {
    if (main[i] == NA_INTEGER || date[i] == NA_INTEGER) continue;
    const int slot =
        secondary[i] == NA_INTEGER ? n_secondary : secondary[i] - 1;
    cell[i] = ((main[i] - 1) * n_dates + (date[i] - 1)) * n_slots + slot + 1;
  }
  const Buckets b = bucket_rows(cell, n_cells);

  const int n_out = n_main * n_dates;
  IntegerVector n_obs(n_out);
  NumericVector vw(n_out, NA_REAL), ew(n_out, NA_REAL),
      vw_capped(n_out, NA_REAL);
  std::vector<double> buf, cell_vw, cell_ew, cell_vw_capped, tmp;
  for (int o = 0; o < n_out; ++o) {
    cell_vw.clear();
    cell_ew.clear();
    cell_vw_capped.clear();
    int n_rows = 0, n_firms = 0;
    for (int s = 0; s < n_slots; ++s) {
      const int c = o * n_slots + s;
      const R_xlen_t size = b.start[c + 1] - b.start[c];
      if (size == 0) continue;
      const CellReturns r = cell_returns(ret, w, w_capped, &b.rows[b.start[c]],
                                         size, 0, buf);
      cell_vw.push_back(r.vw);
      cell_ew.push_back(r.ew);
      cell_vw_capped.push_back(r.vw_capped);
      n_rows += r.n;
      if (s < n_secondary) n_firms += r.n;
    }
    n_obs[o] = n_rows;
    if (n_rows == 0 || n_firms == 0 || n_firms < min_size) continue;
    vw[o] = mean_na_rm(cell_vw, tmp);
    ew[o] = mean_na_rm(cell_ew, tmp);
    vw_capped[o] = mean_na_rm(cell_vw_capped, tmp);
  }
  return List::create(_["n"] = n_obs, _["ret_excess_vw"] = vw,
                      _["ret_excess_ew"] = ew,
                      _["ret_excess_vw_capped"] = vw_capped);
}
