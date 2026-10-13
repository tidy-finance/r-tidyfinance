// Core of the portfolio-sort kernels, shared by the cpp11 and Rcpp bindings
// (portfolio_sorts_cpp11.cpp and portfolio_sorts_rcpp.cpp).
//
// The kernels reproduce the arithmetic of the R code they replace bit for bit:
// type-7 quantiles as in stats::quantile(), findInterval(all.inside = TRUE),
// sum() and mean() with long double accumulation as in R's summary.c, and
// weighted.mean() as sum((x * w)[w != 0]) / sum(w). Rows are processed in
// their original order within each group, which is the order dplyr uses.
//
// The functions work on plain pointers so that they do not depend on either
// binding. Outputs are written to caller-allocated arrays.

#ifndef PORTFOLIO_SORTS_CORE_H
#define PORTFOLIO_SORTS_CORE_H

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>

#include <algorithm>
#include <cfloat>
#include <cmath>
#include <stdexcept>
#include <vector>

namespace portfolio_sorts {

typedef long double LDOUBLE;

// Grouping -----------------------------------------------------------------

// Row indices bucketed by a 1-based group id. Group k occupies
// rows[start[k - 1]] to rows[start[k] - 1], in original row order. Rows with
// an NA group id are skipped.
struct Buckets {
  std::vector<R_xlen_t> start;
  std::vector<R_xlen_t> rows;
};

inline Buckets bucket_rows(const int* group, R_xlen_t n, int n_groups) {
  Buckets b;
  b.start.assign(n_groups + 1, 0);
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
inline double quantile7(const std::vector<double>& sorted, double p) {
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

inline void quantiles7(const std::vector<double>& sorted,
                       const std::vector<double>& probs,
                       std::vector<double>& out) {
  out.resize(probs.size());
  for (size_t k = 0; k < probs.size(); ++k) {
    out[k] = quantile7(sorted, probs[k]);
  }
}

// Conversion of a long double sum to double as in R's rsum().
inline double ld_to_double(LDOUBLE s) {
  if (s > DBL_MAX) return R_PosInf;
  if (s < -DBL_MAX) return R_NegInf;
  return (double)s;
}

// mean() of a double vector, following real_mean() in R's summary.c.
inline double r_mean(const std::vector<double>& x) {
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

inline double weighted_mean(const double* x, const double* w,
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

inline CellReturns cell_returns(const double* ret, const double* w,
                                const double* w_capped, const R_xlen_t* rows,
                                R_xlen_t n, double min_size,
                                std::vector<double>& buf) {
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
inline void cap_weights(const double* w, const int* group, R_xlen_t n,
                        int n_groups, double cap_weight, double* out) {
  const Buckets b = bucket_rows(group, n, n_groups);
  std::fill(out, out + n, NA_REAL);
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
}

// Portfolio assignment -------------------------------------------------------

// Breakpoint options of compute_breakpoints(). `exchange_keep` (rows eligible
// for breakpoints by exchange, as an R logical) and `mktcap` (size threshold)
// are null when the respective option is not used.
struct BreakpointSpec {
  const int* exchange_keep;
  const double* mktcap;
  double size_threshold;
  std::vector<double> probs;
  bool smooth_bunching;
  bool use_percentiles;
  std::vector<double> probs_both_edges;
  std::vector<double> probs_one_edge;
};

// Number of groups that triggered each warning of assign_portfolio() and
// compute_breakpoints().
struct AssignWarnings {
  int constant, empty, missing, clusters, smooth_percentiles;
};

// Equivalent of assign_portfolio() with compute_breakpoints() for every group.
inline AssignWarnings assign_portfolios(const double* x, const int* group,
                                        R_xlen_t n, int n_groups,
                                        const BreakpointSpec& spec,
                                        int* portfolio) {
  const Buckets b = bucket_rows(group, n, n_groups);
  const bool by_exchange = spec.exchange_keep != nullptr;
  const bool by_size = spec.mktcap != nullptr;
  const int n_portfolios = spec.probs.size() - 1;

  std::fill(portfolio, portfolio + n, NA_INTEGER);
  AssignWarnings warnings = {0, 0, 0, 0, 0};

  std::vector<double> values, sorted, bp, bp_new, mc, inner;
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
      warnings.constant++;
      for (R_xlen_t k = from; k < to; ++k) portfolio[b.rows[k]] = 1;
      continue;
    }

    // Size threshold among the (exchange-filtered) reference stocks.
    double size_cutoff = NA_REAL;
    if (by_size) {
      mc.clear();
      for (R_xlen_t k = from; k < to; ++k) {
        const R_xlen_t i = b.rows[k];
        if ((!by_exchange || spec.exchange_keep[i] == TRUE) &&
            !ISNAN(spec.mktcap[i])) {
          mc.push_back(spec.mktcap[i]);
        }
      }
      std::sort(mc.begin(), mc.end());
      size_cutoff = quantile7(mc, spec.size_threshold);
    }

    // Values used for the breakpoints (NAs are kept for the length check).
    // As in R, an NA size cutoff turns the subset index into NA, which
    // contributes an NA value.
    values.clear();
    for (R_xlen_t k = from; k < to; ++k) {
      const R_xlen_t i = b.rows[k];
      if (by_exchange && spec.exchange_keep[i] != TRUE) continue;
      if (by_size) {
        if (ISNAN(spec.mktcap[i])) continue;
        if (ISNAN(size_cutoff)) {
          values.push_back(NA_REAL);
          continue;
        }
        if (!(spec.mktcap[i] > size_cutoff)) continue;
      }
      values.push_back(x[i]);
    }
    if (values.empty()) {
      warnings.empty++;
      warnings.missing++;
      continue;
    }

    sorted.clear();
    for (double v : values) {
      if (!ISNAN(v)) sorted.push_back(v);
    }
    std::sort(sorted.begin(), sorted.end());
    quantiles7(sorted, spec.probs, bp);

    if (spec.smooth_bunching) {
      const int np = n_portfolios;
      const bool lower_edge = bp[0] == bp[1];
      const bool upper_edge = bp[np - 1] == bp[np];
      if (lower_edge || upper_edge) {
        if (spec.use_percentiles) warnings.smooth_percentiles++;
        const double lower = bp[0], upper = bp[np];
        const double upper_inner = bp[np - 1];
        inner.clear();
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
          quantiles7(inner, spec.probs_both_edges, bp_new);
          bp_new[np - 2] = bp_new[np - 2] + 1e-15;
          bp.assign(1, lower);
          bp.insert(bp.end(), bp_new.begin(), bp_new.end());
          bp.push_back(upper);
        } else if (lower_edge) {
          quantiles7(inner, spec.probs_one_edge, bp_new);
          bp.assign(1, lower);
          bp.insert(bp.end(), bp_new.begin(), bp_new.end());
        } else {
          quantiles7(inner, spec.probs_one_edge, bp_new);
          bp_new[np - 1] = bp_new[np - 1] + 1e-15;
          bp = bp_new;
          bp.push_back(upper);
        }
      }
    }

    for (int k = 1; k <= n_portfolios; ++k) bp[k] = bp[k] + 1e-20;

    bool any_na = false;
    for (double v : bp) any_na = any_na || ISNAN(v);
    if (any_na) {
      warnings.missing++;
      continue;
    }
    for (size_t k = 1; k < bp.size(); ++k) {
      if (bp[k] < bp[k - 1]) {
        throw std::runtime_error(
            "'vec' must be sorted non-decreasingly and not contain NAs");
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
    if (n_used != n_intervals) warnings.clusters++;
  }
  return warnings;
}

// Annual rebalancing -----------------------------------------------------------

// For each row, the 1-based index of the rebalancing row of the same firm with
// the latest date on or before the row's date, provided the row's date does
// not exceed that rebalancing row's upper bound. Equivalent to the inequality
// join in join_rebalanced_portfolios(). Ids are integer codes in 1..n_ids.
inline void carry_forward(const int* id, const double* date, R_xlen_t n,
                          const int* rebal_id, const double* rebal_date,
                          const double* rebal_upper, R_xlen_t n_rebal,
                          int n_ids, int* out) {
  const Buckets b = bucket_rows(rebal_id, n_rebal, n_ids);
  // Sort each firm's rebalancing rows by date.
  std::vector<R_xlen_t> rows = b.rows;
  for (int g = 0; g < n_ids; ++g) {
    std::sort(rows.begin() + b.start[g], rows.begin() + b.start[g + 1],
              [&](R_xlen_t a, R_xlen_t c) {
                return rebal_date[a] < rebal_date[c];
              });
  }
  std::fill(out, out + n, NA_INTEGER);
  for (R_xlen_t i = 0; i < n; ++i) {
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
}

// Aggregation -------------------------------------------------------------------

// Output arrays of the aggregation kernels, one entry per (portfolio, date)
// cell. Cells are laid out portfolio-major: cell (p, d) is at
// (p - 1) * n_dates + (d - 1). `n` is the number of rows in the cell.
struct CellOutput {
  int* n;
  double* vw;
  double* ew;
  double* vw_capped;
};

inline void init_cells(const CellOutput& out, int n_cells) {
  std::fill(out.n, out.n + n_cells, 0);
  std::fill(out.vw, out.vw + n_cells, NA_REAL);
  std::fill(out.ew, out.ew + n_cells, NA_REAL);
  std::fill(out.vw_capped, out.vw_capped + n_cells, NA_REAL);
}

// Univariate sorts: returns per (portfolio, date) cell.
inline void aggregate_univariate(const int* portfolio, const int* date,
                                 R_xlen_t n, int n_portfolios, int n_dates,
                                 const double* ret, const double* w,
                                 const double* w_capped, double min_size,
                                 const CellOutput& out) {
  const int n_cells = n_portfolios * n_dates;
  std::vector<int> cell(n, NA_INTEGER);
  for (R_xlen_t i = 0; i < n; ++i) {
    if (portfolio[i] == NA_INTEGER || date[i] == NA_INTEGER) continue;
    cell[i] = (portfolio[i] - 1) * n_dates + date[i];
  }
  const Buckets b = bucket_rows(cell.data(), n, n_cells);

  init_cells(out, n_cells);
  std::vector<double> buf;
  for (int c = 0; c < n_cells; ++c) {
    const R_xlen_t size = b.start[c + 1] - b.start[c];
    if (size == 0) continue;
    const CellReturns r = cell_returns(ret, w, w_capped, &b.rows[b.start[c]],
                                       size, min_size, buf);
    out.n[c] = r.n;
    out.vw[c] = r.vw;
    out.ew[c] = r.ew;
    out.vw_capped[c] = r.vw_capped;
  }
}

// mean(x, na.rm = TRUE), with NaN (no values) mapped to NA as in
// aggregate_bivariate_returns().
inline double mean_na_rm(const std::vector<double>& x,
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
inline void aggregate_bivariate(const int* main, const int* secondary,
                                const int* date, R_xlen_t n, int n_main,
                                int n_secondary, int n_dates,
                                const double* ret, const double* w,
                                const double* w_capped, double min_size,
                                const CellOutput& out) {
  const int n_slots = n_secondary + 1;  // last slot: NA secondary portfolio
  const int n_cells = n_main * n_dates * n_slots;
  std::vector<int> cell(n, NA_INTEGER);
  for (R_xlen_t i = 0; i < n; ++i) {
    if (main[i] == NA_INTEGER || date[i] == NA_INTEGER) continue;
    const int slot =
        secondary[i] == NA_INTEGER ? n_secondary : secondary[i] - 1;
    cell[i] = ((main[i] - 1) * n_dates + (date[i] - 1)) * n_slots + slot + 1;
  }
  const Buckets b = bucket_rows(cell.data(), n, n_cells);

  const int n_out = n_main * n_dates;
  init_cells(out, n_out);
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
    out.n[o] = n_rows;
    if (n_rows == 0 || n_firms == 0 || n_firms < min_size) continue;
    out.vw[o] = mean_na_rm(cell_vw, tmp);
    out.ew[o] = mean_na_rm(cell_ew, tmp);
    out.vw_capped[o] = mean_na_rm(cell_vw_capped, tmp);
  }
}

}  // namespace portfolio_sorts

#endif  // PORTFOLIO_SORTS_CORE_H
