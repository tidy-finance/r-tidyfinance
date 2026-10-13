// Rcpp bindings for the portfolio-sort kernels in portfolio_sorts_core.h.
//
// Kept for comparison with the cpp11 bindings: both call the same core, so any
// difference in the benchmark is binding overhead.

#include <Rcpp.h>

#include "portfolio_sorts_core.h"

using namespace Rcpp;

namespace {

List cell_list(const IntegerVector& n, const NumericVector& vw,
               const NumericVector& ew, const NumericVector& vw_capped) {
  return List::create(_["n"] = n, _["ret_excess_vw"] = vw,
                      _["ret_excess_ew"] = ew,
                      _["ret_excess_vw_capped"] = vw_capped);
}

}  // namespace

// [[Rcpp::export]]
NumericVector cap_weights_rcpp(NumericVector w, IntegerVector group,
                               int n_groups, double cap_weight) {
  NumericVector out(w.size());
  portfolio_sorts::cap_weights(w.begin(), group.begin(), w.size(), n_groups,
                               cap_weight, out.begin());
  return out;
}

// [[Rcpp::export]]
List assign_portfolios_rcpp(NumericVector x, IntegerVector group, int n_groups,
                            LogicalVector exchange_keep, NumericVector mktcap,
                            double size_threshold, NumericVector probs,
                            bool smooth_bunching, bool use_percentiles,
                            NumericVector probs_both_edges,
                            NumericVector probs_one_edge) {
  portfolio_sorts::BreakpointSpec spec;
  spec.exchange_keep =
      exchange_keep.size() > 0 ? exchange_keep.begin() : nullptr;
  spec.mktcap = mktcap.size() > 0 ? mktcap.begin() : nullptr;
  spec.size_threshold = size_threshold;
  spec.probs.assign(probs.begin(), probs.end());
  spec.smooth_bunching = smooth_bunching;
  spec.use_percentiles = use_percentiles;
  spec.probs_both_edges.assign(probs_both_edges.begin(),
                               probs_both_edges.end());
  spec.probs_one_edge.assign(probs_one_edge.begin(), probs_one_edge.end());

  IntegerVector portfolio(x.size());
  const portfolio_sorts::AssignWarnings w = portfolio_sorts::assign_portfolios(
      x.begin(), group.begin(), x.size(), n_groups, spec, portfolio.begin());

  return List::create(
      _["portfolio"] = portfolio,
      _["warnings"] = IntegerVector::create(
          _["constant"] = w.constant, _["empty"] = w.empty,
          _["missing"] = w.missing, _["clusters"] = w.clusters,
          _["smooth_percentiles"] = w.smooth_percentiles));
}

// [[Rcpp::export]]
IntegerVector carry_forward_rcpp(IntegerVector id, NumericVector date,
                                 IntegerVector rebal_id,
                                 NumericVector rebal_date,
                                 NumericVector rebal_upper, int n_ids) {
  IntegerVector out(id.size());
  portfolio_sorts::carry_forward(id.begin(), date.begin(), id.size(),
                                 rebal_id.begin(), rebal_date.begin(),
                                 rebal_upper.begin(), rebal_id.size(), n_ids,
                                 out.begin());
  return out;
}

// [[Rcpp::export]]
List aggregate_univariate_rcpp(IntegerVector portfolio, IntegerVector date,
                               int n_portfolios, int n_dates,
                               NumericVector ret, NumericVector w,
                               NumericVector w_capped, double min_size) {
  const R_xlen_t n_cells = (R_xlen_t)n_portfolios * n_dates;
  IntegerVector n(n_cells);
  NumericVector vw(n_cells), ew(n_cells), vw_capped(n_cells);
  const portfolio_sorts::CellOutput out = {n.begin(), vw.begin(), ew.begin(),
                                           vw_capped.begin()};
  portfolio_sorts::aggregate_univariate(
      portfolio.begin(), date.begin(), portfolio.size(), n_portfolios,
      n_dates, ret.begin(), w.begin(), w_capped.begin(), min_size, out);
  return cell_list(n, vw, ew, vw_capped);
}

// [[Rcpp::export]]
List aggregate_bivariate_rcpp(IntegerVector main, IntegerVector secondary,
                              IntegerVector date, int n_main, int n_secondary,
                              int n_dates, NumericVector ret, NumericVector w,
                              NumericVector w_capped, double min_size) {
  const R_xlen_t n_cells = (R_xlen_t)n_main * n_dates;
  IntegerVector n(n_cells);
  NumericVector vw(n_cells), ew(n_cells), vw_capped(n_cells);
  const portfolio_sorts::CellOutput out = {n.begin(), vw.begin(), ew.begin(),
                                           vw_capped.begin()};
  portfolio_sorts::aggregate_bivariate(
      main.begin(), secondary.begin(), date.begin(), main.size(), n_main,
      n_secondary, n_dates, ret.begin(), w.begin(), w_capped.begin(), min_size,
      out);
  return cell_list(n, vw, ew, vw_capped);
}
