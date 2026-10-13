// cpp11 bindings for the portfolio-sort kernels in portfolio_sorts_core.h.
//
// This is the binding proposed for the package: cpp11 is header-only, so it
// is only needed at build time (`LinkingTo: cpp11`) and adds no runtime
// dependency.

#include "cpp11.hpp"
#include "portfolio_sorts_core.h"

using namespace cpp11::literals;
namespace writable = cpp11::writable;

namespace {

std::vector<double> to_std(const cpp11::doubles& x) {
  return std::vector<double>(x.begin(), x.end());
}

writable::list cell_list(const writable::integers& n,
                         const writable::doubles& vw,
                         const writable::doubles& ew,
                         const writable::doubles& vw_capped) {
  return writable::list({"n"_nm = n, "ret_excess_vw"_nm = vw,
                         "ret_excess_ew"_nm = ew,
                         "ret_excess_vw_capped"_nm = vw_capped});
}

}  // namespace

[[cpp11::register]]
cpp11::doubles cap_weights_cpp11(cpp11::doubles w, cpp11::integers group,
                                 int n_groups, double cap_weight) {
  writable::doubles out(w.size());
  portfolio_sorts::cap_weights(REAL_RO(w), INTEGER_RO(group), w.size(),
                               n_groups, cap_weight, REAL(out));
  return out;
}

[[cpp11::register]]
cpp11::list assign_portfolios_cpp11(
    cpp11::doubles x, cpp11::integers group, int n_groups,
    cpp11::logicals exchange_keep, cpp11::doubles mktcap,
    double size_threshold, cpp11::doubles probs, bool smooth_bunching,
    bool use_percentiles, cpp11::doubles probs_both_edges,
    cpp11::doubles probs_one_edge) {
  portfolio_sorts::BreakpointSpec spec;
  spec.exchange_keep =
      exchange_keep.size() > 0 ? LOGICAL_RO(exchange_keep) : nullptr;
  spec.mktcap = mktcap.size() > 0 ? REAL_RO(mktcap) : nullptr;
  spec.size_threshold = size_threshold;
  spec.probs = to_std(probs);
  spec.smooth_bunching = smooth_bunching;
  spec.use_percentiles = use_percentiles;
  spec.probs_both_edges = to_std(probs_both_edges);
  spec.probs_one_edge = to_std(probs_one_edge);

  writable::integers portfolio(x.size());
  const portfolio_sorts::AssignWarnings w = portfolio_sorts::assign_portfolios(
      REAL_RO(x), INTEGER_RO(group), x.size(), n_groups, spec,
      INTEGER(portfolio));

  writable::integers warnings(
      {"constant"_nm = w.constant, "empty"_nm = w.empty,
       "missing"_nm = w.missing, "clusters"_nm = w.clusters,
       "smooth_percentiles"_nm = w.smooth_percentiles});
  return writable::list({"portfolio"_nm = portfolio,
                         "warnings"_nm = warnings});
}

[[cpp11::register]]
cpp11::integers carry_forward_cpp11(cpp11::integers id, cpp11::doubles date,
                                    cpp11::integers rebal_id,
                                    cpp11::doubles rebal_date,
                                    cpp11::doubles rebal_upper, int n_ids) {
  writable::integers out(id.size());
  portfolio_sorts::carry_forward(INTEGER_RO(id), REAL_RO(date), id.size(),
                                 INTEGER_RO(rebal_id), REAL_RO(rebal_date),
                                 REAL_RO(rebal_upper), rebal_id.size(), n_ids,
                                 INTEGER(out));
  return out;
}

[[cpp11::register]]
cpp11::list aggregate_univariate_cpp11(cpp11::integers portfolio,
                                       cpp11::integers date, int n_portfolios,
                                       int n_dates, cpp11::doubles ret,
                                       cpp11::doubles w,
                                       cpp11::doubles w_capped,
                                       double min_size) {
  const R_xlen_t n_cells = (R_xlen_t)n_portfolios * n_dates;
  writable::integers n(n_cells);
  writable::doubles vw(n_cells), ew(n_cells), vw_capped(n_cells);
  const portfolio_sorts::CellOutput out = {INTEGER(n), REAL(vw), REAL(ew),
                                           REAL(vw_capped)};
  portfolio_sorts::aggregate_univariate(
      INTEGER_RO(portfolio), INTEGER_RO(date), portfolio.size(), n_portfolios,
      n_dates, REAL_RO(ret), REAL_RO(w), REAL_RO(w_capped), min_size, out);
  return cell_list(n, vw, ew, vw_capped);
}

[[cpp11::register]]
cpp11::list aggregate_bivariate_cpp11(cpp11::integers main,
                                      cpp11::integers secondary,
                                      cpp11::integers date, int n_main,
                                      int n_secondary, int n_dates,
                                      cpp11::doubles ret, cpp11::doubles w,
                                      cpp11::doubles w_capped,
                                      double min_size) {
  const R_xlen_t n_cells = (R_xlen_t)n_main * n_dates;
  writable::integers n(n_cells);
  writable::doubles vw(n_cells), ew(n_cells), vw_capped(n_cells);
  const portfolio_sorts::CellOutput out = {INTEGER(n), REAL(vw), REAL(ew),
                                           REAL(vw_capped)};
  portfolio_sorts::aggregate_bivariate(
      INTEGER_RO(main), INTEGER_RO(secondary), INTEGER_RO(date), main.size(),
      n_main, n_secondary, n_dates, REAL_RO(ret), REAL_RO(w),
      REAL_RO(w_capped), min_size, out);
  return cell_list(n, vw, ew, vw_capped);
}
