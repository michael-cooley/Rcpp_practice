#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;

// [[Rcpp::export]]
double calculateIRR(const arma::vec& cashflows) {
  int n = cashflows.n_elem;
  arma::vec coeffs(n + 1, arma::fill::zeros);
  coeffs(n) = -cashflows(0);
  for (int i = 0; i < n; ++i) {
    coeffs(i) = cashflows(i + 1);
  }
  arma::Col<std::complex<double>> roots = arma::roots(coeffs);
  
  // Find the real root closest to 1
  double irr = 0.0;
  double min_distance = std::numeric_limits<double>::max();
  for (size_t i = 0; i < roots.n_elem; ++i) {
    if (roots(i).imag() == 0.0) {
      double distance = std::abs(roots(i).real() - 1.0);
      if (distance < min_distance) {
        min_distance = distance;
        irr = roots(i).real();
      }
    }
  }
  
  return irr - 1.0;
}
