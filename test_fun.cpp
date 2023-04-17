////////////////////////////
// Rcpp functions needed for shiny apps
////////////////////////////

// Compile this file in R by running this command:
// Rcpp::sourceCpp(file="/Users/jerzy/Develop/Presentations/test_fun.cpp")

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <vector>
using namespace arma;
// Use STL
using namespace std;


// This is an old version of run_reg() from 2022-02-09
// Used in app_runreg_daily_strat.R
// [[Rcpp::export]]
arma::mat run_reg_20220209(const arma::mat& response, 
                  const arma::mat& predictor,
                  double lambda, 
                  std::string method = "none") {
  
  arma::uword num_rows = predictor.n_rows;
  arma::uword num_cols = predictor.n_cols;
  arma::mat means_resp = arma::zeros<mat>(num_rows, 1);
  arma::mat means_pred = arma::zeros<mat>(num_rows, num_cols);
  arma::mat vars = arma::square(predictor);
  arma::mat covars = arma::zeros<mat>(num_rows, num_cols);
  arma::mat betas = arma::zeros<mat>(num_rows, num_cols);
  arma::mat alphas = arma::zeros<mat>(num_rows, 1);
  arma::mat resids = arma::zeros<mat>(num_rows, 1);
  arma::mat varz = arma::ones<mat>(num_rows, 1);
  arma::mat meanz = arma::zeros<mat>(num_rows, 1);
  double lambda1 = 1-lambda;
  
  // Perform loop over the rows
  means_resp.row(0) = response.row(0);
  means_pred.row(0) = predictor.row(0);
  for (arma::uword it = 1; it < num_rows; it++) {
    // Calculate the mean as the weighted sum
    means_resp.row(it) = lambda1*response.row(it) + lambda*means_resp.row(it-1);
    means_pred.row(it) = lambda1*predictor.row(it) + lambda*means_pred.row(it-1);
    // cout << "Calculating vars: " << it << endl;
    vars.row(it) = lambda1*arma::square(predictor.row(it)-means_pred.row(it)) + lambda*vars.row(it-1);
    // cout << "Calculating covars: " << it << endl;
    covars.row(it) = lambda1*((response.row(it)-means_resp.row(it))*(predictor.row(it)-means_pred.row(it))) + lambda*covars.row(it-1);
    // cout << "Calculating betas: " << it << endl;
    // Calculate the alphas and betas.
    betas.row(it) = lambda1*covars.row(it)/vars.row(it) + lambda*betas.row(it-1);
    alphas.row(it) = lambda1*(means_resp.row(it) - arma::dot(betas.row(it), means_pred.row(it))) + lambda*alphas.row(it-1);
    // cout << "Calculating resids: " << it << endl;
    // Calculate the residuals.
    resids.row(it) = lambda1*(response.row(it) - arma::dot(betas.row(it), predictor.row(it))) + lambda*resids.row(it-1);
    // Calculate the mean and variance of the residuals.
    meanz.row(it) = lambda1*resids.row(it) + lambda*meanz.row(it-1);
    varz.row(it) = lambda1*arma::square(resids.row(it) - meanz.row(it)) + lambda*varz.row(it-1);
  }  // end for
  
  if (method == "scale") {
    // Divide the residuals by their volatility
    resids = resids/sqrt(varz);
  } else if (method == "standardize") {
    // De-mean the residuals and divide them by their volatility
    resids = (resids - meanz)/sqrt(varz);
  }  // end if
  
  return join_rows(resids, alphas, betas);
  
}  // end run_reg_20220209


