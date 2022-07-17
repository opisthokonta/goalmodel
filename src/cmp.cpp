#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <math.h>
#include <string>


// Internal function for computing the normalizing constant for the CMP distribution.
// [[Rcpp::export]]
Rcpp::NumericVector CMP_normalizing_constant(Rcpp::NumericVector& lambda,
                                                 Rcpp::NumericVector& upsilon,
                                                 double error = 0.01){

  // if any of the inputs are empty, return empty vector.
  if (lambda.length() == 0 | upsilon.length() == 0){
    Rcpp::NumericVector empty_vec (0);
    return empty_vec;
  }


  const int N = std::max(lambda.length(), upsilon.length());

  Rcpp::NumericVector res (N);

  double cc;

  for (int ll = 0; ll != N; ++ll){

    // reset current result.
    cc = 0;

    int lambda_idx = ll % lambda.length();
    int upsilon_idx = ll % upsilon.length();

    if (upsilon[upsilon_idx] == 1){
      res[ll] = std::exp(lambda[lambda_idx]);
      continue;
    }

    // Upper limit of sum.
    int ul;
    ul = std::max(20.0, std::ceil( std::pow(lambda[lambda_idx] / error, 1 / upsilon[upsilon_idx] ) ));

    for (int ii = 0; ii != ul; ++ii){
      cc += std::exp( (ii * std::log(lambda[lambda_idx] ))  -  upsilon[upsilon_idx] * lgamma(ii+1) );
    }

    res[ll] = cc;

  }

  return res;

}


//' @rdname CMP
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector dCMP(Rcpp::IntegerVector& x,
                         Rcpp::NumericVector& lambda,
                         Rcpp::NumericVector& upsilon,
                         bool log = false,
                         double error = 0.01){

  // http://en.wikipedia.org/wiki/Conway%E2%80%93Maxwell%E2%80%93Poisson_distribution
  // reparameteriseing
  // http://onlinelibrary.wiley.com/doi/10.1111/j.1539-6924.2008.01014.x/abstract

  // if any of the inputs are empty, return empty vector.
  if (lambda.length() == 0 | upsilon.length() == 0 | x.length() == 0){
    Rcpp::NumericVector empty_vec (0);
    return empty_vec;
  }

  int N = std::max(std::max(x.length(), lambda.length()), upsilon.length());

  Rcpp::NumericVector res (N);

  // The normalizing constant.
  Rcpp::NumericVector nconst = CMP_normalizing_constant(lambda, upsilon, error=error);

  double log_prob;

  for (int ii = 0; ii != N; ++ii){

    // Compute the index to look up.
    int x_idx = ii % x.size();
    int lambda_idx = ii % lambda.size();
    int upsilon_idx = ii % upsilon.size();
    int nconst_idx = ii % nconst.size();

    // Todo: check lambda[lambda_idx] >= 1 && upsilon[upsilon_idx] != 0

    if (Rcpp::IntegerVector::is_na(x[x_idx])){
      res[ii] = NA_REAL;
      continue;
    } else {
      if (x[x_idx] >= 0){

        log_prob = (x[x_idx] * std::log(lambda[lambda_idx])) - (upsilon[upsilon_idx] * lgamma(1 + x[x_idx])) - std::log(nconst[nconst_idx]);

        if (log == false){
          res[ii] = std::exp(log_prob);
        } else {
          res[ii] = log_prob;
        }

      } else {
        // if x < 0.
        if (log == false){
          res[ii] = 0;
        } else {
          res[ii] = NA_REAL;
        }
      }

    }


  }

  return res;

}



//' @rdname CMP
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector pCMP(Rcpp::IntegerVector& q,
                         Rcpp::NumericVector& lambda,
                         Rcpp::NumericVector& upsilon,
                         bool lower_tail=true,
                         double error = 0.01){

  // if any of the inputs are empty, return empty vector.
  if (lambda.length() == 0 | upsilon.length() == 0 | q.length() == 0){
    Rcpp::NumericVector empty_vec (0);
    return empty_vec;
  }

  int N = std::max(std::max(q.length(), lambda.length()), upsilon.length());

  Rcpp::NumericVector res (N);

  // The normalizing constant.
  Rcpp::NumericVector nconst = CMP_normalizing_constant(lambda, upsilon, error=error);

  double log_prob;

  for (int ii = 0; ii != N; ++ii){

    // Compute the index to look up.
    int q_idx = ii % q.size();
    int lambda_idx = ii % lambda.size();
    int upsilon_idx = ii % upsilon.size();
    int nconst_idx = ii % nconst.size();

    // Check if nconst is na is the same as checking if any of lambda or upsilon are NA.
    if (Rcpp::IntegerVector::is_na(q[q_idx]) || Rcpp::NumericVector::is_na(nconst[nconst_idx])){
      res[ii] = NA_REAL;
      continue;
    } else {

      // loop from 0 to x.
      log_prob = 0.0;
      if (q[q_idx] >= 0){
        for (int jj = 0; jj != q[q_idx]+1; ++jj){
          log_prob += std::exp((jj * std::log(lambda[lambda_idx])) - (upsilon[upsilon_idx] * lgamma(1 + jj)) - std::log(nconst[nconst_idx]));
        }
      }

      res[ii] = log_prob;

    }

  }


  if (lower_tail == false){
    for (int kk = 0; kk != res.size(); ++kk){
      res[kk] = 1 - res[kk];
    }

  }

  return res;

}

//' @rdname CMP
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector qCMP(Rcpp::NumericVector& p,
                         Rcpp::NumericVector& lambda,
                         Rcpp::NumericVector& upsilon,
                         bool lower_tail = true,
                         double error = 0.01){


  // if any of the inputs are empty, return empty vector.
  if (lambda.length() == 0 | upsilon.length() == 0 | p.length() == 0){
    Rcpp::NumericVector empty_vec (0);
    return empty_vec;
  }

  int N = std::max(std::max(p.length(), lambda.length()), upsilon.length());

  Rcpp::NumericVector res (N);

  // The normalizing constant.
  Rcpp::NumericVector nconst = CMP_normalizing_constant(lambda, upsilon, error=error);

  double qq;

  for (int ii = 0; ii != N; ++ii){

    // Compute the index to look up.
    int p_idx = ii % p.size();
    int nconst_idx = ii % nconst.size();

    if (Rcpp::IntegerVector::is_na(p[p_idx]) || Rcpp::NumericVector::is_na(nconst[nconst_idx])){
      res[ii] = NA_REAL;
      continue;
    }

    if (lower_tail == false){
      p[p_idx] = 1 - p[p_idx];
    }

    if (p[p_idx] < 0 || p[p_idx] > 1){
      qq = R_NaN;
    } else if (p[p_idx] == 0){
      qq = 0;
    } else if (p[p_idx] < 1){

      // Compute the index to look up.
      int lambda_idx = ii % lambda.size();
      int upsilon_idx = ii % upsilon.size();

      qq = 0;
      double cur_prob = 0;

      while (true){
        cur_prob += std::exp((qq * std::log(lambda[lambda_idx])) - (upsilon[upsilon_idx] * lgamma(1 + qq)) - std::log(nconst[nconst_idx]));

        if (cur_prob > p[p_idx]){
          break;
        }

        ++qq;

      }
    } else if (p[p_idx] == 1){
      qq = R_PosInf;
    }

    res[ii] = qq;

  }


  return res;

}




//' @rdname CMP
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector eCMP(Rcpp::NumericVector& lambda,
                             Rcpp::NumericVector& upsilon,
                             std::string method = "sum",
                             double error = 0.01){

  // if any of the inputs are empty, return empty vector.
  if (lambda.length() == 0 | upsilon.length() == 0){
    Rcpp::NumericVector empty_vec (0);
    return empty_vec;
  }


  int N = std::max(lambda.length(), upsilon.length());

  Rcpp::NumericVector res (N);


  if (method == "fast"){

    for (int ii = 0; ii != N; ++ii){
      int lambda_idx = ii % lambda.size();
      int upsilon_idx = ii % upsilon.size();

      res[ii] = std::pow(lambda[lambda_idx], (1/upsilon[upsilon_idx])) - ((upsilon[upsilon_idx] - 1) / (2*upsilon[upsilon_idx]));
    }

  } else if (method == "sum") {

    double ee;
    int ul;

    // The normalizing constant.
    Rcpp::NumericVector nconst = CMP_normalizing_constant(lambda, upsilon, error=error);

    for (int ii = 0; ii != N; ++ii){

      int lambda_idx = ii % lambda.size();
      int upsilon_idx = ii % upsilon.size();
      int nconst_idx = ii % nconst.size();

      // Check if nconst is na is the same as checking if any of lambda or upsilon are NA.
      if (Rcpp::NumericVector::is_na(nconst[nconst_idx])){
        res[ii] = NA_REAL;
        continue;
      }

      // Upper limit for computing the expectation
      // the same as for computing the normalization constant.
      ul = std::max(20.0, std::ceil( std::pow(lambda[lambda_idx] / error, 1 / upsilon[upsilon_idx]) ));

      // The expectation.
      ee = 0;

      // sum from 1 to ul. (kan skip jj = 0, since it evaluates to 0 anyway.
      for (int jj = 1; jj != ul; ++jj){
        ee += jj * std::exp((jj * std::log(lambda[lambda_idx])) - (upsilon[upsilon_idx] * lgamma(1 + jj)) - std::log(nconst[nconst_idx]));
      }

      res[ii] = ee;

    }

  }

  return res;

}



// Approximation of lambda, based on the
// approximation for expectation given in Shmueli et al (2005).
// [[Rcpp::export]]
Rcpp::NumericVector lambda_approx(Rcpp::NumericVector& mu,
                                      Rcpp::NumericVector& upsilon){


  // if any of the inputs are empty, return empty vector.
  if (mu.length() == 0 | upsilon.length() == 0){
    Rcpp::NumericVector empty_vec (0);
    return empty_vec;
  }

  int N = std::max(mu.length(), upsilon.length());

  Rcpp::NumericVector res (N);

  for (int ii = 0; ii != N; ++ii){
    int mu_idx = ii % mu.size();
    int upsilon_idx = ii % upsilon.size();

    res[ii] = std::pow(mu[mu_idx] + ((upsilon[upsilon_idx] - 1) / (2*upsilon[upsilon_idx])), upsilon[upsilon_idx]);
  }

  return res;

}

// The condition (eq. 2.2) in Huang (2017), for converting
// the mu and upsilon parameters to lambda.
//[[Rcpp::export]]
double lambda_cond(double& lambda, double& mu, double& upsilon, int ul = 100){

  double lc = 0;

  for (int ii = 0; ii != ul; ++ii){
    lc += std::exp((ii * std::log(lambda)) - upsilon * lgamma(ii+1)) * (ii - mu) ;
  }

  return lc;

}


