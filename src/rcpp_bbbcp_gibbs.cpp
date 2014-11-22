
#include <Rcpp.h>
using namespace Rcpp;


double l(double x, double n, List prior) {
  return ::Rf_lbeta(x + prior["a"], n - x + prior["b"]) - prior["norm"];
}

//[[Rcpp::export]]
int rcpp_bbbcp_gibbs_step(IntegerVector x, IntegerVector n, LogicalVector cpt, int b, 
                          NumericVector h, List prior) {

  int end = 1;
  int k = x.length();
  
  int x_left = x[0];
  int n_left = n[0];
  int x_right = -99999;
  int n_right = -99999;

  for(int i = 1; i < k; i++) {
    int u = cpt[i];

    if(i == end) {
      x_right = x[end];
      n_right = n[end];
      while(!cpt[++end] && end < k) {
        x_right += x[end];
        n_right += n[end];
      }
    }
    
    double bothP  = h[b]  + l(x_left + x_right, n_left + n_right, prior); 
    double splitP = h[b+1] + l(x_left, n_left, prior) + l(x_right, n_right, prior);
    double prob = 1 / (1 + exp(splitP - bothP)); 
    
    cpt[i] = R::runif(0,1) > prob;
    
    b += cpt[i] - u; 
    
    x_right -= x[i];
    n_right -= n[i];
    
    if(cpt[i]) {
      x_left = x[i];
      n_left = n[i];
    } else {
      x_left += x[i];
      n_left += n[i];
    }
    
    
  }

  return b;

}

// [[Rcpp::export]]
List rcpp_bbbcp_gibbs(IntegerVector x, IntegerVector n, List prior, List control) {

    int k = x.length();
    int b = 1;
    
    // MCMC control constants
    int      M = as<int>(control["mcmc.iterations"]);
    int   thin = as<int>(control["mcmc.thin"]);
    int burnin = as<int>(control["mcmc.burnin"]);
        
    //Precalculate constants
    prior["norm"] = R::lbeta(prior["a"], prior["b"]);
    
    NumericVector h(16);
    for(int i = 1; i < h.length(); i++)
      h[i] = R::lbeta(i+1, k - i);
      
    
    // Assumming approx 15 cpts  for now.
    IntegerVector cpt_i(M * 15);
    IntegerVector cpt_j(M * 15);
    int cpt_index = 0;

    LogicalVector cpt(k);
    
    
    //Burnin
    for(int i = thin; i < burnin; i++) {
      b = rcpp_bbbcp_gibbs_step(x, n, cpt, b, h, prior);
    }
    
    
    //Main loop
    for(int i = 0; i < M/thin; i++) {
      
      // Run thinned iterations inplace
      for(int j = 0; j < thin; j++) {
//        Rcout << "++" << i << "**" << j  << "\n";
        b = rcpp_bbbcp_gibbs_step(x, n, cpt, b, h, prior);
      }  
         
      // save out
      for(int j = 0; j < k; j++) {
        if(cpt[j]) {
          cpt_i[cpt_index] = i;
          cpt_j[cpt_index] = j;
          cpt_index++;
        }
      }
    }
    

    return List::create(cpt_i, cpt_j) ;
}

