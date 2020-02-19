## Original Code: Mine Dogucu

## This is what we have done in class

## H_0 <=0.7
## H_A > 0.7

prior_alpha <- 100
prior_beta <- 10
bayesrules::plot_beta(prior_alpha, prior_beta)
x <- 76
n <- 100
posterior_alpha <- prior_alpha + x
posterior_beta <- prior_beta + n - x
bayesrules::plot_beta(posterior_alpha, posterior_beta)


prior_odds <- 
  
  pbeta(0.7, 
        prior_alpha, 
        prior_beta, 
        lower.tail = FALSE) /
  pbeta(0.7, 
        prior_alpha, 
        prior_beta)


posterior_odds <- 
  
  pbeta(0.7, 
        posterior_alpha, 
        posterior_beta, 
        lower.tail = FALSE) /
  pbeta(0.7, 
        posterior_alpha, 
        posterior_beta)

bayes_factor <- posterior_odds/prior_odds

bayes_factor





## Taneisha used a function writing approach to this.

# Original Code author: Taneisha Arora
# Edits: Mine Dogucu

## This function calculates a Bayes factor 
## for one sided hypothesis test using the Beta-Binomial model
## Note that the alternative is always "greater than" 
## some value (h_pi)

bb_bayes_factor <- function(prior_alpha, 
                             prior_beta, 
                             n, 
                             x, 
                             h_pi){
  
  posterior_alpha <- prior_alpha + x
  posterior_beta <- prior_beta + n - x
  
  prior_odds <- 
    pbeta(h_pi, prior_alpha, prior_beta, lower.tail = FALSE)/
    (pbeta(h_pi, prior_alpha, prior_beta))
  
  posterior_odds <- 
    pbeta(h_pi, posterior_alpha, posterior_beta, lower.tail = FALSE)/
    (pbeta(h_pi, posterior_alpha, posterior_beta))
  
  
  bayes_factor <- posterior_odds/prior_odds
  
  c("prior odds" = prior_odds, 
           "posterior odds" = posterior_odds, 
           "bayes factor" = bayes_factor)
} 

