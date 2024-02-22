find_mle_pseudo_inverse <- function(design, outcome) {
  as.vector(solve(t(design)%*%design, t(design)%*%outcome))
}

find_mle_BFGS <- function(beta_start, design, outcome, noise_var=1) {
  optim(beta_start, lm_likelihood, grad_likelihood, design=design, outcome=outcome, method="BFGS", control=list(fnscale=-1, abstol=1e-9))
}

lm_likelihood <- function(beta, design, outcome, noise_variance=1) {
  -0.5*sum((outcome - design%*%beta)^2)/noise_variance
}

grad_likelihood <- function(beta, design, outcome, noise_variance=1) {
  1/noise_variance*(t(design)%*%outcome - t(design)%*%design%*%beta)
}

logistic_loglikelihood <- function(beta, design, outcome) {
  sum(outcome*(design%*%beta)) - sum(log(1+exp(design%*%beta)))
}

logistic_grad_loglikelihood <- function(beta, design, outcome) {
  pi = exp(design%*%beta)/(1+exp(design%*%beta))
  return(t(design)%*%(outcome - pi))
}

logistic_hessian <- function(beta, design, outcome){
  pi = exp(design%*%beta)/(1+exp(design%*%beta))
  return(-t(design)%*%diag(as.vector(pi*(1-pi)))%*%design)
}

