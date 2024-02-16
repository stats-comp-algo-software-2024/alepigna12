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
