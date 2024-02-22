find_mle_pseudo_inverse <- function(design, outcome) {
  beta = as.vector(solve(t(design)%*%design, t(design)%*%outcome))
  noise_variance = mean((outcome - design%*%beta)^2)
  n = nrow(design)
  p = ncol(design)
  noise_variance = noise_variance/(1-p/n)
  info = t(design)%*%design/noise_variance
  return(list(beta_mle=beta, information=info))
}

find_mle_BFGS <- function(beta_start, design, outcome, noise_variance=1, model="linear") {
  if(model == "linear"){
    model_res = optim(beta_start, lm_likelihood, grad_likelihood, design=design, outcome=outcome, method="BFGS", control=list(fnscale=-1, abstol=1e-9), hessian=TRUE)
  }
  else if(model == "logit"){
    model_res = optim(beta_start, logistic_loglikelihood, logistic_grad_loglikelihood, design=design, outcome=outcome, method="BFGS", control=list(fnscale=-1, abstol=1e-9), hessian=TRUE)
  }
  return(list(beta_mle=model_res$par, information=-model_res$hessian))
}

lm_likelihood <- function(beta, design, outcome, noise_variance=1) {
  -0.5*sum((outcome - design%*%beta)^2)/noise_variance
}

grad_likelihood <- function(beta, design, outcome, noise_variance=1) {
  1/noise_variance*(t(design)%*%outcome - t(design)%*%design%*%beta)
}

find_mle_newton <- function(beta_start, design, outcome){
  beta = beta_start
  n_iter = 0
  change = 5
  abs_tol = 0.05*(qchisq(0.975,1) - qchisq(0.025,1))
  while(!are_all_close(change,0,abs_tol)){
    hessian = logistic_hessian(beta,design,outcome)
    gradient = logistic_grad_loglikelihood(beta,design,outcome)
    curr_lik = logistic_loglikelihood(beta,design,outcome)
    beta = beta + solve(hessian,gradient)
    change = abs(logistic_loglikelihood(beta,design,outcome) - curr_lik)
    n_iter = n_iter + 1
    if(n_iter == 100000){
      stop("Algorithm did not converge")
    }
  }
  return(list(beta_MLE=beta, information=-logistic_hessian(beta,design,outcome)))
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

