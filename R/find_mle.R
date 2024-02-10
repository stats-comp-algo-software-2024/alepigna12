find_mle_pseudo_inverse <- function(X, y) {
  solve(t(X)%*%X, t(X)%*%y)
}

find_mle_BFGS <- function(beta_start,X,y,noise_var=1) {
  optim(beta_start,lm_likelihood,X=X,y=y,method="BFGS")
}

# Function used to verify that likelihood is right
lm_likelihood_test <- function(y,beta,X,noise_variance=1) {
  -length(y)/2*log(2*pi) -0.5*log(det(diag(noise_variance, length(y), length(y)))) - 0.5*t(y - X%*%beta)%*%diag(1/noise_variance,length(y),length(y))%*%(y-X%*%beta)
}
lm_likelihood <- function(beta,X,y,noise_variance=1) {
  -length(y)/2*log(2*pi) -0.5*log(det(diag(noise_variance, length(y), length(y)))) - 0.5*t(y - X%*%beta)%*%diag(1/noise_variance,length(y),length(y))%*%(y-X%*%beta)
}

grad_likelihood <- function(beta, X, y, noise_variance=1) {
  -diag(1/noise_variance, length(y), length(y))%*%(y-X%*%beta)
}

approx_grad <- function(func, x, ..., dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- rep(0, length(x))

  fn <- function(x) func(x,...)

  for(i in 1:length(x)){
    e = rep(0, times = length(x))
    e[i] = 1
    numerical_grad[i] = (fn(x + dx*e) - fn(x - dx*e))/(2*dx)
  }
  return(numerical_grad)
}
