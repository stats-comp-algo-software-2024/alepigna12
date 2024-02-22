#'  @export
hiper_glm <- function (outcome, design, model="linear", method="BFGS") {
  if(!(model %in% c("linear", "logit"))){
    stop("Please, specify linear or logit model. Other models are not implemented")
  }
  else if(model="linear"){
    if(!(method %in% c("BFGS","pseudo-inverse"))){
      stop("Method must be BFGS or pseudo-inverse")
    }
    else if(method = "BFGS"){
      beta_start = c(0.1,ncol(design))
      beta_MLE = find_mle_BFGS(beta_start, design, outcome)
    }
    else{
      beta_MLE = find_mle_pseudo_inverse(design, outcome)
    }
  }
  else if(model="logit"){
    if(method!="BFGS"){
      stop("Only BFGS method currently implemented")
    }
    else{
      beta_start = c(0.1,ncol(design))
      beta_MLE = find_mle_newton(beta_start, design, outcome)
    }
  }

  #TO DO: additional output
  hglm <- list(coef=beta_MLE)
  class(hglm) <- "hglm"
  warning("Function not fully implemented yet")
}
