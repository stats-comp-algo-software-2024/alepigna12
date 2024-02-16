#'  @export
hiper_glm <- function (outcome, design) {
  beta_start = c(0.1,ncol(design))
  beta_MLE = find_mle_BFGS(beta_start, design, outcome)
  #TO DO: additional output
  hglm <- list(coef=beta_MLE)
  class(hglm) <- "hglm"
  warning("Function not fully implemented yet")
}
