test_that("MLE via BFGS matches result via pseudo-inverse", {
  set.seed(111)
  data = simulate_data(1000,4)
  design = data$design
  outcome = data$outcome
  beta = rep(0.1, ncol(design))
  mle_pseudo = find_mle_pseudo_inverse(design, outcome)
  mle_optim = find_mle_BFGS(beta, design, outcome)$par
  result = are_all_close(mle_pseudo, mle_optim, rel_tol = 1e-4)
  expect_true(result)
})

test_that("newton and bfgs outputs coincide on logit model", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_newton_out <- hiper_glm(design, outcome, model = 'logit')
  via_bfgs_out <- hiper_glm(
    design, outcome, model = 'logit', method = 'BFGS'
  )
  expect_true(are_all_close(
    coef(via_newton_out), coef(via_bfgs_out), abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
