test_that("MLE via BFGS matches result via pseudo-inverse", {
  set.seed(111)
  data = simulate_data(1000,4)
  design = data$design
  outcome = data$outcome
  beta = rep(0.1, ncol(design))
  mle_pseudo = find_mle_pseudo_inverse(design, outcome)$beta_mle
  mle_optim = find_mle_BFGS(beta, design, outcome)$beta_mle
  result = are_all_close(mle_pseudo, mle_optim, rel_tol = 1e-4)
  expect_true(result)
})

test_that("newton and bfgs outputs coincide on logit model", {
  n_obs <- 32; n_pred <- 2
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 111)
  design <- data$design; outcome <- data$outcome
  via_newton_out <- hiper_glm(outcome, design, model = 'logit', method = 'newton')
  via_bfgs_out <- hiper_glm(
    outcome, design, model = 'logit', method = 'BFGS'
  )
  expect_true(are_all_close(
    via_newton_out, via_bfgs_out, abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
