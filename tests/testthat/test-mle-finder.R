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
