test_that("gradient for linear model is correct", {
  set.seed(111)
  data = simulate_data(50,2)
  design = data$design
  outcome = data$outcome
  beta = data$coef_true
  grad = grad_likelihood(beta, design, outcome)
  num_grad = approx_grad(lm_likelihood, beta, design=design, outcome=outcome)
  result = are_all_close(grad,num_grad)
  expect_true(result)
})
