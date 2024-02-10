test_that("multiplication works", {
  data = simulate_data(50,2)
  X = data$design
  y = data$outcome
  beta = c(0.1,0.1)
  mle_pseudo = find_mle_pseudo_inverse(X,y)
  mle_optim = find_mle_BFGS(beta,X,y)
  result = are_all_close(grad,num_grad)
  expect_true(result)
})
