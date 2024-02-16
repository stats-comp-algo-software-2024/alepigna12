test_that("TRUE when they are close", {
  v = c(5 - 1e-7, 10 - 1e-7, 20 - 1e-7)
  w = c(5, 10, 20)
  #' abs_error = (1e-7, 1e-7, 1e-7)
  #' rel_error = (2e-8, 1e-8, 5e-9)
  result = are_all_close(v, w, abs_tol = 1e-6, rel_tol = 1e-6)
  expect_true(result)
})

test_that("FALSE when high absolute error", {
  v = c(50 - 1e-5, 100 - 1e-5, 200 - 1e-5)
  w = c(50, 100, 200)
  #' abs_error = (1e-5, 1e-5, 1e-5) --> greater than abs_tol
  #' rel_error = (2e-7, 1e-7, 5e-8)
  result = are_all_close(v, w, abs_tol = 1e-6, rel_tol = 1e-6)
  expect_false(result)
})

test_that("FALSE when high relative error", {
  v = c(0.005 - 1e-7, 0.01 - 1e-7, 0.02 - 1e-7)
  w = c(0.005, 0.01, 0.02)
  #' abs_error = (1e-7, 1e-7, 1e-7)
  #' rel_error = (2e-5, 1e-5, 5e-6)  --> greater than rel_tol
  result = are_all_close(v, w, abs_tol = 1e-6, rel_tol = 1e-6)
  expect_false(result)
})
