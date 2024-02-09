library(testthat)

# Define the test
test_that("Churn probability for highest probability customer is higher than lowest probability customer", {
  expect_true(my_fun(data, max_customer$CustomerId) > my_fun(data, min_customer$CustomerId))
})
