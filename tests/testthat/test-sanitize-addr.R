context("sanitize_addr")

test_that("sanitization works", {
  expect_equal(sanitize_addr("L B J SERV EB"), "L B J")
  expect_equal(sanitize_addr("L B J SERV WB"), "L B J")
})