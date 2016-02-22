context("parse_address")

test_that("parsing works" {
  add <- parse_address(c("123 Main St", "546 Another word Dr"))
  expect_equal(length(add), 2)
  expect_identical(add[[1]], list(num=123, type="ST", street="MAIN"))
  expect_identical(add[[2]], list(num=546, type="DR", street="ANOTHER WORD"))
  
  add <- parse_address("78978 S Road St")[[1]]
  expect_equal(add, list(num=78978, prefix="S", type="ST", street="ROAD"))
  
  add <- parse_address("78978 S Road N St")[[1]]
  expect_equal(add, list(num=78978, prefix="S", type="ST", suffix="N", street="ROAD"))
  
  add <- parse_address("78978 S Road St N")[[1]]
  expect_equal(add, list(num=78978, prefix="S", suffix="N", type="ST", street="ROAD"))
  
  add <- parse_address("N CENTRAL EXPY SB")[[1]]
  expect_equal(add, list(prefix="N", direction="SB", type="EXPY", street="CENTRAL"))
})