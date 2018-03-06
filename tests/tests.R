context ("check integrateIt inputs")

test_that ("x and y are numeric", {
  expect_error(integrateIt(c("hallman","mahaffy"),c(1,2),"trapezoid"))
  expect_error(integrateIt(c(1,2), c("mahaffy","hallman"),"trapezoid"))
})

test_that ("inputs arte same length", {
  expect_error(integrateIt(c(1,2,3), c(1,2), "trapezoid"))
})

context("make sure it gets the expected answer")

test_that("trapezoid rule works", {
  expect_equal(integrateIt(c(1,2,3),c(4,5,6),"trapezoid")@area, 10)
})

test_that("simpson rule works", {
  expect_equal(integrateIt(c(1,2,3),c(4,5,6),"Simpson")@area, 10)
})