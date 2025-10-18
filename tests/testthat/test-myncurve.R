test_that("see if area is as expected", {
  expect_equal(myncurve(500,200,300)$area, round(pnorm(300,500,200),4))
  expect_equal(myncurve(10,5,6)$area, round(pnorm(6,10,5),4))
  expect_equal(myncurve(2.343,1.213,3.543)$area, round(pnorm(3.543,2.343,1.213),4))
})

test_that("see if mu is as expected", {
  expect_equal(myncurve(500,200,300)$mu, 500)
  expect_equal(myncurve(10,5,6)$mu, 10)
  expect_equal(myncurve(2.343,1.213,3.543)$mu, 2.343)
})

test_that("see if sigma is as expected", {
  expect_equal(myncurve(500,200,300)$sigma, 200)
  expect_equal(myncurve(10,5,6)$sigma, 5)
  expect_equal(myncurve(2.343,1.213,3.543)$sigma, 1.213)
})
