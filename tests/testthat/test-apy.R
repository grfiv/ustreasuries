library(ustreasuries)
context("Test APY")

test_that("APY(0.0800) == 0.0816", {
    expect_equal(APY(0.0800), 0.0816)
})

CMT = c(7.87, 7.92, 7.91, 7.92, 7.92, 7.92)
test_that("APY(CMT, percent=TRUE) OK", {
    expect_equal(APY(CMT, percent=TRUE),
                 c(0.08024842, 0.08076816, 0.08066420, 0.08076816,
                   0.08076816, 0.08076816))
})
