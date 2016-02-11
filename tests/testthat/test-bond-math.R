library(ustreasuries)
context("Test bond math")

# Test APY
# ========
test_that("APY(0.0800) == 0.0816", {
    expect_equal(APY(0.0800), 0.0816)
})

CMT = c(7.87, 7.92, 7.91, 7.92, 7.92, 7.92)
test_that("APY(CMT, percent=TRUE) OK", {
    expect_equal(APY(CMT, percent=TRUE),
                 c(0.08024842, 0.08076816, 0.08066420, 0.08076816,
                   0.08076816, 0.08076816))
})

# Test YTM
# ========
ytm1 <- YTM(107.8906, 0.0475, 9.5)
ytm2 <- YTM(141.5267, 0.08875, 9.5)

test_that("YTM", {
    expect_equal(all.equal(ytm1, 0.0375479295223684, tolerance=0.000001), TRUE)
    expect_equal(all.equal(ytm2, 0.0366032177811388, tolerance=0.000001), TRUE)
})

# Test Price
# ==========
price1 <- Price(0.0375479295223684, 0.04750, 9.5)
price2 <- Price(0.0366032177811388, 0.08875, 9.5)

test_that("Price", {
    expect_equal(all.equal(price1, 107.8906, tolerance=0.000001), TRUE)
    expect_equal(all.equal(price2, 141.5267, tolerance=0.000001), TRUE)
})
