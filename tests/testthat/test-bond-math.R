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

# Test Zbootstrap
# ===============
p1 <- 98.3607
c1 <- 0
p2 <- 99.2343
c2 <- 0.0275
p3 <- 99.1093
c3 <- 0.03
prices  <- c(p1, p2, p3)
coupons <- c(c1, c2, c3)
Z <- Zbootstrap(prices, coupons)
test_that("Zbootstrap", {
    expect_equal(all.equal(Z, c(0.9836070, 0.9655422, 0.9476411),tolerance=1.6e-8), TRUE)
})

# Test spot_rate
# ==============
Z <- c(0.989590, 0.981892, 0.973147, 0.962441, 0.950822, 0.937612, 0.922213,
       0.906046, 0.887259, 0.869809, 0.850858, 0.831241, 0.811114, 0.790613,
       0.768759, 0.748256, 0.726763, 0.708392, 0.691582, 0.681581)

years <- c(0.5,  1.0,  1.5,  2.0,  2.5,  3.0,  3.5,  4.0,  4.5,  5.0,  5.5,
           6.0,  6.5,  7.0,  7.5,  8.0,  8.5,  9.0,  9.5, 10.0)

sr <- spot_rate(Z, years)

table2.2 <- c(0.020930, 0.018274, 0.018147, 0.019141, 0.020172, 0.021473, 0.023137,
              0.024666, 0.026582, 0.027896, 0.029365, 0.030806, 0.032207, 0.033564,
              0.035064, 0.036251, 0.037548, 0.038306, 0.038818, 0.038334)
test_that("spot_rate", {
    expect_equal(all.equal(round(sr, 6), table2.2, tolerance=1.0e-04), TRUE)
})
