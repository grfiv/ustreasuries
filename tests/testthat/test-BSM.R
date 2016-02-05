library(ustreasuries)
context("Test the Black-Scholes-Merton formulas")

# Test European Call/Put
# ======================

Stock = 49
Exercise = 50
Time = 20/52
Interest = 0.05
sigma = 0.20

Yield = 0
test_that("European Call Option Pricing WITHOUT dividends", {
    expect_equal(all.equal(EuroCall(Stock, Exercise, Time, Interest, Yield, sigma),
                           2.400534, tolerance=0.01), TRUE)
})

Yield = 0.13
test_that("European Call Option Pricing WITH dividends", {
    expect_equal(all.equal(EuroCall(Stock, Exercise, Time, Interest, Yield, sigma),
                           1.343134, tolerance=0.01), TRUE)
})

# Test DeltaCall/Put
# ==================

Stock    <- 49     # S_0
Exercise <- 50     # K
Time     <- 20/52  # T
Interest <- 0.05   # r
Yield    <- 0      # q
sigma    <- 0.20

dcall <- DeltaCall(Stock, Exercise, Time, Interest, Yield, sigma)
dput  <- DeltaPut(Stock, Exercise, Time, Interest, Yield, sigma)

test_that("DeltaCall/Put", {
    expect_equal(all.equal(dcall, 0.5216047, tolerance=0.0001), TRUE)
    expect_equal(all.equal(dput, -0.4783953, tolerance=0.0001), TRUE)
})

# Test ThetaCall/Put
# ==================

Stock    <- 49     # S_0
Exercise <- 50     # K
Time     <- 20/52  # T
Interest <- 0.05   # r
Yield    <- 0      # q
sigma    <- 0.20

thcall <- ThetaCall(Stock, Exercise, Time, Interest, Yield, sigma)
thput  <- ThetaPut(Stock, Exercise, Time, Interest, Yield, sigma)

test_that("ThetaCall/Put", {
    expect_equal(all.equal(thcall, -4.30533, tolerance=0.0001), TRUE)
    expect_equal(all.equal(thput, -1.852947, tolerance=0.0001), TRUE)
})

# Test Gamma
# ==========

Stock    <- 49     # S_0
Exercise <- 50     # K
Time     <- 20/52  # T
Interest <- 0.05   # r
Yield    <- 0      # q
sigma    <- 0.20

gamma <- Gamma(Stock, Exercise, Time, Interest, Yield, sigma)

test_that("Gamma", {
    expect_equal(all.equal(gamma, 0.06554404, tolerance=0.0001), TRUE)
})

# Test Vega
# =========

Stock    <- 49     # S_0
Exercise <- 50     # K
Time     <- 20/52  # T
Interest <- 0.05   # r
Yield    <- 0      # q
sigma    <- 0.20

vega <- Vega(Stock, Exercise, Time, Interest, Yield, sigma)

test_that("Vega", {
    expect_equal(all.equal(vega, 12.10548, tolerance=0.0001), TRUE)
})

# Test RhoCall/RhoPut
# ===================

Stock    <- 49     # S_0
Exercise <- 50     # K
Time     <- 20/52  # T
Interest <- 0.05   # r
Yield    <- 0      # q
sigma    <- 0.20

rhocall <- RhoCall(Stock, Exercise, Time, Interest, Yield, sigma)
rhoput  <- RhoPut(Stock, Exercise, Time, Interest, Yield, sigma)

test_that("RhoCall/RhoPut", {
    expect_equal(all.equal(rhocall, 8.906961, tolerance=0.0001), TRUE)
    expect_equal(all.equal(rhoput, -9.957519, tolerance=0.0001), TRUE)
})

# Test EuroCallVol/EuroPutVol
# ===========================

Stock      <- 21     # S_0
Exercise   <- 20     # K
Time       <- 0.25   # T
Interest   <- 0.10   # r
Yield      <- 0      # q
Call_price <- 1.875
Put_price  <- 1.875

callvol <- EuroCallVol(Stock, Exercise, Time, Interest, Yield, Call_price)
putvol  <- EuroPutVol(Stock, Exercise, Time, Interest, Yield, Put_price)

test_that("EuroCallVol/EuroPutVol", {
    expect_equal(all.equal(callvol, 0.2345128, tolerance=0.0001), TRUE)
    expect_equal(all.equal(putvol,  0.6348004, tolerance=0.0001), TRUE)
})
