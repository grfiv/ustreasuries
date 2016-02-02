library(ustreasuries)
context("Test the Black-Scholes-Merton formulas")

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

