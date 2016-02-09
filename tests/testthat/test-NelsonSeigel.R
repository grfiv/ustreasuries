library(ustreasuries)
context("Test the Nelson Seigel functions")

# Test Nelson.Seigel
# ==================
all_data <- CMTrates()
rate.CMT <- dplyr::filter(all_data,
                             all_data$NEW_DATE>=as.Date("2016-01-29","%Y-%m-%d") &
                             all_data$NEW_DATE<=as.Date("2016-02-05","%Y-%m-%d"))
NSParameters <- Nelson.Siegel(rate=rate.CMT)
NSPnames <- names(NSParameters) == c("beta_0", "beta_1", "beta_2", "lambda")
test_that("Test Nelson.Seigel", {
    expect_equal(NSPnames, c(TRUE, TRUE, TRUE, TRUE))
    expect_equal(all.equal(NSParameters[[1]], 3.148307, tolerance=0.000001), TRUE)
})

# Test NSrates
# ==================
NS_rates <- NSrates(NSParameters)
test_that("Test NSrates", {
    expect_equal(all.equal(NS_rates[[1]], 0.255056, tolerance=0.000001), TRUE)
})

# Test NSzeros
# ==================
NS_zeros <- NSzeros(NSrates(NSParameters))
test_that("Test NSzeros", {
    expect_equal(all.equal(NS_zeros[[1]][1], 0.9997875, tolerance=0.000001), TRUE)
})

