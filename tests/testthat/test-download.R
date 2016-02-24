library(ustreasuries)
context("Test data download")

# Test CMTrates
# =============

test_that("class and attr of CMTrates is as expected", {
    expect_equal(class(CMTrates()),c('tbl_df', 'tbl', 'data.frame'))
    expect_equal(attr(CMTrates(), "data.source"), "CMT")
})
test_that("names of columns are as expected for CMTrates", {
    expect_equal(names(CMTrates()),c("Id", "NEW_DATE", "BC_1MONTH",
                                            "BC_3MONTH","BC_6MONTH","BC_1YEAR",
                                            "BC_2YEAR","BC_3YEAR","BC_5YEAR",
                                            "BC_7YEAR","BC_10YEAR","BC_20YEAR",
                                            "BC_30YEAR","BC_30YEARDISPLAY"))
})
test_that("classes of columns of CMTrates are as expected", {
    expect_equal(unname(sapply(CMTrates(), class)),
                 c("numeric","Date","numeric","numeric","numeric","numeric",
                   "numeric","numeric","numeric","numeric","numeric","numeric",
                   "numeric","numeric") )
})
all_data = CMTrates()
test_that("first non-NA dates are as expected", {
    expect_equal(all_data[[min(which(!is.na(all_data['BC_1MONTH']))),'NEW_DATE']],
                 as.Date("2001-07-31"))
    expect_equal(all_data[[min(which(!is.na(all_data['BC_3MONTH']))),'NEW_DATE']],
                 as.Date("1982-01-04"))
    expect_equal(all_data[[min(which(!is.na(all_data['BC_6MONTH']))),'NEW_DATE']],
                 as.Date("1982-01-04"))
    expect_equal(all_data[[min(which(!is.na(all_data['BC_1YEAR']))),'NEW_DATE']],
                 as.Date("1962-01-02"))
    expect_equal(all_data[[min(which(!is.na(all_data['BC_2YEAR']))),'NEW_DATE']],
                 as.Date("1976-06-01"))
    expect_equal(all_data[[min(which(!is.na(all_data['BC_3YEAR']))),'NEW_DATE']],
                 as.Date("1962-01-02"))
    expect_equal(all_data[[min(which(!is.na(all_data['BC_5YEAR']))),'NEW_DATE']],
                 as.Date("1962-01-02"))
    expect_equal(all_data[[min(which(!is.na(all_data['BC_7YEAR']))),'NEW_DATE']],
                 as.Date("1969-07-01"))
    expect_equal(all_data[[min(which(!is.na(all_data['BC_10YEAR']))),'NEW_DATE']],
                 as.Date("1962-01-02"))
    expect_equal(all_data[[min(which(!is.na(all_data['BC_20YEAR']))),'NEW_DATE']],
                 as.Date("1993-10-01"))
    expect_equal(all_data[[min(which(!is.na(all_data['BC_30YEAR']))),'NEW_DATE']],
                 as.Date("1977-02-15"))
})

test_that("number of rows is as expected", {
    expect_lte(all_data$NEW_DATE[nrow(all_data)], Sys.Date())
})

# Test FedInvestData
# ==================

fedinvest_data <- FedInvestData()

test_that("class and attr of FedInvestData is as expected", {
    expect_equal(class(fedinvest_data),c('tbl_df', 'tbl', 'data.frame'))
    expect_equal(attr(fedinvest_data, "data.source"), "FedInvest")
})

test_that("names of columns are as expected for FedInvestData", {
    expect_equal(names(fedinvest_data),c("CUSIP", "SECURITY.TYPE", "RATE",
                                         "MATURITY.DATE", "CALL.DATE", "BUY",
                                         "SELL", "END.OF.DAY", "Date") )
})

test_that("classes of columns of FedInvestData are as expected", {
    expect_equal(unname(sapply(fedinvest_data, class)),
                 c("character", "character", "character", "Date", "logical",
                 "numeric", "numeric", "numeric", "Date") )
})

# Test sp500 & sp500TR
# ====================

sp500_idx <- SP500()

test_that("SP500 names", {
    expect_equal(names(sp500_idx), c("Date","Open","High","Low","Close","Volume","Adj.Close"))
})

sp500_idx_tr <- SP500TR()

test_that("sp500 names", {
    expect_equal(names(sp500_idx_tr), c("Date","Open","High","Low","Close","Volume","Adj.Close"))
})
