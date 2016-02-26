#' Convert CMT to APY
#'
#' @description
#' Convert Constant-Maturity Treasury (CMT) yields to
#' Annualized Percentage Yields (APY)
#'
#' @param i vector of CMT yields to be converted
#' @param percent boolean, if TRUE, i should be divided by 100
#'                  (for example 7.87 becomes 0.0787)
#' @return APY yield(s) as a decimal (not as a percent)
#'
#' @author George Fisher \email{GeorgeRFisher@gmail.com}
#'
#' @references
#' Interest Rates - Frequently Asked Questions
#'
#' \url{https://www.treasury.gov/resource-center/faqs/Interest-Rates/Pages/faq.aspx#2}
#'
#' @examples
#' APY(0.0800) #> [1] 0.0816
#'
#' CMT = c(7.87, 7.92, 7.91, 7.92, 7.92, 7.92)
#' APY(CMT, percent=TRUE)
#' #> [1] 0.08024842 0.08076816 0.08066420 0.08076816 0.08076816 0.08076816
#'
#' @export
APY <- function(i, percent=FALSE) {

    if (percent) {
        i <- i / 100
    }

    return(((1 + i/2)**2) - 1)
}

#' Convert fractional years into a lubridate period object
#'
#' @description
#' Given a fractional number of years you need to convert into years, months and
#' days to do date math
#'
#' See the example for the conversion
#'
#' @author
#' George Fisher
#'
#' @param years Fractional years: 2 years, 3 months, 10 days = 2 + 3/12 + 10/365 = 2.277397
#' @return a lubridate period object
#'
#' @examples
#' library(ustreasuries)
#' (diff_tm <- YearsMonthsDays(2 + 3/12 + 10/365))
#'
#' # July 1, 2000 plus 2 years (2002), 3 months (10), 10 days (11)
#' as.Date(lubridate::ymd("2000/07/01") + diff_tm)
#'
#' @export
YearsMonthsDays <- function(years) {
    ret = list(years  = 0,
               months = 0,
               days   = 0)
    if (years >= 1) {
        ret$years <- (as.integer(years - years%%1))
        years     <- years%%1
    }
    if (years*12 >= 1) {
        ret$months <- (as.integer(years*12 - (years*12)%%1))
        years      <- years - ret$months/12
    }
    if (years >= 0) {
        ret$days <- ceiling(as.integer(years*365))
    }
    return(lubridate::years(ret$years)+months(ret$months)+lubridate::days(ret$days))
}

#' Yield To Maturity
#'
#' @description
#' "Lazy-Man's" YTM: no dates, just fractional years
#'
#' @param  price the actual price paid
#' @param coupon the annual coupon rate
#' @inheritParams YearsMonthsDays
#' @return the YTM
#'
#' @author George Fisher
#'
#' @examples
#' # Veronesi example 2.11
#' library(ustreasuries)
#'
#' ytm <- YTM(107.8906, 0.0475, 9.5)
#' print(paste0(ytm, " = ", round(ytm*100,4),"%"))
#' ytm <- YTM(141.5267, 0.08875, 9.5)
#' print(paste0(ytm, " = ", round(ytm*100,4),"%"))
#'
#' @export
YTM <- function(price, coupon, years) {
    diff_tm <- YearsMonthsDays(years)
    settle  <- lubridate::ymd("2008/02/15") # Veronesi ch2 example 2.11
    mature  <- settle + diff_tm
    if (weekdays(mature) == "Saturday") mature <- mature + lubridate::days(2)
    if (weekdays(mature) == "Sunday")   mature <- mature + lubridate::days(1)
    return(jrvFinance::bond.yield(settle=settle, mature=mature,
                                  coupon=coupon, price=price,
                                  freq=2, convention="ACT/ACT"))
}

#' Bond Price
#'
#' @description
#' "Lazy-Man's" bond price: no dates, just fractional years
#'
#' @param ytm yield to maturity
#' @inheritParams YTM
#' @return bond price
#'
#' @author
#' George Fisher
#'
#' @examples
#' # Veronesi example 2.11
#' library(ustreasuries)
#'
#' Price(0.0375479295223684, 0.04750, 9.5)
#' Price(0.0366032177811388, 0.08875, 9.5)
#'
#' @export
Price <- function(ytm, coupon, years) {
    diff_tm <- YearsMonthsDays(years)
    settle  <- lubridate::ymd("2008/02/15") # Veronesi ch2 example 2.11
    mature  <- settle + diff_tm
    if (weekdays(mature) == "Saturday") mature <- mature + lubridate::days(2)
    if (weekdays(mature) == "Sunday")   mature <- mature + lubridate::days(1)
    return(jrvFinance::bond.price(settle=settle, mature=mature,
                                  yield=ytm,     coupon=coupon,
                                  freq=2, convention="ACT/ACT"))
}

#' Use the Booststrap Method to determine discount factors Z(0, T_i)
#'
#' @description
#' Assuming a sequence of semi-annual coupon bonds beginning with
#' a maturity equal to six months and increasing, this function determines
#' the sequence of discount factors for each time segment using the boothstrap
#' method. This method is limited by the facts that it assumes evenly-spaced
#' six-month maturities and an unbroken sequence. It is also iterative instead
#' of using matrix algebra.
#'
#' @seealso
#' Nelson Seigel and Svensson are more helpful for dirty, real-life bond data.
#'
#' @author
#' George Fisher
#'
#' @references
#' Veronesi Ch2 p46-47
#'
#' @param prices a vector of prices from the shortest (6-months) to the longest maturity
#' @param coupons a vector, usually beginning with 0, with the annual coupon
#' rate as a decimal
#' @return  Z a vector of the discount factors
#'
#' @examples
#' # Veronesi Ch2 p 46
#' p1 <- 98.3607
#' c1 <- 0
#'
#' p2 <- 99.2343
#' c2 <- 0.0275
#'
#' p3 <- 99.1093
#' c3 <- 0.03
#'
#' prices  <- c(p1, p2, p3)
#' coupons <- c(c1, c2, c3)
#'
#' Zbootstrap(prices, coupons)
#'
#' @export
Zbootstrap <- function(prices, coupons) {
    Z <- c()
    Z[1] <- prices[1] / (100 * (1 + coupons[1]/2))
    for (i in 2:length(prices)) {
        Z[i] <- (prices[i] - coupons[i]/2*100 * sum(Z)) / (100 * (1 + coupons[i]/2))
    }
    return(Z)
}

#' Derive Spot Rate from Discount Factor and Fractional Years
#'
#' @description Given a discount factor Z(t, T) and T - t, return the spot rate
#'
#' @param Z the discount factor (a decimal in (0, 1))
#' @param years the fractional years to maturity
#' @return the spot rate in decimal form
#'
#' @references
#' See the vignette "veronesi-ch02" Example 2.11
#'
#' @author
#' George Fisher
#'
#' @examples
#' library(ustreasuries)
#' Z <- c(0.989590, 0.981892, 0.973147, 0.962441, 0.950822, 0.937612, 0.922213,
#'        0.906046, 0.887259, 0.869809, 0.850858, 0.831241, 0.811114, 0.790613,
#'        0.768759, 0.748256, 0.726763, 0.708392, 0.691582, 0.681581)
#'
#'  years <- c(0.5,  1.0,  1.5,  2.0,  2.5,  3.0,  3.5,  4.0,  4.5,  5.0,  5.5,
#'             6.0,  6.5,  7.0,  7.5,  8.0,  8.5,  9.0,  9.5, 10.0)
#'
#'  spot_rate(Z, years)
#'
#' @export
spot_rate <- function(Z, years) {
    CAGR(Z, 1, years, type="continuous")
}
