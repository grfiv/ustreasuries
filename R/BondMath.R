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
