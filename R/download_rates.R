#' Download Constant-Maturity Treasury (CMT) rates
#'
#' For the dates 1962-01-02 to 2015-12-31 the data is retrieved from a static
#' file that was previously created from the data downloaded; the
#' data for 2016-01-01 to present are pulled from an XML feed from the US
#' Treasury accessed in real time.
#'
#' @section data.frame format:
#'
#'   The data.frame returned contains one row for every day, sorted in
#'   descending order by date. The columns are "Id", "NEW_DATE", "BC_1MONTH",
#'   "BC_3MONTH", "BC_6MONTH", "BC_1YEAR", "BC_2YEAR",  "BC_3YEAR", "BC_5YEAR",
#'   "BC_7YEAR", "BC_10YEAR", "BC_20YEAR", "BC_30YEAR", "BC_30YEARDISPLAY".
#'
#'   "Id" is \code{NA} for the historical data; for the current data it is an
#'   integer returned in the XML feed. "NEW_DATE" is formatted as
#'   \code{as.Date("1962-01-02")}. All the other columns are numeric in
#'   "percentage" format: 7.12 means 7.12\% or 0.0712.
#'
#'   Yields are missing for certain maturities on various dates: for example, in
#'   the 1990's, as a result of a budget surplus, certain bonds were retired.
#'
#'   The size of the \code{data.frame} downloaded on 2016-01-25 was 1,514,504
#'   bytes.
#'
#' @section Starting Dates:
#'
#'   Different maturities have different starting dates:
#'
#'   M01 2001-07-31
#'
#'   M03 1982-01-04
#'
#'   M06 1982-01-04
#'
#'   Y01 1962-01-02
#'
#'   Y02 1976-06-01
#'
#'   Y03 1962-01-02
#'
#'   Y05 1962-01-02
#'
#'   Y07 1969-07-01
#'
#'   Y10 1962-01-02
#'
#'   Y20 1993-10-01
#'
#'   Y30 1977-02-15
#'
#' @return \code{data.frame} containing daily rates from 1962 to the
#'   most-recently completed business day. The class has c("ustreasuries", "CMT")
#'   appended so that the data.frame can be identified by other functions.
#'
#' @author George Fisher \email{GeorgeRFisher@gmail.com}
#'
#' @references
#'   Static Data
#'
#'   \url{http://www.federalreserve.gov/datadownload/Choose.aspx?rel=H15}
#'
#'   XML Data
#'
#'   \url{http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData}
#' @examples
#' all_data <- CMTrates()
#' head(all_data, n=3L)
#' # =========================
#' # Above: the earliest dates
#' # =========================
#'
#'
#' tail(all_data)
#' # ============================
#' # Above: the most-recent dates
#' # ============================
#'
#' @export
CMTrates <- function() {
    # supresses
    # Note: no visible binding for global variable 'NEW_DATE'
    assign("NEW_DATE", NULL)

    # =============================
    # Load the rates from 1962-2015
    # by loading a static data file
    # =============================
    if (file.exists("R/sysdata.rda"))
        load(file="R/sysdata.rda")
    if (file.exists("sysdata.rda"))
        load(file="sysdata.rda")

    # remove the superfluous FedInvest_historical_data file
    #if (exists("FedInvest_historical_data")) rm("FedInvest_historical_data")

    # ============================================
    # append the rates from 2016 to current year
    # by downloading the near-real-time XML stream
    # ============================================
    years <- seq(from=2016, to=as.integer(substr(Sys.Date(),1,4)))

    for (year in years) {

        # =========================
        # build the XML request URL
        # =========================
        base_url    <- "http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData"
        year_filter <- "?$filter=year(NEW_DATE)%20eq%20"
        url         <- paste0(base_url, year_filter, year)

        # =============================
        # pull down the XML data stream
        # =============================
        treas_data <- RCurl::getURLContent(url)

        # ==============================================
        # Pull out the 'entry' nodes from the XML stream
        # ==============================================
        doc        <- XML::xmlParse(treas_data)
        entryNodes <- XML::xmlRoot(doc)['entry']

        # =================================================================
        # Parse each entry node and create a temporary dataframe (elements)
        # =================================================================
        elements <- NULL
        for (entry in entryNodes) {

            # the children of <content type="application/xml"> are <m:properties>
            #   the children of <m:properties> are what we want

            # so we get the children of the children
            #    of the content element of the entry node
            properties <- XML::xmlChildren( XML::xmlChildren(entry)[['content']] )

            # build up the temporary 'elements' data.frame by adding rows to the bottom
            if (is.null(elements)) {
                elements <- XML::xmlToDataFrame(properties,
                                                stringsAsFactors = FALSE)
            } else {
                elements <- dplyr::bind_rows(elements,
                                             XML::xmlToDataFrame(properties,
                                                                 stringsAsFactors = FALSE))
            }
        }

        # convert NEW_DATE field to Date format
        elements <- transform(elements, NEW_DATE = as.Date(elements$NEW_DATE))

        # transform chr into numeric
        elements <- as.data.frame(lapply(X   = elements,
                                         FUN = function(x) {
                                             if (class(x) == "Date") {
                                                 return(as.Date(x))
                                             } else {
                                                 return(as.numeric(x))
                                             }
                                         } ))

        # ==============================================================================
        # add the temporary 'elements' data.frame to the bottom of FRB_H15_1962_2015_mod
        # ==============================================================================
        FRB_H15_1962_2015_mod <- dplyr::bind_rows(FRB_H15_1962_2015_mod,
                                                  elements)
    }

    # ============================================================
    # sort the FRB_H15_1962_2015_mod data.frame
    #   add an attribute so we can identify it in other functions
    #      and return it
    # ============================================================
    FRB_H15_1962_2015_mod <- dplyr::arrange(FRB_H15_1962_2015_mod, NEW_DATE)
    attr(FRB_H15_1962_2015_mod, "data.source") <- "CMT"
    return(FRB_H15_1962_2015_mod)
}

#' Return treasury bond price data from 2010
#'
#' @description The FedInvest site provides rate and price data on a daily basis
#' for approximately 400 Treasury securites. This function returns all that data
#' from 2010 to the last-completed business day.
#'
#' @return data.frame containing the FedInvest data from 2010 to the most-recently
#' completed business day
#'
#' @details The columns of the data.frame returned
#' \itemize{
#'     \item \bold{CUSIP}  Committee on Uniform Security Identification
#'     Procedures' nine-character, alphanumeric security identification code
#'     \item \bold{SECURITY.TYPE}
#'        \itemize{
#'            \item MARKET BASED BILL
#'            \item MARKET BASED NOTE
#'            \item MARKET BASED BOND
#'            \item TIPS
#'            \item MARKET BASED FRN
#'            }
#'     \item \bold{RATE}
#'     \item \bold{MATURITY.DATE}
#'     \item \bold{CALL.DATE} All \code{NA}
#'     \item \bold{BUY}
#'     \item \bold{SELL}
#'     \item \bold{END.OF.DAY}
#'     \item \bold{Date} Date for which the data was retrieved
#'     }
#'
#'
#' @author
#' George Fisher
#'
#' @references
#' US Dept. of the Treasury FedInvest Historical Prices
#' \url{https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate.htm}
#'
#' @examples
#' library(ustreasuries)
#' fedinvest_data <- FedInvestData()
#' head(fedinvest_data)
#' tail(fedinvest_data)
#'
#'
#' @export
FedInvestData <- function() {

    # =================================
    # Load the FedInvest data from 2010
    # by loading a static data file
    # =================================
    if (file.exists("R/sysdata.rda"))
        load(file="R/sysdata.rda")
    if (file.exists("sysdata.rda"))
        load(file="sysdata.rda")

    # get rid of the superfluous CMT file
    #if (exists("FRB_H15_1962_2015_mod")) rm("FRB_H15_1962_2015_mod")

    # the FedInvest history page
    post_url <- "https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate.htm"

    # retrieve all the forms on this page
    forms    <- rvest::html_form(xml2::read_html(post_url))

    # this data.frame will accumulate the data
    df       <- data.frame(CUSIP         = character(),
                           SECURITY.TYPE = character(),
                           RATE          = character(),
                           MATURITY.DATE = character(),
                           CALL.DATE     = character(),
                           BUY           = numeric(),
                           SELL          = numeric(),
                           END.OF.DAY    = numeric(),
                           DATE          = as.Date(character()),
                           stringsAsFactors=FALSE)

    # this is the list of dates to iterate over
    # from the date after the history ended
    #    to today
    last_date <- FedInvest_historical_data$Date[nrow(FedInvest_historical_data)]+1
    if (last_date >= Sys.Date()) return(FedInvest_historical_data)

    date_seq  <- seq(as.Date(last_date, "%Y/%m/%d"),
                     as.Date(format(Sys.Date(), "%Y/%m/%d")), "days")

    # for each date in date_seq,
    #   fill the form
    #     submit it and retrieve the data
    #        append to 'df'
    for (dt in date_seq) {

        dte <- as.Date(dt, origin = "1970-01-01")
        if (weekdays(dte) %in% c("Saturday","Sunday")) next

        # ---------------------- fill the form ----------------------
        yr <- format(dte,"%Y")
        mo <- format(dte,"%m")
        dy <- format(dte,"%d")

        this_date <- as.Date(paste0(mo,"/",dy,"/",yr),"%m/%d/%Y")

        values    <- rvest::set_values(forms[[2]],
                                       priceDate.month = mo,
                                       priceDate.day   = dy,
                                       priceDate.year  = yr)
        # ---------------------- fill the form ----------------------

        # submit the form and retrieve the data
        submit <- rvest::submit_form(session = rvest::html_session(post_url),
                                     form    = values)
        prices <- data.frame(rvest::html_table(submit))

        # a normal response has 8 columns
        # errors are reported via a 3-column empty respose
        if (ncol(prices) != 8) next

        # add a field to show the date of the data
        #   append to 'df'
        prices$Date <- this_date
        df          <- rbind(df, prices)
    }

    # batch convert the MATURITY.DATE to date format
    df$MATURITY.DATE <- as.Date(df$MATURITY.DATE, "%m/%d/%Y")

    # ==============================================================================
    # add the temporary 'df' data.frame to the bottom of FedInvest_historical_data
    # ==============================================================================
    FedInvest_historical_data <- dplyr::bind_rows(FedInvest_historical_data,
                                                  df)
    # ====================================================
    # add the attribute so we know what kind of data it is
    # ====================================================
    attr(FedInvest_historical_data, "data.source") <- "FedInvest"

    return(FedInvest_historical_data)
}

#' Daily S&P 500 data from Jan 3, 1950 to present
#'
#' @description
#' The "usual" S&P 500 index, without dividends reinvested.
#'
#' @return A data.frame with the daily data
#'
#' @references
#' Yahoo Finance
#'
#' @details The columns of the data.frame returned
#' \itemize{
#'     \item \bold{Date}
#'     \item \bold{Open}
#'     \item \bold{High}
#'     \item \bold{Low}
#'     \item \bold{Close}
#'     \item \bold{Volume}
#'     \item \bold{Adj.Close}
#'     }
#'
#' @importFrom utils read.table
#'
#' @author George Fisher
#'
#' @examples
#' sp500_idx <- SP500()
#' head(sp500_idx)
#' tail(sp500_idx)
#'
#' @export
SP500 <- function() {
    table <- read.table("http://ichart.finance.yahoo.com/table.csv?s=%5EGSPC",
                        header = TRUE,sep=",",stringsAsFactors = FALSE)
    table$Date <- as.Date(table$Date, "%Y-%m-%d")
    return(table)
}

#' Daily S&P 500 Total Return data from Jan 4, 1988 to present
#'
#' @description
#' The S&P 500 index, with dividends reinvested.
#'
#' @return A data.frame with the daily data
#'
#' @references
#' Yahoo Finance
#'
#' @details The columns of the data.frame returned
#' \itemize{
#'     \item \bold{Date}
#'     \item \bold{Open}
#'     \item \bold{High}
#'     \item \bold{Low}
#'     \item \bold{Close}
#'     \item \bold{Volume}
#'     \item \bold{Adj.Close}
#'     }
#'
#' @author George Fisher
#'
#' @importFrom utils read.table
#'
#' @examples
#' sp500_idx_tr <- SP500TR()
#' head(sp500_idx_tr)
#' tail(sp500_idx_tr)
#'
#' @export
SP500TR <- function() {
    table <- read.table("http://ichart.finance.yahoo.com/table.csv?s=%5ESP500TR",
                        header = TRUE,sep=",",stringsAsFactors = FALSE)
    table$Date <- as.Date(table$Date, "%Y-%m-%d")
    return(table)
}

#' Coerce a slice of FedInvest data to xts
#'
#' @description
#' The FedInvest data contains one row per bond on a particular date.
#' This function converts that format into an xts frame.
#'
#' @details
#' The FedInvest data has one row per date, per bond. For analyses such as
#' Nelson Seigel and Svensson we need the data in an xts frame with the format
#' of one row per date and one column per maturity.
#'
#' This function performs this primary transformation: The bond data has a
#' continuous set of maturities; a fixed xts table needs discrete maturities.
#' So, for example, the 1-year maturity data comprises the average YTM for all
#' the bonds with maturities between 0.5 and 1.5 years.
#'
#' A number of minor transformations are also performed such as converting
#' the RATE data from a character percent format (7.82\%) to decimal (0.0782)
#'
#' @importFrom magrittr %>%
#' @importFrom stats complete.cases
#'
#' @author George Fisher
#'
#' @param bond_data a slice of the FedInvest data
#' @return a list
#' \itemize{
#'      \item \bold{$rate_table_xts} the xts frame
#'      \item \bold{$unique_maturities} a numeric vector of the maturities
#'      for which there is a column in the xts frame in fractional-year format
#'      (1-month = 1/12 = 0.08333333). It should be noted that if bonds in a
#'      matruity range of say 15.5-16.5 are not found in the data, there will
#'      not be a 16.00000 maturity column in the xts frame.
#'      \item \bold{$modified_data} the original data with additional columns
#'          \itemize{
#'              \item \bold{fracyrs} fractional-year time to maturity
#'              \item \bold{maturities} the regularized fractional-year time to maturity
#'              \item \bold{coupon} numeric coupon from percent-format RATE
#'              \item \bold{mid} the mid-point between BUY & SELL
#'              \item \bold{YTM} yield to maturity
#'          }
#' }
#'
#' @seealso
#' \code{\link{FedInvestData}} the function to download the FedInvest data
#'
#' @importFrom YieldCurve Nelson.Siegel
#' @importFrom YieldCurve NSrates
#' @importFrom YieldCurve Svensson
#' @importFrom YieldCurve Srates
#'
#' @examples
#' library(ustreasuries)
#' library(magrittr)
#'
#' start.date <- Sys.Date() - 25
#' end.date   <- Sys.Date()
#'
#' rate_table_list <- dplyr::filter(FedInvestData(),
#'                                   Date >= start.date &
#'                                   Date <= end.date   &
#'                                   SECURITY.TYPE != "TIPS") %>%
#'     CoerceFedInvest_xts()
#'
#' rate_table_xts    <- rate_table_list$rate_table_xts
#' unique_maturities <- rate_table_list$unique_maturities
#' modified_data     <- rate_table_list$modified_data
#'
#' unique_maturities
#'
#' knitr::kable(as.data.frame(modified_data[30:32, c(2:4,10:14)]),
#' caption="Sample of FedInvest Data with additional Data",
#' row.names=FALSE)
#'
#' knitr::kable(as.data.frame(rate_table_xts[1:3, 1:5]),
#' caption="Sample of FedInvest Data after Coersion to xts Format",
#' row.names=TRUE)
#'
#' @export
CoerceFedInvest_xts <- function(bond_data) {
    # supresses
    # Note: no visible binding for global variable ...
    assign("Date", NULL)
    assign("MATURITY.DATE", NULL)
    assign("coupon", NULL)
    assign("END.OF.DAY", NULL)
    assign("ytm", NULL)

    # find the fractional-year time to maturity for each bond
    bond_data$fracyrs <- as.numeric((bond_data$MATURITY.DATE - bond_data$Date) / 365)

    # the bonds have maturities all over the map, let's regularize them
    # 1-month, 3-months, 6-months, years 1-30
    # ===============================================================
    mo1 = c(0,                   1/12+(3/12-1/12)/2)
    mo3 = c(1/12+(3/12-1/12)/2,  3/12+(6/12-3/12)/2)
    mo6 = c(3/12+(6/12-3/12)/2,  6/12+(12/12-6/12)/2)
    yr1 = c(6/12+(12/12-6/12)/2, 1+(2-1)/2)
    breaks = list(mo0.08333333=mo1, mo0.25=mo3, mo0.5=mo6, yr1=yr1)
    for (i in seq(2,30)) {
        breaks[[i+3]] <- c((i-1) + ((i)-(i-1))/2, (i) + ((i+1)-(i))/2)
    }
    names(breaks) <- c("mo0.08333333", "mo0.25", "mo0.5", "yr1", paste0("yr",seq(2,30)))

    maturities = unlist(sapply(bond_data$fracyrs, function(x) {
        for (i in 1:length(breaks)) {
            if (x >= breaks[[i]][1] & x <= breaks[[i]][2]) {
                return(as.numeric(substr(names(breaks[i]),3,nchar(names(breaks[i])))))
            }
        }
    }))
    bond_data$maturities <- maturities
    # ===============================================================

    # create fractional coupons from percent-format RATE
    # ======================================================
    matches <- regexpr("(\\d+\\.\\d+)%", bond_data$RATE, perl=TRUE);
    result  <- attr(matches, "capture.start")[,1]
    attr(result, "match.length") <- attr(matches, "capture.length")[,1]
    bond_data$coupon <- as.numeric(regmatches(bond_data$RATE, result))/100

    # find the mid-point between BUY & SELL
    # =====================================
    bond_data$mid <- apply(bond_data, 1, function(x) {
                                            buy       <- as.numeric(x["BUY"])
                                            sell      <- as.numeric(x["SELL"])
                                            lower     <- min(buy, sell)
                                            half_diff <- abs(buy - sell) / 2
                                            mid       <- lower + half_diff
                                            return(mid)
                                        })

    # find the YTM for each bond
    # ==========================
    bond_data$YTM <- apply(bond_data, 1, function(x) {
                    Date   <- as.Date(x["Date"], origin="1970-01-01")
                    mature <- as.Date(x["MATURITY.DATE"], origin="1970-01-01")
                    coupon <- as.numeric(x["coupon"])

                    # don't know which is better, mid or eod
                    # but
                    #     eod sometimes zero
                    #     mid sometimes gives weird answers
                    price  <- as.numeric(x["mid"])
                    eod    <- as.numeric(x["END.OF.DAY"])
                    price  <- eod

                    if (grepl("BILL", x["SECURITY.TYPE"], perl=TRUE)) {
                        tryCatch({
                            jrvFinance::bond.yields(settle=Date,
                                                    mature=mature,
                                                    coupon=coupon,
                                                    price=price,
                                                    convention = "ACT/360")
                            },
                            error   = function(e) {NA},
                            warning = function(e) {NA}
                        )
                    } else {
                        tryCatch({
                            jrvFinance::bond.yields(settle=Date,
                                                    mature=mature,
                                                    coupon=coupon,
                                                    price=price,
                                                    convention = "ACT/ACT")
                            },
                            error   = function(e) {NA},
                            warning = function(e) {NA}
                        )
                    }
                })

    # for each set of bonds
    #   on a date
    #     with the same regularized maturity
    # find the average YTM in percentage format
    #   and create one row per date, one column per maturity
    # ===========================================
    rate_table <- bond_data %>%
        dplyr::group_by(Date, maturities) %>%
        dplyr::summarise(ytm=mean(YTM))   %>%
        tidyr::spread(maturities, ytm)

    # if Newton-Raphson and Bisection failed  to find IRR/YTM
    # ... remove the rows
    rate_table <- rate_table[complete.cases(rate_table),]

    # convert to xts format
    rate_table_xts <- xts::xts(rate_table[,-c(1)], order.by = rate_table$Date)

    # get the vector of regularized maturities
    unique_maturities <- as.numeric(names(rate_table_xts))

    return(list(rate_table_xts    = rate_table_xts,
                unique_maturities = unique_maturities,
                modified_data     = bond_data))
}

#' Convert rates to discount factors Z(0, T)
#'
#' @description Returns the discount factors associated with Nelson Seigel fitted rates
#'
#' @details \eqn{Z(0, T) = e^{-r(0, T) * T}}
#'
#' @param NS_rates the value returned by the \code{NSrates()} or \code{Srates()}functions
#' @return \code{data.frame} rownames are the dates,
#' colnames are the time to maturity in character format preceded by "X"
#'
#' @author
#' George Fisher
#'
#' @references
#' Veronesi, P., \emph{Fixed Income Securities} p67
#'
#'
#' @export
NSzeros <- function(NS_rates) {
    # function to generate Z(0, T) = exp(-r(0, T) * T)
    # see p 67 of Veronesi
    foo <- function(r) {
        # extract T from the column name
        T <- unname(sapply(names(r),
                           function(x) unname(as.numeric(substr(x,2,nchar(x))))))

        # NOTE: r(0, T) is divided by 100 '(r/100)'
        #       because the data is in "percent" format
        #       7.82 means 7.82%, 0.0782
        return(exp(-(r/100) * T))
    }
    rates <- t(apply(NS_rates, 1, foo))
    return(rates)
}
