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
