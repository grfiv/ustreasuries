# Create the FedInvest history data
# =================================

# =============================================================
loaded_file_names <- load(file="../R/sysdata.rda")

print("LIST OF LOADED FILES")
print(loaded_file_names)

rm(FedInvest_historical_data)
# =============================================================

FedInvestHistory <- function() {

    # this function downloads the history of US Treasury FedInvest
    # bond prices for the purpose of creating a static file so that
    # the exported function to download this data in real time does
    # not have to do very much work

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

    # ===============================================================
    # ---------- this is the list of dates to iterate over ----------
    date_seq = seq(as.Date("2010/01/01", "%Y/%m/%d"),
    #date_seq = seq(as.Date(format(Sys.Date()-10, "%Y/%m/%d")),
                   as.Date(format(Sys.Date()-1, "%Y/%m/%d")), "days")
    # ===============================================================

    print(paste("#### start the real-time load with date", Sys.Date(), "####"))

    # for each date in date_seq,
    #   fill the form
    #     submit it and retrieve the data
    #        append to 'df'
    for (dt in date_seq) {

        dte <- as.Date(dt, origin = "1970-01-01")
        if (weekdays(dte) %in% c("Saturday","Sunday")) next

        # ----------- fill the form -----------
        yr <- format(dte,"%Y")
        mo <- format(dte,"%m")
        dy <- format(dte,"%d")

        this_date <- as.Date(paste0(mo,"/",dy,"/",yr),"%m/%d/%Y")

        values <- rvest::set_values(forms[[2]],
                                    priceDate.month = mo,
                                    priceDate.day   = dy,
                                    priceDate.year  = yr)
        # ----------- fill the form -----------

        # submit the form and retrieve the data
        submit <- rvest::submit_form(session=rvest::html_session(post_url),form=values)
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

    return(df)
}
FedInvest_historical_data = FedInvestHistory()

backup <- FedInvest_historical_data


# ----------------------------------------------------------------
#
# ----------------------------------------------------------------

# NOTE: make sure you include all saved internal files
# ====================================================
devtools::use_data(FRB_H15_1962_2015_mod, FedInvest_historical_data,
                   internal = TRUE, overwrite = TRUE, compress = "bzip2")

# determine the best compression scheme
# use in 'compress' parameter of
tools::checkRdaFiles("~/Dropbox/Stanford/STATS290/ustreasuries/R")
