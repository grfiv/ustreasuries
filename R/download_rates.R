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
#'   most-recently completed business day.
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

    # ====================================================
    # sort and return the FRB_H15_1962_2015_mod data.frame
    # ====================================================
    FRB_H15_1962_2015_mod <- dplyr::arrange(FRB_H15_1962_2015_mod, NEW_DATE)
    return(FRB_H15_1962_2015_mod)
}

