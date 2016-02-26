#' Print yield curves
#'
#' Given a data.frame returned by the \emph{CMTrates}, function, you can
#' specify one or more dates to use to print the yield curve on those dates.
#'
#' @param YieldCurveDF data.frame created by the \emph{CMTrates} function
#' @param rows a vector of integer row indices indicating the row(s) to use
#' for plotting
#' @param title a string to use as the title of the plot
#' @param tabprt if TRUE, print a table of the rate data used
#' @return nothing: produces a plot and optionally a table
#'
#' @author George Fisher \email{GeorgeRFisher@gmail.com}
#'
#'
#' @examples
#' library(ustreasuries)
#' all_data <- CMTrates()
#' PrintYieldCurves(dplyr::filter(all_data,
#'                                    all_data$NEW_DATE>=as.Date("2006-01-01") &
#'                                    all_data$NEW_DATE<=as.Date("2009-12-31")),
#'                    rows=c(1, 272, 272*2, 272*3),
#'                    title="Yield Curves Before & During the Financial Crisis",
#'                    tabprt=TRUE)
#'
#' @importFrom graphics axTicks
#' @importFrom graphics axis
#' @importFrom graphics grid
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom graphics text
#'
#' @export
PrintYieldCurves <- function(YieldCurveDF, rows=c(1), title="Yield Curve", tabprt=FALSE) {

    slicedDF <- dplyr::slice(YieldCurveDF, rows)

    # find the data points for y-axis plotting ylim = c(y_min, y_max)
    y_min    <- min(slicedDF[,3:13], na.rm=TRUE)
    y_max    <- max(slicedDF[,3:13], na.rm=TRUE)

    # ==================
    # plot the first row
    # ==================
    plot(x = 1:11,
         y = slicedDF[1, 3:13],
         type="b",   pch=10, col=1,
         ylim=c(y_min*.90, y_max*1.10), ylab="Interest Rate",
         xaxt="n", xlab='',
         main=title)
    axis(1, at     = axTicks(1),
         labels = substr(names(slicedDF[1, 3:13])[axTicks(1)],4,9))
    grid()
    text(x      = 9,
         y      = slicedDF[1, "BC_10YEAR"],
         pos    = 4,
         labels = format(slicedDF[1, "NEW_DATE"], "%Y-%m-%d"),
         cex    = 0.75, col = 1)

    # ========================
    # plot all subsequent rows
    # ========================
    if (length(rows) > 1) {
        for (i in 2:length(rows)) {
            lines(x   = 1:11,
                  y   = slicedDF[i, 3:13],
                  col = i)
            points(x   = 1:11,
                   y   = slicedDF[i, 3:13],
                   col = i, pch = 10)
            text(x      = 9,
                 y      = slicedDF[i, "BC_10YEAR"],
                 pos    = 4,
                 labels = format(slicedDF[i, "NEW_DATE"], "%Y-%m-%d"),
                 cex    = 0.75, col = i)
        }
        legend("bottomright",
               legend = format(slicedDF$NEW_DATE, "%a %b %d, %Y"),
               lty    = rep(1, length(rows)),
               lwd    = rep(1, length(rows)),
               col    = 1:length(rows),
               cex    = 0.75)
    }
    if (tabprt) {
        print(knitr::kable(slicedDF[, 2:8]))
        print(knitr::kable(slicedDF[, c(2, 9:(ncol(slicedDF)-1))]))
    }
}
