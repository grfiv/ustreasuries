#' Convert CMT to APY
#'
#' Convert Constant-Maturity Treasury (CMT) yields to
#' Annualized Percentage Yields (APY)
#'
#' @param i vector of CMT yields to be converted
#' @param percent boolean, if TRUE, i should be divided by 100
#'                  (for example 7.87 becomes 0.0787)
#' @return APY yield(s) as a decimal (not as a percent)
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
