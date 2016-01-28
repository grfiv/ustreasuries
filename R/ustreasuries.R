#' US Treasury bond analysis
#'
#' The \emph{ustreasuries} package downloads daily Constant-Maturity Treasury
#' (CMT) yields and provides visualizations & analytics that use that data
#' including all the 'greeks' for derivative analysis.
#'
#' @section Vignettes:
#' \itemize{
#'     \item \bold{cmt-rates} a description of Constant Maturity and
#'     Annualized Percentage rates
#'     \item \bold{yield-curves} examples of downloading the data and printing
#'     yield curves for interesting periods in recent financial history.
#'     \item \bold{plot-10year} a plot of the 10-year from 1962 to present
#' }
#'
#' @section Functions:
#' \itemize{
#'     \item \bold{USTreasuryRates} downloads a data.frame with daily data from 1962
#'     \item \bold{PrintYieldCurves} prints one or more yield curves
#'     \item \bold{APY} converts Constant-Maturity Treasury (CMT) yields to
#'     Annualized Percentage Yields (APY)
#' }
#'
#' @section Installation:
#'     \code{install.packages("devtools")}
#'
#'     \code{devtools::install_github("grfiv/ustreasuries", build_vignettes=TRUE)}
#'
#'     (restart R)
#'
#'
#' @docType package
#' @name ustreasuries
NULL
