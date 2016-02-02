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
#'     \item \bold{utilities} examples of the utility functions
#'     \item \bold{black-scholes-merton} examples of options pricing
#' }
#'
#' @section Functions:
#' \itemize{
#'
#' \item \bold{Treasury Rates}
#'     \itemize{
#'        \item \bold{USTreasuryRates} downloads a data.frame with daily rates from 1962
#'        \item \bold{PrintYieldCurves} prints one or more yield curves
#'        \item \bold{APY} converts Constant-Maturity Treasury (CMT) yields to
#'     Annualized Percentage Yields (APY)
#'    }
#'
#'  \item \bold{Black Scholes Merton}
#'      \itemize{
#'          \item \bold{EuroCall} Calculate the price of a European call option with or without dividends
#'          \item \bold{EuroPut} Calculate the price of a European put option with or without dividends
#'      }
#'
#'  \item \bold{Utility Functions}
#'      \itemize{
#'          \item \bold{CAGR}
#'              \itemize{
#'                  \item \bold{CAGRd} Calculate discrete Compound Annual Growth Rate
#'                  \item \bold{r_continuous} Convert from discrete to continuous CAGR
#'                  \item \bold{r_discrete} Convert from continuous to discrete CAGR
#'              }
#'          \item \bold{Put/Call Parity}
#'          \itemize{
#'              \item \bold{CallParity} Convert from a put-option price using put/call parity
#'              \item \bold{PutParity} Convert from a call-option price using put/call parity
#'              }
#'          \item \bold{Risk Neutral/Forwards}
#'              \itemize{
#'                  \item \bold{RiskNeutralProb} Binomial tree risk-neutral probability
#'                  \item \bold{ForwardPrice} Forward price with or without income or yield
#'                  \item \bold{ForwardRate} Forward rate from Time1 to Time2 (discrete compounding)
#'              }
#'      }
#'
#'  \item \bold{Installed but not yet undocumented}
#'      \itemize{
#'          \item \bold{CashCall}
#'          \item \bold{CashPut}
#'          \item \bold{AssetCall}
#'          \item \bold{AssetPut}
#'          \item \bold{American_Put_Binomial}
#'          \item \bold{DeltaCall}
#'          \item \bold{DeltaPut}
#'          \item \bold{OptionGamma}
#'          \item \bold{ThetaCall}
#'          \item \bold{ThetaPut}
#'          \item \bold{Vega}
#'          \item \bold{RhoFuturesCall}
#'          \item \bold{RhoFuturesPut}
#'          \item \bold{RhoFXCall}
#'          \item \bold{RhoFXPut}
#'          \item \bold{RhoCall}
#'          \item \bold{RhoPut}
#'          \item \bold{EuroCallVol}
#'          \item \bold{EuroPutlVol}
#'          \item \bold{Black_Scholes_Call_Implied_Vo}l
#'          \item \bold{Black_Scholes_Put_Implied_Vol}
#'          \item \bold{ImpVolCall}
#'          \item \bold{ImpVolPut}
#'          \item \bold{American_Call_Dividend}
#'
#'
#'      }
#' }
#'
#' @section Installation:
#'     \code{# Windows users STOP: see https://github.com/hadley/devtools}
#'
#'     \code{install.packages("devtools") }
#'
#'     \code{devtools::install_github("grfiv/ustreasuries",
#'                                    build_vignettes=TRUE,
#'                                    ,auth_token="088a25903f767320fa0a5458a1a71a20e0668611")}
#'
#'     (restart R)
#'
#'
#' @docType package
#' @name ustreasuries
NULL
