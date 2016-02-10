#' US Treasury bond analysis
#'
#' The \emph{ustreasuries} package downloads daily Constant-Maturity Treasury
#' (CMT) yields and provides visualizations & analytics that use that data
#' including all the 'greeks' for derivative analysis.
#'
#' @section GitHub Wiki:
#' \url{https://github.com/grfiv/ustreasuries/wiki}
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
#' \item \bold{Treasury Rate Data}
#'     \itemize{
#'        \item \bold{CMTrates} downloads real-time daily CMT data from 1962
#'        \itemize{
#'            \item \bold{PrintYieldCurves} prints one or more yield curves
#'            \item \bold{APY} converts Constant-Maturity Treasury (CMT) yields to
#'     Annualized Percentage Yields (APY)
#'     }
#'     \item \bold{FedInvestData} downloads real-time treasury bond price data from 2010
#'    }
#'
#'  \item \bold{Black Scholes Merton}
#'      \itemize{
#'          \item \bold{EuroCall} Calculate the price of a European call option with or without dividends
#'          \item \bold{EuroPut} Calculate the price of a European put option with or without dividends
#'          \item \bold{EuroCallVol} Implied Volatility for a European call option
#'          \item \bold{EuroPutlVol} Implied Volatility for a European put option
#'      }
#'
#'  \item \bold{Greeks}
#'      \itemize{
#'          \item \bold{DeltaCall} Amount call-option price changes for a change in asset price
#'          \item \bold{DeltaPut} Amount put-option price changes for a change in asset price
#'          \item \bold{ThetaCall} the decay in the value of a call or a portfolio of calls as time passes
#'          \item \bold{ThetaPut} the decay in the value of a put or a portfolio of puts as time passes
#'          \item \bold{Gamma} the change in Delta with respect to asset price
#'          \item \bold{Vega} the sensitivity to changes in the volatility of the underlying
#'          \item \bold{RhoCall} the sensitivity to changes in the risk-free rate of return
#'          \item \bold{RhoPut} the sensitivity to changes in the risk-free rate of return
#'      }
#'
#'  \item \bold{Utility Functions}
#'  \itemize{
#'          \item \bold{Bonds}
#'              \itemize{
#'                  \item \bold{Nelson.Siegel} Estimation of the Nelson-Siegel parameters
#'                  \item \bold{NSrates} Interest rates of the Nelson-Siegel model
#'                  \item \bold{NSzeros} Convert NSrates to discount factors Z(0, T)
#'                  \item \bold{Nelson.Siegel.plot} Plot the actual and Nelson.Siegel Yield Curves
#'                  \item \bold{NSzeros.plot} Plot Z(0, T) and r(0, T)
#'              }
#'      }
#'      \itemize{
#'          \item \bold{CAGR}
#'              \itemize{
#'                  \item \bold{CAGR} Calculate Compound Annual Growth Rate; geometric or continuous
#'                  \item \bold{r_continuous} Convert from discrete to continuous
#'                  \item \bold{r_discrete} Convert from continuous to discrete
#'              }
#'           \item \bold{Discount Factors}
#'              \itemize{
#'                  \item \bold{discount_factor} Calculate discount factor Z(t, T)
#'                  \item \bold{interest_rate} Calculate annualized interest rate r(t, T) from a discount factor Z(t, T)
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
#'          \item \bold{Options}
#'              \itemize{
#'                  \item \bold{IntrinsicValueCall} The in-the-money portion of a call option's premium
#'                  \item \bold{IntrinsicValuePut} The in-the-money portion of a put option's premium
#'                  \item \bold{TimeValueCall} Price = Intrinsic + Time
#'                  \item \bold{TimeValuePut} Price = Intrinsic + Time
#'                  \item \bold{InTheMoneyCall} Is a call in the money?
#'                  \item \bold{InTheMoneyPut} Is a put in the money?
#'              }
#'      }
#'
#'  \item \bold{Installed but not yet tested and documented}
#'      \itemize{
#'          \item Digital
#'          \itemize{
#'              \item \bold{CashCall}
#'              \item \bold{CashPut}
#'              \item \bold{AssetCall}
#'              \item \bold{AssetPut}
#'            }
#'          \item Greeks
#'          \itemize{
#'              \item \bold{RhoFuturesCall}
#'              \item \bold{RhoFuturesPut}
#'              \item \bold{RhoFXCall}
#'              \item \bold{RhoFXPut}
#'          }
#'          \item American
#'          \itemize{
#'              \item \bold{American_Put_Binomial}
#'              \item \bold{American_Call_Dividend}
#'          }
#'      }
#' }
#'
#' @section Installation:
#'     \code{## Windows users STOP: see https://github.com/hadley/devtools}
#'
#'     \code{install.packages("devtools") }
#'
#'     \code{devtools::install_github("grfiv/ustreasuries",
#'                                    build_vignettes=TRUE)}
#'
#' \code{## NOTE: If you get a 404 on installation, contact the author for the 'auth_token' parameter}
#'
#'     (restart R)
#'
#' @examples
#' # example of US Treasury rate data download
#' # =========================================
#' all_data <- CMTrates()
#' tail(all_data)
#' # ==================================================
#' # last row displayed should be for last business day
#' # ==================================================
#'
#' # example of option pricing
#' # =========================
#' # Hull 7th edition Ch 13 P 294
#' Stock     <- 42    # S_0
#' Exercise  <- 40    # K
#' Time      <- 0.50  # T
#' Interest  <- 0.10  # r
#' Yield     <- 0     # q
#' sigma     <- 0.20
#' ans <- EuroCall(Stock, Exercise, Time, Interest, Yield, sigma)
#' round(ans,2) # 4.76
#'
#' @references
#' Back, K., \emph{A Course in Derivative Securities} 2005 Springer Finance ISBN 9783540253734
#'
#' Consiglio A. and Guirreri S.S. \emph{Simulating the Term Structure of Interest Rates with
#' arbitrary marginals}. International Journal of Risk Assessment and Management, 15(4),
#' September 2011.
#'
#' Hull, J., \emph{Options, Futures and Other Derivatives Seventh Edition} 2008 Pearson/Prentice Hall ISBN 9780136015864
#'
#' US Dept. of Treasury Daily Treasury Yield Curve Rates
#' \url{https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield}
#'
#' US Federal Reserve Board Data Download Program
#' \url{http://www.federalreserve.gov/datadownload/Choose.aspx?rel=H15}
#'
#' Varma, J. \emph{jrvFinance} v1.03
#' \url{https://github.com/jrvarma/jrvFinance}
#'
#' Veronesi P., \emph{Fixed Income Securities} 2010 John Wiley & Sons ISBN 9780470109106
#'
#' Wall Street Journal Market Data Center
#' \url{http://online.wsj.com/mdc/public/page/mdc_bonds.html?mod=mdc_topnav_2_3010}
#'
#' @author
#' George Fisher \email{GeorgeRFisher@gmail.com}
#'
#' Pratik Biswas \email{pratik.k.biswas@gmail.com}
#'
#' @docType package
#' @name ustreasuries
NULL
