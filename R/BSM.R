## ====================================================
## Black Scholes Merton for R (from Python)
## Written by George Fisher
## see https://github.com/grfiv/BlackScholesMerton
##
## Translated by Pratik K. Biswas on 1/25/2015
## ====================================================

## ===================================
##       Black Scholes Merton
## ===================================


#' The d1 component of the Black-Scholes-Merton formula
#' @param Stock S0, the initial stock price
#' @param Exercise K, the strike price
#' @param Time T, the time to maturity in fractional years
#' @param Interest r, the risk-free rate of return
#' @param Yield q, the dividend yield
#' @param sigma the asset volatility
#' @return d1
#' @references
#' Hull 7th edition Ch 13 P 291
#'
#' @examples
#' # Hull 7th edition Ch 13 P 294
#' Stock     <- 42
#' Exercise  <- 40
#' Time      <- 0.50
#' Interest  <- 0.10
#' Yield     <- 0
#' sigma     <- 0.20
#' ans <- dOne(Stock, Exercise, Time, Interest, Yield, sigma)
#' round(ans,4)
#'
#' @export
dOne <- function(Stock, Exercise, Time, Interest, Yield, sigma) {
    return ((log(Stock / Exercise) + (Interest - Yield + 0.5 * sigma * sigma) * Time) / (sigma * sqrt(Time)))
}

#' The d2 component of the Black-Scholes-Merton formula
#' @inheritParams dOne
#' @return d1
#'
#' @examples
#' # Hull 7th edition Ch 13 P 294
#' Stock     <- 42
#' Exercise  <- 40
#' Time      <- 0.50
#' Interest  <- 0.10
#' Yield     <- 0
#' sigma     <- 0.20
#' ans <- dTwo(Stock, Exercise, Time, Interest, Yield, sigma)
#' round(ans,4)
#'
#' @export
dTwo <- function(Stock, Exercise, Time, Interest, Yield, sigma) {
    return(dOne(Stock, Exercise, Time, Interest, Yield, sigma) -
               (sigma * sqrt(Time)))
}

#
# Binary Options
#

# Digital: Cash or Nothing

CashCall <- function(Stock, Exercise, Time, Interest, Yield, sigma) {

    if (Time < 0.000000005)
        if (Stock >= Exercise)
            return (1)
    else
        return (0)


    if (sigma == 0) sigma = 0.0000000001

    d2_ <- dTwo(Stock, Exercise, Time, Interest, Yield, sigma)
    Nd2 <- phi(d2_)
    return (exp(-Interest * Time) * Nd2)
}

CashPut <- function(Stock, Exercise, Time, Interest, Yield, sigma) {

    if (Time < 0.000000005)
        if (Stock >= Exercise)
            return (0)
    else
        return (1)

    if (sigma == 0) sigma = 0.0000000001

    d2_ <- dTwo(Stock, Exercise, Time, Interest, Yield, sigma)
    Nminusd2 = phi(-d2_)

    return (exp(-Interest * Time) * Nminusd2)
}

## Asset or Nothing

AssetCall <- function (Stock, Exercise, Time, Interest, Yield, sigma) {

    if (Time < 0.000000005)
        if (Stock >= Exercise)
            return (Stock)
    else
        return (0)

    if (sigma == 0) sigma = 0.0000000001

    if (Exercise < 0.000000005) Nd1 = 1
    else {
        d1_ = dOne(Stock, Exercise, Time, Interest, Yield, sigma)
        Nd1 = phi(d1_)
    }

    return (Stock * exp(-Yield * Time) * Nd1)
}

AssetPut <- function(Stock, Exercise, Time, Interest, Yield, sigma) {

    if (Time < 0.000000005)
        if (Stock >= Exercise)
            return (0)
    else
        return (Stock)

    if (sigma == 0) sigma = 0.0000000001

    d1_ = dOne(Stock, Exercise, Time, Interest, Yield, sigma)
    Nminusd1 = phi(-d1_)

    return (Stock * exp(-Yield * Time) * Nminusd1)
}


##
## European Call and Put
## ---------------------
##

#' European Call
#'
#' A call option that can only be exercised at expiration
#'
#' @inheritParams dOne
#' @return the price of a European Call Option
#' @references
#' Hull 7th edition Ch 13 P 291
#'
#' @examples
#' # Hull 7th edition Ch 13 P 294
#' Stock     <- 42
#' Exercise  <- 40
#' Time      <- 0.50
#' Interest  <- 0.10
#' Yield     <- 0
#' sigma     <- 0.20
#' ans <- EuroCall(Stock, Exercise, Time, Interest, Yield, sigma)
#' round(ans,2)
#'
#' @export
EuroCall <- function(Stock, Exercise, Time, Interest, Yield, sigma) {

    if (Time == 0) return (max(0, Stock - Exercise))

    if (sigma == 0) return (max(0, exp(-Yield * Time) * Stock - exp(-Interest * Time) * Exercise))

    d1_ = dOne(Stock, Exercise, Time, Interest, Yield, sigma)
    d2_ = dTwo(Stock, Exercise, Time, Interest, Yield, sigma)

    return (Stock * exp(-Yield * Time) * en(d1_) -
                Exercise * exp(-Interest * Time) * en(d2_))
}

#' European Put
#'
#' A put option that can only be exercised at expiration
#'
#' @inheritParams dOne
#' @return the price of a European Put Option
#' @references
#' Hull 7th edition Ch 13 P 291
#'
#' @examples
#' # Hull 7th edition Ch 13 P 294
#' Stock     <- 42
#' Exercise  <- 40
#' Time      <- 0.50
#' Interest  <- 0.10
#' Yield     <- 0
#' sigma     <- 0.20
#' ans <- EuroPut(Stock, Exercise, Time, Interest, Yield, sigma)
#' round(ans,2)
#'
#' @export
EuroPut <- function(Stock, Exercise, Time, Interest, Yield, sigma) {

    if (Time == 0) return (max(0, Exercise - Stock))

    if (sigma == 0) return (max(0, exp(-Interest * Time) * Exercise - exp(-Yield * Time) * Stock))

    d1_ = dOne(Stock, Exercise, Time, Interest, Yield, sigma)
    d2_ = dTwo(Stock, Exercise, Time, Interest, Yield, sigma)

    return (Exercise * exp(-Interest * Time) * en(-d2_) -
                Stock * exp(-Yield * Time) * en(-d1_))
}

##
## American Put
## ------------
## Per Kerry Back Chapt5.bas
##

##
## Inputs are S0 = initial stock price
##            K = strike price
##            r = risk-free rate
##            sigma = volatility
##            q = dividend yield
##            Tm = time to maturity
##            N = number of time periods
##

American_Put_Binomial <- function(S0, K, r, sigma, q, Tm, N) {
    PutV <- rep(-1, N+2)
    dt <- Tm / N                                   # length of time period
    u <- exp(sigma * sqrt(dt))                    # size of up step
    d <- 1 / u                                    # size of down step
    pu <- (exp((r - q) * dt) - d) / (u - d)       # probability of up step

    dpu <- exp(-r * dt) * pu                      # one-period discount x prob of up step
    dpd <- exp(-r * dt) * (1 - pu)                # one-period discount x prob of down step

    u2 <- u * u
    S <- S0 * d ** N                              # stock price at bottom node at last date
    PutV[1] <- max(K - S, 0)                         # put value at bottom node at last date
    #lapply(2:N+2, function (j) {S <- S * u2
    #                            PutV[j] <- max(K - S, 0) })
    for (j in seq(2, N+2)) {
        S <- S * u2
        PutV[j] <- max(K - S, 0)
    }

    for (i in rev(seq(1, N)))  {                   # back up in time to date 0
        S <- S0 * d ** i                            # stock price at bottom node at date i
        PutV[1] <- max(K - S, dpd * PutV[1] + dpu * PutV[2])
        for (j in seq(2, i+2)) {                    # step up over nodes at date i
            S <- S * u2
            PutV[j] = max(K - S, dpd * PutV[j] + dpu * PutV[j + 1])
        }
    }

    return (PutV[1])                               # put value at bottom node at date 0
}

##
## Greeks from Hull (Edition 7) Chapter 17 p378
##--------------------------------------------
##

#' Delta of a European Call Option
#'
#' Given a change in asset price, DeltaCall describes the amount by which the
#' call option price changes.
#'
#' \code{Change_in_Call-Option_Price = DeltaCall * Change_in_Asset_Price}
#'
#' @inheritParams dOne
#' @return The Delta of the call option
#'
#' @examples
#' # Hull, 7th edition Ch 17 p 363
#' library(ustreasuries)
#' Stock    <- 49     # S_0
#' Exercise <- 50     # K
#' Time     <- 20/52  # T
#' Interest <- 0.05   # r
#' Yield    <- 0      # q
#' sigma    <- 0.20
#'
#' ans <- DeltaCall(Stock, Exercise, Time, Interest, Yield, sigma)
#'
#' writeLines(paste0("Delta call: when the asset price changes by Delta_S,\n",
#'                   "                the option price changes by Delta_S*",round(ans, 3)))
#'
#' @export
DeltaCall <- function(Stock, Exercise, Time, Interest, Yield, sigma) {

    if (Time == 0) {
        if (Stock > Exercise) {
            return (1)
        } else {
            return (0)
        }
    }

    if (sigma == 0) sigma = 0.0000000001

    d1_ = dOne(Stock, Exercise, Time, Interest, Yield, sigma)

    return (exp(-Yield * Time) * phi(d1_))
}

#' Delta of a European Put Option
#'
#' Given a change in asset price, DeltaPut describes the amount by which the
#' put option price changes.
#'
#' \code{Change_in_Put-Option_Price = DeltaPut * Change_in_Asset_Price}
#'
#' @inheritParams dOne
#' @return The Delta of the put option
#' @examples
#' # Hull, 7th edition Ch 17 p 362,3
#' library(ustreasuries)
#' Stock    <- 49     # S_0
#' Exercise <- 50     # K
#' Time     <- 20/52  # T
#' Interest <- 0.05   # r
#' Yield    <- 0      # q
#' sigma    <- 0.20
#'
#' dcall <- DeltaCall(Stock, Exercise, Time, Interest, Yield, sigma)
#' dput  <- DeltaPut(Stock, Exercise, Time, Interest, Yield, sigma)
#'
#' writeLines(paste0("Delta put: when the asset price changes by Delta_S,\n",
#'                   "               the option price changes by Delta_S*",round(dput, 3),
#'                   "\nDelta put = Delta call - 1? ", dput == dcall-1))
#'
#' @export
DeltaPut <- function(Stock, Exercise, Time, Interest, Yield, sigma) {

    if (Time == 0) {
        if (Stock < Exercise) {
            return (-1)
        } else {
            return (0)
        }
    }

    if (sigma == 0) sigma = 0.0000000001

    d1_ = dOne(Stock, Exercise, Time, Interest, Yield, sigma)

    return (exp(-Yield * Time) * (phi(d1_) - 1))
}


#
# Gamma the convexity
# -----
#

#' Gamma of a European Option; convexity, curvature
#'
#' Gamma describes the rate of change in the Delta; it is the second derivative of the option or portfolio of options with respect to the underlying asset's price.
#'
#' If \code{abs(Gamma)} is large then the Delta is very sensitive to changes in the price of the underlying asset.
#'
#' @inheritParams dOne
#' @return The Gamma of the option
#' @references
#' Hull, 7th edition Ch 17 p 369-373
#' @examples
#' # Hull, 7th edition Ch 17 p 371,2
#' library(ustreasuries)
#' Stock    <- 49     # S_0
#' Exercise <- 50     # K
#' Time     <- 20/52  # T
#' Interest <- 0.05   # r
#' Yield    <- 0      # q
#' sigma    <- 0.20
#'
#' gamma <- Gamma(Stock, Exercise, Time, Interest, Yield, sigma)
#' round(gamma, 3) # 0.066
#'
#' @note This Gamma masks that of package:stats; to use the latter write stats::Gamma()
#'
#' @export
Gamma <- function (Stock, Exercise, Time, Interest, Yield, sigma) {

    if (sigma == 0) sigma = 0.0000000001

    d1_ = dOne(Stock, Exercise, Time, Interest, Yield, sigma)

    return(nprime(d1_) * exp(-Yield * Time)/(Stock * sigma * sqrt(Time)))
}

##
## Theta the decay in the value of an option/portfolio of options as time passes
## -----
##
## divide by 365 for "per calendar day"; 252 for "per trading day"
##
## In a delta-neutral portfolio, Theta is a proxy for Gamma
##

#' Theta of a European Call Option
#'
#' Theta is the decay in the value of an option or a portfolio of options as time passes
#'
#' In a delta-neutral portfolio, Theta is a proxy for Gamma
#'
#' @note divide by 365 for "per calendar day"; 252 for "per trading day"
#'
#' @inheritParams dOne
#' @return The Theta of the call option
#' @references
#' Hull, 7th edition ch 17 p367-368
#' @examples
#' # Hull, 7th edition Ch 17 p 367
#' library(ustreasuries)
#' Stock    <- 49     # S_0
#' Exercise <- 50     # K
#' Time     <- 20/52  # T
#' Interest <- 0.05   # r
#' Yield    <- 0      # q
#' sigma    <- 0.20
#'
#' thcall <- ThetaCall(Stock, Exercise, Time, Interest, Yield, sigma)
#'
#' writeLines(paste0("Theta:            ", round(thcall, 2),     "\n",
#'                   "per calendar day: ", round(thcall/365, 4), "\n",
#'                   "per trading day:  ", round(thcall/252, 4)))
#'
#' @export
ThetaCall <- function(Stock, Exercise, Time, Interest, Yield, sigma) {

    if (sigma == 0) sigma <- 0.0000000001

    d1_  <- dOne(Stock, Exercise, Time, Interest, Yield, sigma)
    d2_  <- dTwo(Stock, Exercise, Time, Interest, Yield, sigma)

    Nd1_ <- phi(d1_)
    Nd2_ <- phi(d2_)

    return(-Stock * nprime(d1_) * sigma * exp(-Yield * Time) / (2 * sqrt(Time))
           + Yield * Stock * Nd1_ * exp(-Yield * Time)
           - Interest * Exercise * exp(-Interest * Time) * Nd2_)
}

#' Theta of a European Put Option
#'
#' Theta is the decay in the value of an option or a portfolio of options as time passes
#'
#' In a delta-neutral portfolio, Theta is a proxy for Gamma
#'
#' @note divide by 365 for "per calendar day"; 252 for "per trading day"
#'
#' @inheritParams dOne
#' @return The Theta of the put option
#'
#' @references
#' Hull, 7th edition ch 17 p367-368
#'
#' @examples
#' # Hull, 7th edition Ch 17 p 367
#' library(ustreasuries)
#' Stock    <- 49     # S_0
#' Exercise <- 50     # K
#' Time     <- 20/52  # T
#' Interest <- 0.05   # r
#' Yield    <- 0      # q
#' sigma    <- 0.20
#'
#' thcall <- ThetaCall(Stock, Exercise, Time, Interest, Yield, sigma)
#' thput  <- ThetaPut(Stock, Exercise, Time, Interest, Yield, sigma)
#'
#' rKe    <- Interest * Exercise * exp(-Interest*Time)
#'
#' writeLines(paste0("ThetaCall:        ", round(thcall, 2), "\n",
#'                   "ThetaPut:         ", round(thput, 2),   "\n",
#'                   "per calendar day: ", round(thput/365, 4), " (put)", "\n",
#'                   "per trading day:  ", round(thput/252, 4), " (put)", "\n\n",
#'     "ThetaPut is always greater than ThetaCall by an amount rKe:", "\n",
#'     "Diff: ",thput-thcall,"\n",
#'     "rKe:  ",rKe))
#'
#' @export
ThetaPut <- function(Stock, Exercise, Time, Interest, Yield, sigma) {

    if (sigma == 0) sigma <- 0.0000000001

    d1_ <- dOne(Stock, Exercise, Time, Interest, Yield, sigma)
    d2_ <- dTwo(Stock, Exercise, Time, Interest, Yield, sigma)

    Nminusd1_ <- phi(-d1_)
    Nminusd2_ <- phi(-d2_)

    return(-Stock * nprime(d1_) * sigma * exp(-Yield * Time) / (2 * sqrt(Time))
           - Yield * Stock * Nminusd1_ * exp(-Yield * Time)
           + Interest * Exercise * exp(-Interest * Time) * Nminusd2_)
}

##
## Vega the sensitivity to changes in the volatility of the underlying
## ----
##

#' Vega of a European Option
#'
#' Vega is the sensitivity of an option price to changes in the volatility of the underlying asset
#'
#' Vega is not a Greek letter, it is the brightest star in the constellation Lyra
#'
#' @note if \code{abs(Vega)} is large, the option or portolio is very sensitive to changes in the volatility of the underlying asset
#'
#' @inheritParams dOne
#' @return The Vega of the option
#'
#' @references
#' Hull, 7th edition ch 17 p373-375
#'
#' @examples
#' # Hull, 7th edition Ch 17 p 375
#' library(ustreasuries)
#' Stock    <- 49     # S_0
#' Exercise <- 50     # K
#' Time     <- 20/52  # T
#' Interest <- 0.05   # r
#' Yield    <- 0      # q
#' sigma    <- 0.20
#'
#' vega <- Vega(Stock, Exercise, Time, Interest, Yield, sigma)
#'
#' writeLines(paste0("The value of Vega is ", round(vega,1),   "\n",
#' "Therefore, a 1% change in the volatility from 20% to 21%", "\n",
#' "will raise the price of the option by this amount:",       "\n",
#' "1% x ", round(vega,1), " = ", round((0.01 * vega), 3),
#' ", from ", Stock, " to ", Stock+round((0.01 * vega), 3)))
#'
#' @export
Vega <- function(Stock, Exercise, Time, Interest, Yield, sigma) {

    if (sigma == 0) sigma = 0.0000000001
    d1_ = dOne(Stock, Exercise, Time, Interest, Yield, sigma)
    return(Stock * sqrt(Time) * nprime(d1_) * exp(-Yield * Time))
}

##
## Rho the sensitivity to changes in the interest rate

## Note the various Rho calculations see Hull 7th Edition Ch 17 P378

RhoFuturesCall <- function(Stock, Exercise, Time, Interest, Yield, sigma) {
    return(-EuroCall(Stock, Exercise, Time, Interest, Yield, sigma) * Time)
}

RhoFuturesPut <- function(Stock, Exercise, Time, Interest, Yield, sigma) {
    return(-EuroPut(Stock, Exercise, Time, Interest, Yield, sigma) * Time)
}


## The Rho corresponding to the domestic interest rate is RhoCall/Put, below foreign  interest rate is RhoFXCall/Put, shown here
RhoFXCall <- function(Stock, Exercise, Time, Interest, Yield, sigma) {
    d1_ = dOne(Stock, Exercise, Time, Interest, Yield, sigma)
    Nd1_ = phi(d1_)
    return(-Time * exp(-Yield * Time) * Stock * Nd1_)
}

RhoFXPut <- function(Stock, Exercise, Time, Interest, Yield, sigma) {
    d1_ = dOne(Stock, Exercise, Time, Interest, Yield, sigma)
    Nminusd1_ = phi(-d1_)
    return(Time * exp(-Yield * Time) * Stock * Nminusd1_)
}

# ===============
# "Standard" Rhos
# ===============

#' Rho of a European Call Option
#'
#' Rho is the sensitivity of an option price to changes in the risk-free
#' rate of interest
#'
#' @note for futures and foreign exchange, use the specialized functions
#' RhoFuturesCall, RhoFuturesPut, RhoFXCall, RhoFXPut
#'
#' @inheritParams dOne
#' @return The Rho of the call option
#'
#' @references
#' Hull, 7th edition ch 17 p375-376
#'
#' @examples
#' # Hull, 7th edition Ch 17 p 376
#' library(ustreasuries)
#' Stock    <- 49     # S_0
#' Exercise <- 50     # K
#' Time     <- 20/52  # T
#' Interest <- 0.05   # r
#' Yield    <- 0      # q
#' sigma    <- 0.20
#'
#' rho <- RhoCall(Stock, Exercise, Time, Interest, Yield, sigma)
#' round(rho, 2) # 8.91
#'
#' @export
RhoCall <- function(Stock, Exercise, Time, Interest, Yield, sigma) {
    if (sigma == 0) sigma = 0.0000000001
    d2_ = dTwo(Stock, Exercise, Time, Interest, Yield, sigma)
    Nd2_ = phi(d2_)
    return(Exercise * Time * exp(-Interest * Time) * Nd2_)
}

#' Rho of a European Put Option
#'
#' Rho is the sensitivity of an option price to changes in the risk-free
#' rate of interest
#'
#' @note for futures and foreign exchange, use the specialized functions
#' RhoFuturesCall, RhoFuturesPut, RhoFXCall, RhoFXPut
#'
#' @inheritParams dOne
#' @return The Rho of the put option
#'
#' @references
#' Hull, 7th edition ch 17 p375-376
#'
#' @examples
#' # Hull, 7th edition Ch 17 p 376
#' Stock    <- 49     # S_0
#' Exercise <- 50     # K
#' Time     <- 20/52  # T
#' Interest <- 0.05   # r
#' Yield    <- 0      # q
#' sigma    <- 0.20
#'
#' rhocall <- RhoCall(Stock, Exercise, Time, Interest, Yield, sigma)
#' rhoput  <- RhoPut(Stock, Exercise, Time, Interest, Yield, sigma)
#'
#' writeLines(paste0("RhoCall:  ", round(rhocall, 2), "\n",
#'                   "RhoPut:  ", round(rhoput, 2)))
#'
#' @export
RhoPut <- function(Stock, Exercise, Time, Interest, Yield, sigma) {
    if (sigma == 0) sigma = 0.0000000001
    d2_ = dTwo(Stock, Exercise, Time, Interest, Yield, sigma)
    Nminusd2_ = phi(-d2_)
    return(-Exercise * Time * exp(-Interest * Time) * Nminusd2_)
}

## =================================
## Implied Volatility from Benningna
## =================================


#' Implied Volatility for a European Call Option
#'
#' @inheritParams dOne
#' @param Call_price the price of the call option being valued
#' @return The Implied Volatility of the call option
#'
#' @references
#' Hull, 7th edition ch 13 p296-297
#'
#' @examples
#' # Hull, 7th edition ch 13 p296
#' Stock      <- 21     # S_0
#' Exercise   <- 20     # K
#' Time       <- 0.25   # T
#' Interest   <- 0.10   # r
#' Yield      <- 0      # q
#' Call_price <- 1.875
#'
#' callvol <- EuroCallVol(Stock, Exercise, Time, Interest, Yield, Call_price)
#' writeLines(paste0("Implied Call Volatility: ", round(callvol*100, 1), "% per annum"))
#'
#' @export
EuroCallVol <- function(Stock, Exercise, Time, Interest, Yield, Call_price) {
    High <- 2
    Low <- 0
    while ((High - Low) > 0.000001) {
        if (EuroCall(Stock, Exercise, Time, Interest, Yield, (High + Low) / 2) > Call_price)
            High <- (High + Low) / 2
        else Low <- (High + Low) / 2
    }
    return((High + Low) / 2)
}

#' Implied Volatility for a European Put Option
#'
#' @inheritParams dOne
#' @param Put_price the price of the put option being valued
#' @return The Implied Volatility of the put option
#'
#' @references
#' Hull, 7th edition ch 13 p296-297
#'
#' @examples
#' # Hull, 7th edition ch 13 p296
#' Stock      <- 21     # S_0
#' Exercise   <- 20     # K
#' Time       <- 0.25   # T
#' Interest   <- 0.10   # r
#' Yield      <- 0      # q
#' Put_price <- 1.875
#'
#' putvol <- EuroPutVol(Stock, Exercise, Time, Interest, Yield, Put_price)
#' writeLines(paste0("Implied Put Volatility: ", round(putvol*100, 1), "% per annum"))
#'
#' @export
EuroPutVol <- function(Stock, Exercise, Time, Interest, Yield, Put_price) {
    High = 2
    Low = 0
    while ((High - Low) > 0.000001) {
        if (EuroPut(Stock, Exercise, Time, Interest, Yield, (High + Low) / 2) > Put_price) High = (High + Low) / 2
        else Low = (High + Low) / 2
    }
    return((High + Low) / 2)
}


## Implied Volatility from Kerry Back p64
## Chapt3.bas Newton Raphson technique
## Answer IDENTICAL to Bennigna (EuroCallVol)
Black_Scholes_Call <- function(S, K, r, sigma, q, T) {
    return(EuroCall(S, K, T, r, q, sigma))
}

#
## Inputs are S = initial stock price
##            K = strike price
##            r = risk-free rate
##            q = dividend yield
##            T = time to maturity
##            CallPrice = call price
Black_Scholes_Call_Implied_Vol <- function(S, K, r, q, T, CallPrice) {
    if (CallPrice < exp(-q * T) * S - exp(-r * T) * K) stop ("Option price violates the arbitrage bound.")
    tol <- 10 ^ -6
    lower <- 0
    flower <- Black_Scholes_Call(S, K, r, lower, q, T) - CallPrice
    upper <- 1
    fupper <- Black_Scholes_Call(S, K, r, upper, q, T) - CallPrice
    while (is.finite(fupper) && fupper < 0) {                       # double upper until it is an upper bound
        upper <- 2 * upper
        fupper <- Black_Scholes_Call(S, K, r, upper, q, T) - CallPrice
    }
    if (is.infinite(fupper) || is.nan(fupper)) {
        upper <- upper/2
        fupper <- Black_Scholes_Call(S, K, r, upper, q, T) - CallPrice
    }
    guess <- 0.5 * lower + 0.5 * upper
    fguess <- Black_Scholes_Call(S, K, r, guess, q, T) - CallPrice
    while ((upper - lower) > tol)  {                                # until root is bracketed within tol
        if (is.finite(fupper * fguess) && fupper * fguess < 0) {      # root is between guess and upper
            lower <- guess                                              # make guess the new lower bound
            flower <- fguess
            guess <- 0.5 * lower + 0.5 * upper                          # new guess = bi-section
            fguess <- Black_Scholes_Call(S, K, r, guess, q, T) - CallPrice
        }
        else {                                                        # root is between lower and guess
            upper <- guess                                              # make guess the new upper bound
            fupper <- fguess
            guess <- 0.5 * lower + 0.5 * upper                          # new guess = bi-section
            fguess <- Black_Scholes_Call(S, K, r, guess, q, T) - CallPrice
        }
    }
    return(guess)
}


## Implied Volatility from Wilmott Into Ch 8 p192 Newton Raphson***NOT DEBUGGED***
ImpVolCall <- function(Stock, Exercise, Time, Interest, Yield, Call_price) {
    Volatility <- 0.2
    epsilon <- 0.0001
    dv <- epsilon + 1
    while (is.finite(dv) && abs(dv) > epsilon) {
        PriceError <- EuroCall(Stock, Exercise, Time, Interest, Yield, Volatility) - Call_price
        dv <- PriceError / Vega(Stock, Exercise, Time, Interest, Yield, Volatility)
        if (is.finite(dv)) Volatility <- Volatility - dv
    }
    return(Volatility)
}


## from Kerry Back Chapt8.bas
## ... need Python's "BiNormalProb", R's "pbivnorm"
##
## Inputs are S = initial stock price
##            K = strike price
##            r = risk-free rate
##            sigma = volatility
##            Div = cash dividend
##            TDiv = time until dividend payment
##            TCall = time until option matures >= TDiv
##
American_Call_Dividend <- function(S, K, r, sigma, Div, TDiv, TCall) {

    LessDiv <- S - exp(-r * TDiv) * Div               # stock value excluding dividend
    if ((Div / K) <= (1 - exp(-r * (TCall - TDiv))))      # early exercise cannot be optimal
        return (Black_Scholes_Call(LessDiv, K, r, sigma, 0, TCall))

    ## Now we find an upper bound for the bisection.
    upper <- K
    while ((upper + Div - K) < Black_Scholes_Call(upper, K, r, sigma, 0, TCall - TDiv)) {
        upper <- 2 * upper
    }
    ## Now we use bisection to compute Zstar = LessDivStar.
    tol <- 10 ** -6
    lower <- 0
    flower <- Div - K
    fupper <- upper + Div - K - Black_Scholes_Call(upper, K, r, sigma, 0, TCall - TDiv)
    guess <- 0.5 * lower + 0.5 * upper
    fguess <- guess + Div - K - Black_Scholes_Call(guess, K, r, sigma, 0, TCall - TDiv)

    while (upper - lower > tol)
        if (fupper * fguess < 0) {
            lower <- guess
            flower <- fguess
            guess <- 0.5 * lower + 0.5 * upper
            fguess <- guess + Div - K - Black_Scholes_Call(guess, K, r, sigma, 0, TCall - TDiv)
        }
    else {
        upper <- guess
        fupper <- fguess
        guess <- 0.5 * lower + 0.5 * upper
        fguess <- guess + Div - K - Black_Scholes_Call(guess, K, r, sigma, 0, TCall - TDiv)
    }

    LessDivStar = guess

    ## Now we calculate the probabilities and the option value.
    d1 <- (log(LessDiv / LessDivStar) + (r + sigma ** 2 / 2) * TDiv) / (sigma * sqrt(TDiv))
    d2 <- d1 - sigma * sqrt(TDiv)
    d1prime <- (log(LessDiv / K) + (r + sigma ** 2 / 2) * TCall) / (sigma * sqrt(TCall))
    d2prime <- d1prime - sigma * sqrt(TCall)
    rho <- -sqrt(TDiv / TCall)
    N1 <- phi(d1)
    N2 <- phi(d2)
    M1 <- pbivnorm::pbivnorm(-d1, d1prime, rho)
    M2 <- pbivnorm::pbivnorm(-d2, d2prime, rho)
    return (LessDiv * N1 + exp(-r * TDiv) * (Div - K) * N2 + LessDiv * M1 - exp(-r * TCall) * K * M2)
}



