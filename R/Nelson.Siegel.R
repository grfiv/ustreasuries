#' Estimation of the Nelson-Siegel parameters
#'
#' @description Returns the estimated coefficients of the Nelson-Siegel's model.
#'
#' @param rate	vector or matrix which contains the interest rates
#' @param maturity	vector wich contains the maturity ( in months) of the rate.
#' The vector's length must be the same of the number of columns of the rate.
#'
#' @return Returns a data frame with the estimated coefficients:
#' \eqn{\beta_{0t}, \beta_{1t}, \beta_{2t},  \lambda}
#'
#' @author
#' From \code{YieldCurve} package by Sergio Salvino Guirreri
#'
#' Modified by George Fisher
#'
#' @examples
#' library(ustreasuries)
#'
#' # ==============================================
#' # example of fitting CMT data with Nelson.Seigel
#' # ==============================================
#' all_data <- CMTrates()
#'
#' # pick a selection of data around the date of the curve you want
#' rate.CMT    <- dplyr::filter(all_data,
#'                                all_data$NEW_DATE>=Sys.Date()-10 &
#'                                all_data$NEW_DATE<=Sys.Date())
#'
#' NSParameters <- Nelson.Siegel(rate=rate.CMT)
#'
#' head(NSParameters)
#'
#' Nelson.Siegel.plot(rate.CMT, NSrates(NSParameters), rows=5)
#'
#' @importFrom stats coef lm median na.omit optimize resid time
#' @importFrom xts xts reclass
#'
#' @export
`Nelson.Siegel` <-
function( rate, maturity=c(1/12, 3/12, 6/12, 1,2,3,5,7,10,20,30) )
  {
      # check the "data.source" attribute of 'rate'
      if (attr(rate, "data.source") == "CMT") {
          rate <- xts::xts(rate[,3:13], order.by=rate$NEW_DATE)
      } else {
          stop("rate parameter must have a valid 'data.source' attribute")
      }

      pillars.number <- length(maturity)
      lambdaValues   <- seq(maturity[1], maturity[ pillars.number ], by=0.5)

      # create a zeros matrix,
      #      one row for each rate
      #      columns for "beta_0", "beta_1", "beta_2", "lambda"
      FinalResults             <- matrix(0, nrow(rate), 4)
      colnames( FinalResults ) <- c("beta_0","beta_1","beta_2","lambda")

    j <- 1
    while(j <= nrow(rate) )
      {
        # another zeros matrix
        InterResults             <- matrix(0, length(lambdaValues), 5)
        colnames( InterResults ) <- c("beta0","beta1","beta2","lambda","SSR")

        for( i in 1:length(lambdaValues))
          {
            lambdaTemp <- optimize(.factorBeta2, interval=c(0.001,1),
                          maturity=lambdaValues[i], maximum=TRUE)$maximum
            InterEstimation <- .NS.estimator(as.numeric(rate[j,]), maturity, lambdaTemp)
            BetaCoef        <- InterEstimation$Par
	    if( BetaCoef[1]>0 & BetaCoef[1]<20)
              {
                SSR <- sum(InterEstimation$Res^2)
                InterResults[i,] <- c(BetaCoef, lambdaTemp, SSR)
              } else
            {
              InterResults[i,] <- c(BetaCoef,lambdaValues[i],1e+5)
            }
          }
        BestRow          <- which.min(InterResults[,5])
        FinalResults[j,] <- InterResults[BestRow,1:4]
        j <- j+1
      }
    xts::reclass( FinalResults, rate )
  }

#' Interest rates of the Nelson-Siegel model
#'
#' @description Returns the interest rates by Nelson-Siegel's model.
#'
#' @details Coeff is a vector or matrix of the four coefficients of the Nelson-Siegel's model:
#' \eqn{\beta_0, \beta_1, \beta_2,  \lambda}
#'
#' @param Coeff	Vector or matrix of the beta's coefficients and lambda as the function Nelson.Siegel returns.
#' @param maturity	maturity of the yield curve of which want to return the interest rates.
#' @return Return interest rates in matrix object with number of rows equal to nrow(betaCoeff) and number of columns equal to length(maturity).
#'
#' @author
#' From \code{YieldCurve} package by Sergio Salvino Guirreri
#'
#' Modified by George Fisher
#'
#' @examples
#' library(ustreasuries)
#' all_data <- CMTrates()
#'
#' # pick a selection of data around the date of the curve you want
#' rate.CMT    <- dplyr::filter(all_data,
#'                                all_data$NEW_DATE>=Sys.Date()-10 &
#'                                all_data$NEW_DATE<=Sys.Date())
#'
#' NSParameters <- Nelson.Siegel(rate=rate.CMT)
#'
#' head(NSrates(NSParameters))
#'
#' @export
`NSrates` <- function (Coeff, maturity=c(1/12, 3/12, 6/12, 1,2,3,5,7,10,20,30))
{
    Curve <- xts::xts(matrix( 0, nrow(Coeff), length(maturity) ), order.by=time(Coeff))
    colnames(Curve) <- make.names(maturity)
    Coeff <- as.matrix( Coeff )

    for(i in 1:nrow(Curve))
    {
        Curve[i,] <- as.numeric(Coeff[i,1]) * rep(1, length(maturity)) +
            as.numeric(Coeff[i,2]) * as.numeric(.factorBeta1(Coeff[i,4], maturity) ) +
            as.numeric(Coeff[i,3]) * as.numeric(.factorBeta2(Coeff[i,4], maturity ))
    }
    return( Curve )
}

#' Convert NSrates to discount factors Z(0, T)
#'
#' @description Returns the discount factors associated with Nelson Seigel fitted rates
#'
#' @details \eqn{Z(0, T) = e^{-r(0, T) * T}}
#'
#' @param NS_rates the value returned by the \code{NSrates()} function
#' @return \code{data.frame} rownames are the dates,
#' colnames are the time to maturity in character format preceded by "X"
#'
#' @author
#' George Fisher
#'
#' @references
#' Veronesi, P., \emph{Fixed Income Securities} p67
#'
#' @examples
#' library(ustreasuries)
#' all_data <- CMTrates()
#'
#' rate.CMT    <- dplyr::filter(all_data,
#'                                all_data$NEW_DATE>=Sys.Date()-10 &
#'                                all_data$NEW_DATE<=Sys.Date())
#'
#' NSParameters <- Nelson.Siegel(rate=rate.CMT)
#'
#' head(NSzeros(NSrates(NSParameters)))
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
    return(data.frame(t(apply(NS_rates, 1, foo))))
}

`.beta1Spot` <- function(maturity, tau)
{
    as.numeric( (1 - exp(-maturity/tau))/(maturity/tau))
}

`.beta2Spot` <- function(maturity, tau)
{
    as.numeric(  ((1 - exp(-maturity/tau))/(maturity/tau) - exp(-maturity/tau)) )
}

`.beta1Forward` <- function(maturity, tau)
{
    as.numeric( exp(-maturity/tau) )
}

`.beta2Forward` <- function(maturity, tau)
{
    as.numeric( exp(-maturity/tau) * (maturity/tau) )
}

`.factorBeta1` <- function(lambda, maturity)
{
    as.numeric( (1-exp(-lambda*maturity)) / (lambda*maturity) )
}

`.factorBeta2` <- function(lambda, maturity)
{
    as.numeric( (1-exp(-lambda*maturity)) / (lambda*maturity) - exp(-lambda*maturity) )
}

`.NS.estimator` <- function( rate, maturity, lambda )
{
    beta <- lm( rate ~ 1 + .factorBeta1(lambda,maturity) +
                    .factorBeta2(lambda,maturity) )
    betaPar <- coef(beta)
    NaValues <- na.omit(betaPar)
    if( length(NaValues)<3 ) betaPar <- c(0,0,0)
    names(betaPar) <- c("beta_0", "beta_1", "beta_2")
    EstResults <- list(Par=betaPar, Res=resid(beta))
    return(EstResults)
}

`.NSS.estimator` <- function( rate, maturity, tau1, tau2 )
{
    beta <- lm( rate ~ 1 + .beta1Spot(maturity,tau1) +
                    .beta2Spot(maturity,tau1) +
                    .beta2Spot(maturity,tau2) )
    betaPar <- coef(beta)
    NaValues <- na.omit(betaPar)
    if( length(NaValues)<4 ) betaPar <- c(0,0,0,0)
    names(betaPar) <- c("beta_0", "beta_1", "beta_2","beta_3")
    EstResults <- list(Par=betaPar, Res=resid(beta))
    return(EstResults)
}

`Srates`<- function( Coeff, maturity, whichRate="Forward" )
{
    if(ncol(Coeff)==1) Coeff<-matrix(as.vector(Coeff),1,nrow(Coeff))
    Curve <- xts(matrix( 0, nrow(Coeff), length(maturity) ), order.by=time(Coeff))
    colnames(Curve) <- make.names(maturity)
    Coeff <- as.matrix(Coeff)

    switch(whichRate,
           Forward =
           {
               CurveForward <- Curve
               for(i in 1:nrow(Coeff))
               {
                   CurveForward[i,] <- Coeff[i,1]+
                       Coeff[i,2] * .beta1Forward( maturity, Coeff[i,5] ) +
                       Coeff[i,3] * .beta2Forward( maturity, Coeff[i,5] ) +
                       Coeff[i,4] * .beta2Forward( maturity, Coeff[i,6] )
               }
               FinalCurve<-CurveForward
           },
           Spot =
           {
               for(i in 1:nrow(Coeff))
               {
                   Curve[i,] <- Coeff[i,1] +
                       Coeff[i,2] * .beta1Spot( maturity, Coeff[i,5] ) +
                       Coeff[i,3] * .beta2Spot( maturity, Coeff[i,5] ) +
                       Coeff[i,4] * .beta2Spot( maturity, Coeff[i,6] )
               }
               FinalCurve <- Curve
           })
    reclass( FinalCurve, Coeff )
}


`Svensson` <-
    function( rate, maturity )
    {

        rate <- xts::try.xts(rate, error=as.matrix)
        if(ncol(rate)==1) rate<-matrix(as.vector(rate),1,nrow(rate))
        pillars.number <- length(maturity)
        Tau1Values <- seq(maturity[1], median(maturity), by=1)
        Tau2Values <- seq(median(maturity), maturity[pillars.number], by=1.5)

        FinalResults <- matrix(0, nrow(rate), 6)
        FinalResultsTau2 <- matrix(0, length(Tau1Values), 7)
        colnames( FinalResults ) <- c("beta_0","beta_1","beta_2","beta_3","tau1","tau2" )
        j <- 1
        while(j <= nrow(rate) )
        {
            InterResultsTau1 <- matrix(0,length(Tau1Values), 7)
            InterResultsTau2 <- matrix(0,length(Tau2Values), 7)
            # colnames( InterResults ) <- c("beta0","beta1","beta2","beta_3","Tau1","Tau2","SSR")
            for( i in 1:length(Tau1Values))
            {
                Tau1Temp <- optimize(.beta2Spot,interval=c(0.001,max(Tau1Values)),maturity=Tau1Values[i],maximum=TRUE)$maximum
                for( a in 1:length(Tau2Values))
                {
                    Tau2Temp <- optimize(.beta2Spot,interval=c(0.001,maturity[pillars.number]),maturity=Tau2Values[a],maximum=TRUE)$maximum
                    InterEstimation <- .NSS.estimator(as.numeric(rate[j,]), maturity, Tau1Temp, Tau2Temp)
                    BetaCoef <- InterEstimation$Par
                    SSR <- sum(InterEstimation$Res^2)
                    InterResultsTau2[a,] <- c(BetaCoef, Tau1Temp, Tau2Temp, SSR)
                }
                BestRowTau2 <- which.min(InterResultsTau2[,7])
                FinalResultsTau2[i,] <- InterResultsTau2[BestRowTau2,]
            }
            BestRow <- which.min(FinalResultsTau2[,7])
            FinalResults[j,] <- FinalResultsTau2[BestRow,1:6]
            j <- j+1
        }
        reclass( FinalResults, rate )
    }

#' Plot the actual and Nelson.Siegel Yield Curves
#'
#' @description Takes actual rates and Nelson Seigel rates and plot them
#'
#' @param actual_Rates a data.frame with the actual rates to be used in analysis
#' @param NS_rates The resulting Nelson Seigel rates
#' @param rows The rows in the two input matrices to plot
#' @param title plot title
#'
#' @return plot
#'
#' @seealso
#' CMTrates, Nelson.Siegel, NSrates
#'
#' @author
#' George Fisher
#'
#' @examples
#' library(ustreasuries)
#'
#' # ======================================================
#' # example of plotting CMT data fitted with Nelson.Seigel
#' # ======================================================
#' all_data <- CMTrates()
#'
#' # pick a selection of data around the date of the curve you want
#' rate.CMT    <- dplyr::filter(all_data,
#'                                all_data$NEW_DATE>=Sys.Date()-10 &
#'                                all_data$NEW_DATE<=Sys.Date())
#'
#' NSParameters <- Nelson.Siegel(rate=rate.CMT)
#'
#' Nelson.Siegel.plot(rate.CMT, NSrates(NSParameters), rows=5)
#'
#' @export
`Nelson.Siegel.plot` <-
    function(actual_Rates, NS_rates, rows=1,
             title="Fitted Nelson-Siegel yield curve") {

        # check for valid "data.source" attribute
        if (is.null(attr(actual_Rates, "data.source")) ||
            !(attr(actual_Rates, "data.source") %in% c("CMT"))) {
            stop("actual_Rates parameter must have a valid 'data.source' attribute")
        }

        if (attr(actual_Rates, "data.source") == "CMT") {
            rows     <- rows[1]
            rates    <- actual_Rates[rows, 3:13]
            Date     <- actual_Rates[rows, "NEW_DATE"]
            NS_rates <- NS_rates[rows,]
        }

        # find the data points for y-axis plotting ylim = c(y_min, y_max)
        y_min    <- min(rates, NS_rates, na.rm=TRUE)
        y_max    <- max(rates, NS_rates, na.rm=TRUE)

        # =====================
        # plot the actual rates
        # =====================
        plot(x = 1:ncol(rates),
             y = rates,
             type="b",   pch=10, col=1,
             ylim=c(y_min*.90, y_max*1.10), ylab="Interest Rate",
             xaxt="n", xlab='',
             main=title)
        axis(1, at     = axTicks(1),
             labels = substr(names(rates)[axTicks(1)],4,9))
        grid()
        text(x      = 9,
             y      = rates["BC_10YEAR"],
             pos    = 4,
             labels = format(Date, "%Y-%m-%d"),
             cex    = 0.75, col = 1)

        # ========================
        # plot Nelson.Seigel rates
        # ========================
        lines(x   = 1:ncol(NS_rates),
              y   = NS_rates,
              col = "red")
        points(x   = 1:ncol(NS_rates),
               y   = NS_rates,
               col = "red", pch = 10)

        legend("bottomright",
               legend = c("Actual", "Fitted"),
               lty    = c(1, 1),
               lwd    = c(1, 1),
               col    = c("black", "red"),
               cex    = 1.00)
    }

#' Plot Z(0, T) and r(0, T)
#'
#' @description Plot the discount factors and fitted rates derived from the NSrates and NSzeros functions
#'
#' @param r an xts list of NSrates
#' @param Z a data.frame list of NSzeros
#' @return a plot
#'
#' @author George Fisher
#'
#' @examples
#' library(ustreasuries)
#' all_data <- CMTrates()
#'
#' rate.CMT    <- dplyr::filter(all_data,
#'                         all_data$NEW_DATE>=Sys.Date()-10 &
#'                         all_data$NEW_DATE<=Sys.Date())
#' NSParameters <- Nelson.Siegel(rate=rate.CMT)
#'
#' r <- NSrates(NSParameters)[1,]           # r(0, T)
#' Z <- NSzeros(NSrates(NSParameters))[1,]  # Z(0, T)
#'
#' NSzeros.plot(r, Z)
#'
#' @importFrom graphics abline
#'
#' @export
NSzeros.plot <- function(r, Z) {
    r <- unlist(r)
    Z <- unlist(Z)
    if (ncol(r) != length(Z)) {
        stop("ncol(r) must equal length(Z)")
    }
    title <- paste0("Zeros Derived from Nelson Seigel Rates for ",
                    rownames(data.frame(r)))
    plot(1:length(Z), Z,
         ylim = c(min(Z,r),max(Z,r)), ylab="Interest Rate/Discount Factor",
         xaxt = "n", xlab = '',
         main = title, pch = 20)

    labelDF <- data.frame(X0.08="1MONTH", X0.25="3MONTH", X0.5="6MONTH",  X1="1YEAR",
                          X2="2YEAR",    X3="3YEAR",    X5="5YEAR",    X7="7YEAR",
                          X10="10YEAR",   X20="20YEAR",   X30="30YEAR",
                          stringsAsFactors = FALSE)
    labelIdx <- unname(sapply(names(Z),
                              function(x) paste0("X",
                                                 round(as.numeric(substr(x, 2, nchar(x))),
                                                       digits=2))))
    x_labels <- unname(sapply(labelIdx, function(x) labelDF[1,x]))
    axis(1, at     = axTicks(1),
         labels = x_labels[axTicks(1)])

    points(1:ncol(r), r, col="red", pch=20)

    text(1,r[[1]],paste0(round(r[[1]],digits=4),"%"),pos=4,col="red")
    text(1,Z[1],round(Z[1],digits=4),pos=4)

    text(ncol(r), r[[ncol(r)]], paste0(round(r[[11]],digits=4),"%"),pos=2,col="red")
    text(length(Z), Z[length(Z)],   round(Z[length(Z)],digits=4),pos=2)

    legend("top",c("Zeros","Rates"),pch=c(20,20),col=c("black","red"))
    grid()
    abline(h=1, lty=3)
}
