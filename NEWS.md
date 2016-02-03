# ustreasuries 0.0.0.9002   

-------------------------------------------------------------------------------
--------

## Third Development Release 0.0.0.9002
* ### Added Greeks
    * **DeltaCall** / **DeltaPut**
    * **ThetaCall** / **ThetaPut**
    * **OptionGamma**
    * **Vega** 
    * **RhoCall** / **RhoPut**
* ### Added Utilities
    * **IntrinsicValueCall** / **IntrinsicValuePut**
    * **InTheMoneyCall** / **InTheMoneyPut**
* ### Added examples to ustreasuries.R
* ### Updated black-scholes-merton vignette
* ### Updated GitHub Wiki

-------------------------------------------------------------------------------
--------

## Second Development Release 0.0.0.9001

* ### Added all the Black Scholes Merton functions
    * **Black-Scholes-Merton**
        * **EuroCall** Calculate the price of a European call option with or without dividends
        * **EuroPut** Calculate the price of a European put option with or without dividends
    * **Utility Functions**
        * **CAGR**
            * **CAGRd** Calculate discrete Compound Annual Growth Rate
            * **r_continuous** Convert from discrete to continuous CAGR
            * **r_discrete** Convert from continuous to discrete CAGR
        * **Put/Call Parity**
            * **CallParity** Convert from a put-option price using put/call parity
            * **PutParity** Convert from a call-option price using put/call parity
        * **Risk Neutral/Forwards**
            * **RiskNeutralProb** Binomial tree risk-neutral probability
            * **ForwardPrice** Forward price with or without income or yield
            * **ForwardRate** Forward rate from Time1 to Time2 (discrete compounding)
    * **Installed but not yet undocumented**
        * **CashCall**
        * **CashPut**
        * **AssetCall**
        * **AssetPut**
        * **American_Put_Binomial**
        * **DeltaCall**
        * **DeltaPut**
        * **OptionGamma**
        * **ThetaCall**
        * **ThetaPut**
        * **Vega**
        * **RhoFuturesCall**
        * **RhoFuturesPut**
        * **RhoFXCall**
        * **RhoFXPut**
        * **RhoCall**
        * **RhoPut**
        * **EuroCallVol**
        * **EuroPutlVol**
        * **Black_Scholes_Call_Implied_Vo**l
        * **Black_Scholes_Put_Implied_Vol**
        * **ImpVolCall**
        * **ImpVolPut**
        * **American_Call_Dividend**

* ### Modified *yield-curves* vignette to show last three years
* ### Added two vignettes for the derivatives functions
    * *utilities* examples of the utility functions
    * *black-scholes-merton* examples of options pricing
    
-------------------------------------------------------------------------------   
------------------------   

## Initial Development Release 0.0.0.9000

* ### Three fully-documented functions (roxygen2)
    * **USTreasuryRates** downloads a data.frame with daily data from 1962
    * **PrintYieldCurves** prints one or more yield curves
    * **APY** converts Constant-Maturity Treasury (CMT) yields to Annualized Percentage Yields (APY)
    
* ### Three vignettes
    * **cmt-rates** a description of Constant Maturity and Annualized Percentage rates
    * **yield-curves** examples of downloading the data and printing yield curves for interesting periods in recent financial history
    * **plot-10year** a plot of the 10-year from 1962 to present
    
* ### Unit Tests (testthat) *devtools::test()*
    * **USTreasuryRates** Test data download : ...............
    * **APY**             Test APY : ..
    
* ### R CMD RStudio Build>Check Package, *devtools::check()*, *devtools::build_win()* 
    * No errors, warnings or notes
    * "Status: OK, R CMD check succeeded""
    
* ### Initial upload to GitHub
    * *devtools::install_github("grfiv/ustreasuries", build_vignettes=TRUE)*    
    successfully tested
    
-------------------------   

