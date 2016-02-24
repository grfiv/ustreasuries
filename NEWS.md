# ustreasuries 0.0.0.9006

-------------------------------------------------------------------------------
--------

## Seventh Development Release 0.0.0.9006   
* ### Added vignette *veronesi-ch02*, all the examples and figures from Chapter 2 of Veronesi   
* ### Added *Zbootstrap* function
* ### Modified *FedInvestData* function to start at the date following the last date in the historical file
* ### Added *data-raw/FedInvestHistory_bring_to_current.R* to "top off" the FedInvest historical dataset so that as few POSTS as possible have to be made to bring it current
* ### Added *SP500* and *SP500TR* to download S&P 500 data


-------------------------------------------------------------------------------
--------

## Sixth Development Release 0.0.0.9005
* ### Extensive miscellaneous cleanup and refactoring
* ### Added rudimentary bond math

-------------------------------------------------------------------------------
--------

## Fifth Development Release 0.0.0.9004
* ### Added References in README and project documentation
* ### Changed **OptionGamma** to **Gamma**
* ### Changed **USTreasuryRates** to **CMTrates**
* ### Changed **CAGRd** to **CAGR**, added geometric & continuous
* ### Added discount factor functions
    * ### **discount_factor**, **interest_rate**
* ### Cited my use of Prof. Jayanth R Varma's *jrvFinance* package Lic: GPL (>=2)
* ### **CMTrates** modified to to add attr(..., "data.source") <- "CMT"
* ### Added **xts (>= 0.9.7)** to DESCRIPTION Imports
* ### Added **Nelson.Siegel**, **NSrates**, **NSzeros**, **Nelson.Siegel.plot**, **NSzeros.plot**
* ### Added **FedInvestData** Return treasury bond price data from 2010

-------------------------------------------------------------------------------
--------

## Fourth Development Release 0.0.0.9003
* ### Added Implied Volatlity **EuroCallVol** / **EuroPutVol**
* ### Added Time Value  **TimeValueCall** / **TimeValuePut**
* ### Updated black-scholes-merton vignette
* ### Updated GitHub Wiki

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

