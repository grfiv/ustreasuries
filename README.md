ustreasuries [![DOI](https://zenodo.org/badge/5738/grfiv/ustreasuries.svg)](https://zenodo.org/badge/latestdoi/5738/grfiv/ustreasuries)
=======================================================================================================================================

### US Treasury Bond Data

-   Real-Time downloads of
    -   daily Constant-Maturity Treasury (CMT) yields from 1962 to the most-recently completed business day
    -   daily prices for 400 Treasury securities (FedInvest) from 2010 to the most-recently completed business day

### Bond Analysis

Several models such as Nelson Seigel and Svensson are provided

**Veronesi** <sup>[Veronesi](#Veronesi)</sup> examples derived with code and live bond data

### Derivative Analysis

Drawing primarily on **Hull, 7th edition** <sup>[Hull](#Hull)</sup> *ustreasuries* includes many options-pricing models, all the Greeks and a number of utility functions, all of which have examples from Hull which demonstrate that they work correctly.

------------------------------------------------------------------------

### Wiki

See the [GitHub Wiki](https://github.com/grfiv/ustreasuries/wiki) for examples of the use of all the functions.

### Functions

-   **Treasury Rate Data**
    -   **CMTrates** downloads real-time daily CMT data from 1962
        -   **PrintYieldCurves** prints one or more CMT yield curves
        -   **APY** converts Constant-Maturity Treasury (CMT) yields to Annualized Percentage Yields (APY)
    -   **FedInvestData** downloads real-time treasury bond price data from 2010
        -   **CoerceFedInvest\_xts** Turn FedInvest data into a time series
-   **Black-Scholes-Merton**
    -   **EuroCall** Calculate the price of a European call option with or without dividends
    -   **EuroPut** Calculate the price of a European put option with or without dividends
    -   **EuroCallVol** Implied Volatility for a European Call option
    -   **EuroPutlVol** Implied Volatility for a European Put option
-   **Greeks**
    -   **DeltaCall** Amount call-option price changes given a change in asset price
    -   **DeltaPut** Amount put-option price changes given a change in asset price
    -   **ThetaCall** the decay in the value of a call or a portfolio of calls as time passes
    -   **ThetaPut** the decay in the value of a put or a portfolio of puts as time passes
    -   **Gamma** the change in Delta with respect to asset price
    -   **Vega** the sensitivity to changes in the volatility of the underlying
    -   **RhoCall** the sensitivity to changes in the risk-free rate of return
    -   **RhoPut** the sensitivity to changes in the risk-free rate of return
-   **Utility Functions**
    -   **Bonds**
        -   **NSzeros** Convert Nelson Seigel and Svensson rates to Z(0, T)
        -   **Zbootstrap** Derive Z(0, T\_i) using the bootstrap method
        -   **spot\_rate** Derive spot rate from Z(0, T) and T
    -   **CAGR**
        -   **CAGR** Calculate Compound Annual Growth Rate; geometric or continuous
        -   **r\_continuous** Convert from discrete to continuous
        -   **r\_discrete** Convert from continuous to discrete
    -   **Discount Factors**
        -   **discount\_factor** Calculate discount factor Z(t, T)
        -   **interest\_rate** Calculate annualized interest rate r(t, T) from a discount factor Z(t, T)
    -   **Put/Call Parity**
        -   **CallParity** Convert from a put-option price using put/call parity
        -   **PutParity** Convert from a call-option price using put/call parity
    -   **Risk Neutral/Forwards**
        -   **RiskNeutralProb** Binomial tree risk-neutral probability
        -   **ForwardPrice** Forward price with or without income or yield
        -   **ForwardRate** Forward rate from Time1 to Time2 (discrete compounding)
    -   **Options**
        -   **IntrinsicValueCall** / **IntrinsicValuePut** the in-the-money portion of an option's premium
        -   **TimeValueCall** / **TimeValuePut** Price = Intrinsic + Time
        -   **InTheMoneyCall** / **InTheMoneyPut** Is an option in the money?
    -   **Equities**
        -   **SP500** Daily S&P 500 data from 1950
        -   **SP500TR** Daily S&P 500 Total Return data from 1988
-   **Installed but not yet tested or documented**
    -   Digital
        -   **CashCall**
        -   **CashPut**
        -   **AssetCall**
        -   **AssetPut**
    -   Greeks
        -   **RhoFuturesCall**
        -   **RhoFuturesPut**
        -   **RhoFXCall**
        -   **RhoFXPut**
    -   American
        -   **American\_Put\_Binomial**
        -   **American\_Call\_Dividend**

#### All of the derivatives functions have examples drawn from **Hull, 7th edition** <sup>[Hull](#Hull)</sup> to demonstrate their correctness.

See <https://github.com/grfiv/BlackScholesMerton> for these functions written in Python and Excel VBA

See also <http://www.philadelphia-reflections.com/topic/230.htm>

Installation
------------

We're not on CRAN yet; get the development version from GitHub:

``` r
# see https://github.com/hadley/devtools for the
# best procedure to install *devtools* on your
# system; Windows in particular has somewhat
# complicated requirements

devtools::install_github("grfiv/treasuries")

# Notes: 
#    1. Add 'build_vignettes=TRUE' to include vignettes 
#       (recommended, but a current version of pandoc is required)
#    2. add 'auth_token="..."' if you get a 404
#       contact the author for this
#    3. if you receive a message about corrupt databases or fetch(key), 
#       restarting R will fix the problem;
#       these appear to be issues with devtools 1.10.0.9000
```

References
----------

<a name="Back">\[Back\]</a>: Back, K., *A Course in Derivative Securities* 2005 Springer Finance ISBN 9783540253734

<a name="Consiglio">\[Consiglio\]</a>: Consiglio A. and Guirreri S.S. *Simulating the Term Structure of Interest Rates with arbitrary marginals*. International Journal of Risk Assessment and Management, 15(4), September 2011.

<a name="Hull">\[Hull\]</a>: Hull, J., *Options, Futures and Other Derivatives Seventh Edition* 2008 Pearson/Prentice Hall ISBN 9780136015864

<a name="CMT">\[CMT\]</a>: US Dept. of Treasury Daily Treasury Yield Curve Rates
<https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield>

<a name="H15">\[H15\]</a>: US Federal Reserve Board Data Download Program
<http://www.federalreserve.gov/datadownload/Choose.aspx?rel=H15>

<a name="Varma">\[Varma\]</a>:
Varma, J. *jrvFinance* v1.03 (<https://github.com/jrvarma/jrvFinance>)

<a name="Veronesi">\[Veronesi\]</a>: Veronesi P., *Fixed Income Securities* 2010 John Wiley & Sons ISBN 9780470109106

<a name="MDC">\[MDC\]</a>: Wall Street Journal Market Data Center
<http://online.wsj.com/mdc/public/page/mdc_bonds.html?mod=mdc_topnav_2_3010>
