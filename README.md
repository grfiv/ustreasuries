<!-- README.md is generated from README.Rmd. Please edit that file -->
ustreasuries
============

The *ustreasuries* package downloads daily Constant-Maturity Treasury (CMT) yields and provides visualizations & analytics that use that data including all the 'greeks' for derivative analysis.

### Vignettes

-   **cmt-rates** a description of Constant Maturity and Annualized Percentage rates
-   **yield-curves** examples of downloading the data and printing yield curves for interesting periods in recent financial history
-   **plot-10year** a plot of the 10-year from 1962 to present

### Functions

-   **USTreasuryRates** downloads a data.frame with daily data from 1962
-   **PrintYieldCurves** prints one or more yield curves
-   **APY** converts Constant-Maturity Treasury (CMT) yields to Annualized Percentage Yields (APY)

Installation
------------

Get the development version from github:

``` r
# install.packages("devtools")
devtools::install_github("grfiv/treasuries")

# Note: if you receive a message about corrupt databases or fetch(key), 
#       restarting R will fix the problem;
#       these appear to be issues with devtools 1.10.0.9000
```
