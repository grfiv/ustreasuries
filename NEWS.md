# ustreasuries 0.0.0.9001   

-------------------------

## Initial Development Release 0.0.0.9001

* ### Modified *yield-curves* vignette to show last three years

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

