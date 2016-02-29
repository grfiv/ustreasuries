This is the initial submission of the *ustreasuries* package.
---

## Test environments
* OS X, R 3.2.3
* Ubuntu 15.10, R 3.2.3
* devtools::build_win()

## R CMD check results

* For devtools::check() locally, there were no ERRORs, WARNINGs or NOTEs.

* For devtools::build_win() there is consistently one NOTE   
    * *checking CRAN incoming feasibility ... NOTE   
    Maintainer: 'George Fisher <george@georgefisher.com>'   
    New submission   
    Version contains large components (0.0.0.9000)*   
    
        Sometimes this is accompanied by
        * *Found the following (possibly) invalid URLs:
              URL: http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData   
                From: man/CMTrates.Rd   
                Status: 501   
                Message: Not Implemented*   
           **The URL is in fact implemented**   
    
       
    * There was also this remark: "*Examples with CPU or elapsed time > 5s*"    
    I am surprised it took so long, but there is one real-time data download    
    function that retrieves a lot of data and adds it to a large static    
    private file   

## Downstream dependencies

There are none.
