# wildstew
Wilderness stewardship performance reporting.


## What

Support tools for Wilderness stewardship based on air quality information.


## Why

Each Wilderness area managed by the USDA Forest Service is required to report annually on progress toward “wildernesses meeting baseline performance for preserving wilderness character”, otherwise known as Wilderness Stewardship Performance (WSP).  Among other elements, managers may report on Wilderness Air Quality Values (WAQVs) as a marker of natural quality of Wilderness character.  However, until now we have lacked a common template for nationally consistent, accurate, and timely reporting of WAQVs.  Here, we provide such a template to streamline annual WSP reporting for WAQVs based on lichen attributes.


## Installation

Install the package from github as follows:
```r
# Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true") # avoid errors
install.packages('devtools')
devtools::install_github('phytomosaic/wildstew')
```


## Further information

```r
require(wildstew)    # load the package
citation('wildstew') # please cite in publications
?est                 # find a help file
```
