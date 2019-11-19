<!-- README.md is generated from README.Rmd. Please edit that file -->

# faahydro

<!-- badges: start -->

<!-- badges: end -->

faahydro is an R package for fetching, processing, and analyzing
hydrologic and water quality data. This package was created to support
work in South Florida by Federico & Associates, Inc.

## Installation

This package is currently only available on github. Use the
`devtools::install_github()` function to install it:

``` r
# install.packages("devtools")
devtools::install_github("walkerjeffd/faahydro")
```

Then load the package

``` r
library(faahydro)
```

## Fetching Data from DBHYDRO

The `dbhydro_get_*()` functions are used to fetch hydrologic and water
quality data from DBHYDRO. These functions call the respective `get_*()`
functions from the `dbhydroR` package, and then perform some additional
cleaning of the dataset (e.g., parsing dates, removing duplicate
columns).

``` r
# Not Run
dbhydro_get_hydro(
  dbkeys = "91599",
  date_min = "2019-10-01",
  date_max = "2019-10-31"
)
dbhydro_get_wq(
  station_ids = "LOX3",
  date_min = "2019-09-01",
  date_max = "2019-09-30",
  test_name = "PHOSPHATE, TOTAL AS P"
)
```
