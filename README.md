
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tefasr

<!-- badges: start -->
<!-- badges: end -->

This package enables you to fetch historical data of public investment
funds in Turkey from [Turkey Electronic Fund Trading
Platform](http://www.fundturkey.com.tr).

## Installation

You can install the development version of tefasr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("can-taslicukur/tefasr")
```

## Example

You can get general information such as price for a given fund type
within a date range like this.

``` r
library(tefasr)
dat <- historical_general_information(
  fund_type = "YAT",
  start_date = as.Date("2021-10-01"),
  end_date = as.Date("2021-10-01")
)
```
