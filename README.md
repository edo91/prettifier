
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `prettifier`

The goal of `prettifier` is to clean up strings.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edo91/prettifier")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(prettifier)
```

``` r

x <- "a1~!#$€%^&*|__+:____()[]{}<>?,./;:'-=òìàèéù dASFGHRV"

prettify_string(x)
#> [1] "a1_oiaeeu_dasfghrv"

# custom replacement
prettify_string(x = "5%_in_€")
#> [1] "5_in_"
prettify_string(x = "5%_in_€", "%" = "perc")
#> [1] "5perc_in_"
prettify_string(x = "5%_in_€", "%" = "perc", "€" = "eur")
#> [1] "5perc_in_eur"
```
