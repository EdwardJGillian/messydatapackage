messydataPackage
================
Edward Gillian
23/03/2021

-----

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

The `messydatapackage` has a number of goals:

Firstly, this package uses different `tidyverse` techniques such as
`pivot_longer` to group employee data in 2 semi-long formats as the data
for salary and commuting have different factor levels.

Secondly, `tidyverse` techniques are used to create contingency tables
for calculating statistics and graphing plots. Different plotting
functions are used including `ggplot2`, `corrplot`, and `base plot`.

The plots are displayed on a tabbed summary page in `R Shiny`.

Thirdly, `R Shiny` reactive elements are included for the `corrplot`
plots to allow for custom data visualisations. Also, reactive elements
are included to let the user choose the dependent variables and
independent variables for the logistic regression models.

Finally, automated testing is done through chained test functions using
`testthat`. These functions allow the developer to add different input
files to test the functions stored in the `R` folder to be tested for
reliable outputs. The functions use `expect_known_value` to generate the
test outputs.

## Installation

You can install the released version of `messydatapackage` with:

``` r
devtools::install_github("EdwardJGillian/messydatapackage")
```
