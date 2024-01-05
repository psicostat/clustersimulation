
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Cluster Simulation

<!-- badges: start -->
<!-- badges: end -->

The goal of `clustersimulation` is to provide a set of utilities
functions and a Shiny app to perform Monte Carlo simulations with a
focus on inferential errors (type 1 error) and statistical power. The
simulation can be performed either providing a dataset or manually
setting all required parameters.

## Installation

You can install the development version of clustersimulation from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("filippogambarota/clustersimulation")
```

Sometimes a restart of R/R Studio could be required to correctly load
the package after the installation.

Once the package is installed the Shiny app (running locally) can be
used using:

``` r
library(clustersimulation)
run_shiny()
```

The `R/` folder contains all the functions used in the Shiny that can be
used also within standard R script to implement more complex and
extensive simulations.

A demo version of the app can be accessed online at
<https://psicostat.shinyapps.io/clustersimulation-demo/>
