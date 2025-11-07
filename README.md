
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AKDECtools

Tools to support Water Quality Analyses by the Alaska Department of
Environmental Conservation (AK DEC).

## Badges

<!-- badges: start -->

[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://github.com/Blocktt/AKDECtools/graphs/commit-activity)
[![GitHub
license](https://img.shields.io/github/license/Blocktt/AKDECtools)](https://github.com/Blocktt/AKDECtools/blob/main/LICENSE)
[![GitHub
issues](https://img.shields.io/github/issues-raw/Blocktt/AKDECtools)](https://github.com/Blocktt/AKDECtools/issues)
[![Github all
releases](https://img.shields.io/github/downloads/Blocktt/AKDECtools/total)](https://github.com/Blocktt/AKDECtools/releases)
<!-- badges: end -->

## Installation

To install the current version use the code below to install from
GitHub. The use of “force = TRUE” ensures the package is installed even
if already present. If the package `remotes` is missing the code below
will install it.

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
install_github("Blocktt/AKDECtools", force=TRUE)
```
A [vignette](https://github.com/Blocktt/AKDECtools/blob/main/vignettes/AKDECtools.Rmd) has been created as a guide to the package functions. The vignette can be installed with the package to be viewed within the R environment using the code below.

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
install_github("Blocktt/AKDECtools", force=TRUE, build_vignettes=TRUE)
```

## Purpose

AKDECtools provides various functions that support water quality
analyses frequently undertaken by the AK DEC’s [Water Quality Monitoring
and Assessment
Program](https://dec.alaska.gov/water/water-quality/monitoring-and-assessment/).

## Status

Package completed by Tetra Tech. AKDEC may update in the future.
