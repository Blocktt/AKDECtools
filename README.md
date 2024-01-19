
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

The vignette (big help file) isn’t created when installing from GitHub
with the basic `install_github` command. If you want the vignette
install with the code below.

``` r
if(!require(remotes)){install.packages("remotes")}  #install if needed
#> Loading required package: remotes
install_github("Blocktt/AKDECtools", force=TRUE)
#> Downloading GitHub repo Blocktt/AKDECtools@HEAD
#> rlang (1.1.2 -> 1.1.3) [CRAN]
#> glue  (1.6.2 -> 1.7.0) [CRAN]
#> withr (2.5.2 -> 3.0.0) [CRAN]
#> Installing 3 packages: rlang, glue, withr
#> package 'rlang' successfully unpacked and MD5 sums checked
#> Warning: cannot remove prior installation of package 'rlang'
#> Warning in file.copy(savedcopy, lib, recursive = TRUE): problem copying
#> C:\Users\ben.block\AppData\Local\Programs\R\R-4.3.2\library\00LOCK\rlang\libs\x64\rlang.dll
#> to
#> C:\Users\ben.block\AppData\Local\Programs\R\R-4.3.2\library\rlang\libs\x64\rlang.dll:
#> Permission denied
#> Warning: restored 'rlang'
#> package 'glue' successfully unpacked and MD5 sums checked
#> Warning: cannot remove prior installation of package 'glue'
#> Warning in file.copy(savedcopy, lib, recursive = TRUE): problem copying
#> C:\Users\ben.block\AppData\Local\Programs\R\R-4.3.2\library\00LOCK\glue\libs\x64\glue.dll
#> to
#> C:\Users\ben.block\AppData\Local\Programs\R\R-4.3.2\library\glue\libs\x64\glue.dll:
#> Permission denied
#> Warning: restored 'glue'
#> package 'withr' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\ben.block\AppData\Local\Temp\RtmpMLouDN\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\ben.block\AppData\Local\Temp\RtmpMLouDN\remotes5d241de23ee2\Blocktt-AKDECtools-58dd72f/DESCRIPTION' ...  ✔  checking for file 'C:\Users\ben.block\AppData\Local\Temp\RtmpMLouDN\remotes5d241de23ee2\Blocktt-AKDECtools-58dd72f/DESCRIPTION'
#>       ─  preparing 'AKDECtools': (1.3s)
#>    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#> ─  checking for empty or unneeded directories
#>   ─  building 'AKDECtools_0.0.1.9040.tar.gz'
#>      
#> 
```

## Purpose

AKDECtools provides various functions that support water quality
analyses frequently undertaken by the AK DEC’s [Water Quality Monitoring
and Assessment
Program](https://dec.alaska.gov/water/water-quality/monitoring-and-assessment/).

## Status

In development.
