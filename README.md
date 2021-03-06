
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fungarium

<!-- badges: start -->

![GitHub](https://img.shields.io/github/license/hjsimpso/fungarium)
![Github
commits](https://img.shields.io/github/commits-since/hjsimpso/fungarium/latest)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/hjsimpso/fungarium/graphs/commit-activity)

<!-- badges: end -->

An R package for enabling the use of fungaria collections data in
comprehensive trait analyses.

## Installing fungarium

``` r
install.packages("remotes") #install 'remotes' (if not already installed)
remotes::install_github("hjsimpso/fungarium@*release") #install the latest fungarium release
#to install the latest development version use:
remotes::install_github("hjsimpso/fungarium@main")
```

## Installing Docker

[Docker](https://docs.docker.com/get-started/overview/) needs to be
installed on your system in order to utilize the `mycoportal_tab`
function. All other functions in the **fungarium** package do not
require Docker, so if you do not wish to utilize `mycoportal_tab` you do
not need to install Docker.

Instructions for installing Docker (available for Linux platforms,
macOS, and Windows 10): <https://docs.docker.com/engine/install/>

## Examples and detailed guidance on package usage

See [fungarium book](https://hjsimpso.github.io/fungarium_book/).

## Meta

-   Get citation information for fungarium in R via
    `citation(package = 'fungarium')`
-   This package is released with a [Contributor Code of
    Conduct](https://github.com/hjsimpso/fungarium/blob/main/CODE_OF_CONDUCT.md).
    By contributing to this project, you agree to abide by its terms.
-   Please report any bugs.
