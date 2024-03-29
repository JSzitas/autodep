
<!-- README.md is generated from README.Rmd. Please edit that file -->

# autodep

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![R-CMD-check](https://github.com/JSzitas/autodep/workflows/R-CMD-check/badge.svg)](https://github.com/JSzitas/autodep/actions)
[![Codecov test
coverage](https://codecov.io/gh/JSzitas/autodep/branch/main/graph/badge.svg)](https://codecov.io/gh/JSzitas/autodep?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/autodep)](https://CRAN.R-project.org/package=autodep)
<!-- badges: end -->

**autodep** scans a package /R directory and finds all valid
dependencies of the form namespace::object. These are then rendered as
roygen tags in the /R directory (so they are easy to read) and the
appropriate packages are written to the description file (into the
**Imports** section).

## Installation

Install easily from github via:

``` r
devtools::install_github("JSzitas/autodep")
```

## Usage

Use **autodep** to automatically convert namespace::function calls to
roxygen tags, and register the required packages in the description
file.

``` r
autodep(path = ".", overwrite = TRUE, roxygen_file_name = "R/package_imports.R")
```

You just need to set the path to the package (via **path**), specify
**overwrite = TRUE** (in case you have previously used autodep and wish
to upgrade the imports) and give it the name of the file to write the
roxygen tags to - by default **“R/package\_imports.R”**

Note that the roxygen tags should always be placed in the **/R**
project/package sub-directory.
