# generate a set of nice things for a new repository

install_if_unavailable <- function( pkg_name )
{
  if(!require(pkg_name, character.only = TRUE))
  {
    install.packages(pkg_name)
  }
}

install_if_unavailable("usethis")

# nice travis CI with covr:
generate_travis <- function( pkg_name = NULL, covr = TRUE )
{
  if(is.null( pkg_name ))
  {
    # get package name from current active project
    pkg_name <- regmatches( rstudioapi::getActiveProject(),
                regexpr( pattern = "[A-z0-9]*$", rstudioapi::getActiveProject()))
  }

  code <- paste0("# Use R\nlanguage: r\ncache: packages\n",
                 "warnings_are_errors: FALSE\n\n",
  ifelse(covr, "r_packages:\n  - covr \n\n",NULL),
  "# environment variables set for all builds\nenv:\n",
  "  global:\n    - R_BUILD_ARGS=\"--no-build-vignettes --no-manual\"\n",
  "    - R_CHECK_ARGS=\"--no-build-vignettes --no-manual --timings\"  ## do not build vignettes or manual\n",
  "    - _R_CHECK_TIMINGS_=\"0\"  ## get the timing information for the examples for all of your functions\n\n",
  "r:\n  - release\n\n",
  "# do not build vignettes...takes too long and times out on travis\n",
  "r_build_args: --no-build-vignettes --no-manual\n",
  "r_check_args: --no-build-vignettes --no-manual --timings\n\n",
  "script:\n  - |\n    R CMD build .\n    travis_wait 40 R CMD check ", pkg_name,"*tar.gz\n\n",
  "notifications:\n  email:\n  on_success: change\n  on_failure: change\n\n",
  ifelse(covr, "after_success:\n - Rscript -e 'library(covr); codecov()'\n",NULL))
  file.create( "./.travis.yml" )
  cat( code , file = "./.travis.yml" )
}

make_nice_pkg <- function(stage = "experimental", covr = TRUE)
{
  usethis::use_mit_license()
  usethis::use_git()
  usethis::use_github()

  usethis::use_appveyor()
  generate_travis(covr = covr)
  usethis::use_lifecycle_badge(stage = stage)
  usethis::use_travis_badge()
  usethis::use_appveyor_badge()
  usethis::use_coverage()
  usethis::use_github_action_check_standard()
  usethis::use_cran_badge()
  file.remove("./codecov.yml")
}
