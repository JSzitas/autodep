test_that("Writing a pkg imports file works", {
  # equivalent to what should be produced by find_imports from the test_file.R
  test_data <- readLines("./test_roxygen.R")

  # write file
  write_imports(test_data, pkg_import_file = "test_pkg_imports.R")
  expect_true(file.exists("./test_pkg_imports.R"))

  test_file <- readLines("./test_pkg_imports.R")

  expect_equal(
    test_file,
    c(
      "#' @importFrom data.table data.table",
      "#' @importFrom data.table setkey",
      "#' @importFrom data.table as.data.table",
      "#' @importFrom data.table frollmean"
    )
  )

  # on no overwrite we return a NULL invisibly - this may seem weird,
  # but then the behaviour is NOT an error, and is to be expected
  no_overwrite <- write_imports(test_data,
                                pkg_import_file = "test_pkg_imports.R",
                                overwrite = FALSE)
  expect_equal(NULL, no_overwrite)

  # this should basically erase the file
  cat("", file = "./test_pkg_imports.R")

  write_imports(test_data,
                pkg_import_file = "test_pkg_imports.R",
                overwrite = TRUE)

  test_file <- readLines("./test_pkg_imports.R")

  expect_equal(
    test_file,
    c(
      "#' @importFrom data.table data.table",
      "#' @importFrom data.table setkey",
      "#' @importFrom data.table as.data.table",
      "#' @importFrom data.table frollmean"
    )
  )
  # clean up
  file.remove("./test_pkg_imports.R")
})
