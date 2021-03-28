test_that("Find imports in a file works", {
  found_imports <- find_imports("./test_file.R")

  expect_true(is.data.frame(found_imports))
  expect_equal(names(found_imports), c("libs", "funs"))
  expect_equal(nrow(found_imports), 4)

  expect_equal(as.character(found_imports$libs),
               rep("data.table", 4))
  expect_equal(as.character(found_imports$funs),
               c(
                 "data.table", "setkey", "as.data.table", "frollmean"
               ))
})

test_that("Converting imports to roxygen works", {
  found_imports <- find_imports("./test_file.R")
  imports <- imports_to_roxygen(found_imports)

  test_roxygen <- readLines("./test_roxygen.R")

  expect_equal(imports, test_roxygen)
})
