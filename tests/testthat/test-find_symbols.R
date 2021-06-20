test_that("Finding symbols works", {
  expect_equal(find_symbols("test_symbols.R"), c("%>%", ":="))

  expect_equal(as.character(register_symbols("%>%")$funs), "%>%")
  expect_equal(as.character(register_symbols("%>%")$libs), "magrittr")

  expect_equal(as.character(register_symbols(c("%>%", ":="))$funs),
               c("%>%", ":="))
  expect_equal(as.character(register_symbols(c("%>%",":="))$libs),
               c("magrittr","data.table"))
})
