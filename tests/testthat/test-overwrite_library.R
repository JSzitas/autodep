test_that("Replacing works", {

  expected <- c("", "", "", "library( dplyr)", "  require(ggplot2 )", "# library( tidyr)",
                "", "", "df <- iris %>%", "    dplyr::mutate( flower_volume = Sepal.Length*Sepal.Width + Petal.Length*Petal.Width ) %>%",
                "    dplyr::filter( Species %in% c(\"setosa\",\"versicolor\") )",
                "", "ggplot2::ggplot( df, ggplot2::aes(x=flower_volume, color = Species, fill = Species) ) +",
                "  ggplot2::geom_density( alpha = 0.5)")
  actual <- fix_library_calls( "./test_lib_call.R",
                               consider_loaded_namespaces = FALSE,
                               remove_lib_calls = FALSE)

  expect_identical(actual, expected )
})

test_that("Replacing also the library calls works", {

  expected <- c("", "", "", "", "  ", "# ",
                "", "", "df <- iris %>%", "    dplyr::mutate( flower_volume = Sepal.Length*Sepal.Width + Petal.Length*Petal.Width ) %>%",
                "    dplyr::filter( Species %in% c(\"setosa\",\"versicolor\") )",
                "", "ggplot2::ggplot( df, ggplot2::aes(x=flower_volume, color = Species, fill = Species) ) +",
                "  ggplot2::geom_density( alpha = 0.5)")
  actual <- fix_library_calls( "./test_lib_call.R",
                               consider_loaded_namespaces = FALSE,
                               remove_lib_calls = TRUE)

  expect_identical(actual, expected )
})

test_that( "Writing to an outfile works", {

  on.exit(fs::file_delete("temp-outfile.R"))

  expected <- c("", "", "", "", "", "#", "", "", "df <- iris %>%", "  dplyr::mutate(flower_volume = Sepal.Length * Sepal.Width + Petal.Length * Petal.Width) %>%",
                "  dplyr::filter(Species %in% c(\"setosa\", \"versicolor\"))",
                "", "ggplot2::ggplot(df, ggplot2::aes(x = flower_volume, color = Species, fill = Species)) +",
                "  ggplot2::geom_density(alpha = 0.5)")
  fix_library_calls( "./test_lib_call.R",
                     outfile = "temp-outfile.R",
                     consider_loaded_namespaces = FALSE,
                     remove_lib_calls = TRUE)

  expect_true( fs::file_exists("temp-outfile.R") )
  expect_equal( readLines( "temp-outfile.R" ), expected )

})
