# write parsed roxygen tags to a file, by default the package_imports.R file
write_imports <-
  function(roxygen_imports,
           path = ".",
           pkg_import_file = "R/package_imports.R",
           overwrite = FALSE,
           add = FALSE)
  {
    pkg_import_filepath <- paste0(path, "/", pkg_import_file)
    if (file.exists(pkg_import_filepath))
    {
      cli::cli_alert("The pkg_import_file already exists at {pkg_import_filepath}")
      if (!overwrite)
      {
        cli::cat_bullet(
          "Cannot overwrite existing file - ",
          "please set overwrite to TRUE after checking your file.",
          bullet = "cross",
          col = "red"
        )
        return(invisible())
      }
    }
    else
    {
      file.create(pkg_import_filepath)
    }

    cat(roxygen_imports, file = pkg_import_filepath, sep = "\n")

    if (all.equal(readLines(pkg_import_filepath), roxygen_imports))
    {
      cli::cat_bullet("Import file written successfully!",
                      bullet = "tick",
                      col = "green")
    }
    else
    {
      cli::cat_bullet(
        "Failed to write import file - ",
        "please check if you have write rights to folder {pkg_import_filepath}",
        col = "red",
        symbol = "cross"
      )
    }
  }
# nocov start
write_dependencies <- function(imports, type = "Imports")
{
  libs <-
    data.frame(
      type = type,
      package = unique(imports$libs),
      version = "*"
    )
  existing_libs <- desc::desc_get_deps()
  # bind new with previously existing dependencies
  all_libs <- rbind(libs, existing_libs)
  # write the unique ones to the DESCRIPTION file
  desc::desc_set_deps(unique(all_libs))
}
# nocov end
