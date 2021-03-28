#' Automate package dependency writing
#'
#' @description Automatically register namespace::function calls
#'
#' @param path The path to the package - defaults to ".".
#' @param overwrite Whether to overwrite existing dependencies - defaults to **FALSE**.
#' @param roxygen_file_name The **.R** file to render the roxygen into - by default
#' **"R/package_imports.R"**. Note that the specification of the **/R** project
#' subdirectory is necessary.
#' @details This function automatically writes package dependencies and generates
#' roxygen tags to import necessary dependencies for a package, specified via
#' the namespace::function convention. Write your code like you always would, and
#' let this handle the boring work.
#' The dependencies are written to the description file into the **Imports** section -
#' where warranted, you can easily change this manually, but it seems as a reasonable default.
#' @export
# nocov start
autodep <-
  function(path = ".",
           overwrite = FALSE,
           roxygen_file_name = "R/package_imports.R")
  {
    # the R folder for available files
    filepaths <-
      list.files(path = paste0(path, "/R"), full.names = TRUE)

    # scan all package R files for potential imports
    all_file_imports <- lapply(filepaths, find_imports)
    # since this is a list of data.frames, we can just rbind them
    all_file_imports <- do.call(rbind, all_file_imports)
    # now simply deduplicate
    all_file_imports <- unique(all_file_imports)
    # write the imports first
    roxygen <- imports_to_roxygen(all_file_imports)
    write_imports(
      roxygen_imports = roxygen,
      pkg_import_file = roxygen_file_name,
      overwrite = overwrite
    )
    # write dependencies into the description file
    write_dependencies(all_file_imports)
  }
# nocov end
