#' Automate package dependency writing
#'
#' @description Automatically register namespace::function calls
#'
#' @param path The path to the package - defaults to ".".
#' @param overwrite Whether to overwrite existing dependencies - defaults to **FALSE**.
#' @param register_symbols Whether to try to register symbols such as the **magrittr `%>%`**,
#' or the **data.table `:=`**. Highly experimental, and by default **FALSE**.
#' @param roxygen_file_name The **.R** file to render the roxygen into - by default
#' **"R/package_imports.R"**. Note that the specification of the **/R** project
#' subdirectory is necessary.
#' @param ignore_base_package Whether to ignore the base package imports (this is an option only
#' for the corner cases where someone **might** want to import the base environment, which
#' we anticipate will never happen). Defaults to **TRUE**.
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
           register_symbols = FALSE,
           roxygen_file_name = "R/package_imports.R",
           ignore_base_package = TRUE)
  {
    # the R folder for available files
    filepaths <-
      list.files(path = paste0(path, "/R"), full.names = TRUE)
    filepaths <- filepaths[ grep(pattern = "\\.R$", x = filepaths) ]

    # scan all package R files for potential imports
    all_file_imports <- lapply(filepaths, find_imports, ignore_package_base = ignore_base_package)
    # since this is a list of data.frames, we can just rbind them
    all_file_imports <- do.call(rbind, all_file_imports)

    if(register_symbols)
    {
      all_file_symbols <- unique(sapply(filepaths, find_symbols))
      all_file_symbols <- register_symbols( all_file_symbols )
      all_file_imports <- rbind( all_file_imports, all_file_symbols )
    }

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
    write_dependencies(all_file_imports, "Imports")

    # similar for tests
    filepaths <- list.files(path = paste0(path, "/tests/testthat"), full.names = TRUE)
    filepaths <- filepaths[ grep(pattern = "\\.R$", x = filepaths) ]
    all_file_imports <- lapply(filepaths, find_imports, ignore_package_base = ignore_base_package)
    if( length(all_file_imports) == 0 ) {
      return(invisible())
    }
    # since this is a list of data.frames, we can just rbind them
    all_file_imports <- do.call(rbind, all_file_imports)
    all_file_imports <- unique(all_file_imports)
    # write dependencies into the description file
    write_dependencies(all_file_imports, "Suggests")
    invisible()
  }
# nocov end
