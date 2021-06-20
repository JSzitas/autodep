#' Reformat from library calls
#' @description Replace calls to library with namespace::functions
#' @param file The file to fix the calls in.
#' @param outfile The file to write the output by default - defaults to **NULL** and does not write
#' to a file.
#' @param consider_loaded_namespaces Look for potential matches to function in loaded namespaces.
#' @param remove_lib_calls Remove the calls to library/require after replacements are done.
#' @return The fixed code (as a string).
#'
#' @export
fix_library_calls <- function(file, outfile = NULL, consider_loaded_namespaces = FALSE, remove_lib_calls = FALSE) {
  matches <- sourcetools::tokenize_file(file)
  matches <- matches[matches$type != "whitespace", ]

  libs <- which((matches$value == "library" | matches$value == "require") &
    matches$type == "symbol")

  # we need to keep track of where the libraries were invoked - so we can adjust
  # the search to reflect their position - ie if you invoke a library which
  # overwrote, say, a function, this should be reflected
  cutoffs <- matches[libs, "row"]
  libs <- matches[libs + 2, "value"]
  # invert the order of library calls so we can use the first match for a function
  libs <- libs[length(libs):1]
  # list all objects in each library
  all_namespace_objects <- lapply(libs, function(lib) ls(asNamespace(lib)))
  if (consider_loaded_namespaces) {
    all_namespace_objects <- c(
      all_namespace_objects,
      lapply(loadedNamespaces(), function(lib) {
        ls(asNamespace(lib))
      })
    )
    libs <- c(libs, loadedNamespaces())
    cutoffs <- c( cutoffs, rep(0, length(loadedNamespaces())) )
  }

  names(all_namespace_objects) <- libs
  # we do not need to do bookkeeping for objects exported from base
  all_namespace_objects <- all_namespace_objects[ !(names(all_namespace_objects) == "base") ]

  # ignore symbols which were assigned to at some point in evaluation of script -
  # we cannot prevent cross-script dependencies this way, though
  are_symbols <- which(matches$type == "symbol")
  # # the cases where we have symbols followed by an operator typically mean "<-"
  # # or "="
  # not_followed_by_operator <- which( !matches[ are_symbols + 1,"type"] == "operator")
  # followed_by_operator <- which( matches[ are_symbols + 1,"type"] == "operator")
  all_symbols <- matches[  #intersect( are_symbols, not_followed_by_operator) ,
    are_symbols,
    c("value", "row","column")
  ]
  # convert the namespace objects to a data.frame so the lookup is a bit easier
  all_namespace_objects <- lapply(seq_len(length(all_namespace_objects)), function(i) {

    lib <- names(all_namespace_objects)[i]
    exported_objects <- all_namespace_objects[[i]]
    if( length(exported_objects) == 0 )
      return(NULL)

    data.frame( lib, exported_objects, cutoffs[i])
  })

  all_namespace_objects <- do.call(rbind, all_namespace_objects)
  names(all_namespace_objects) <- c("pkg", "object", "available_from")

  all_matches <- intersect(all_namespace_objects[, "object"], all_symbols[, "value"])
  matched_calls <- all_namespace_objects[ all_namespace_objects$object %in% all_matches,]
  all_matches <- all_symbols[ all_symbols$value %in% all_matches,]
  matched_calls[["object"]] <- as.character(matched_calls[["object"]])
  matched_calls[["pkg"]] <- as.character(matched_calls[["pkg"]])

  matched_calls <- merge( matched_calls,
                          all_matches,
                          by.x = "object",
                          by.y = "value")

  matched_calls[["replacement"]] <- paste0( matched_calls[["pkg"]],
                                            "::",
                                            matched_calls[["object"]] )

  original_file <- readLines( file )

  for( row in seq_len(nrow(matched_calls)) )
  {
    # which line are we replacing?
    line <- matched_calls[ row, "row" ]
    # where does the token we are replacing start?
    # (ie which character in the string does it start at)
    char_start <- matched_calls[ row, "column" ]
    # how many characters are we replacng?
    char_length <- nchar( matched_calls[ row, "object" ] )

    original_line <- original_file[ line ]

    pre_replacement_point <- substr(original_line, 1, char_start-1)
    post_replacement_point <- substring( original_line, char_start + char_length)

    original_file[ line ] <- paste0( pre_replacement_point,
                                     matched_calls[row, "replacement"],
                                     post_replacement_point )
  }

  if(remove_lib_calls)
  {
    original_file <- gsub( pattern = "(library|require)\\(.*\\)",
                           replacement = "",
                           x = original_file)
  }

  if( !is.null( outfile ) )
  {
    cat( paste0(original_file,"\n"), file = outfile )
    temp <- tempfile()
    sink( temp )
    styler::style_file( outfile )
    unlist( temp )
    sink(NULL)
  }

  invisible( original_file )
}
