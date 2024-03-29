# find package imports, using a simple tokenization scheme
find_imports <- function(file, ignore_package_base = TRUE)
{
  tokens <- sourcetools::tokenize_file(file)
  # get rid of whitespace and comments
  tokens <-
    tokens[!(tokens$type %in% c("comment", "whitespace", "string")),]
  # this gives us the line numbers where the :: operator is found
  potential_matches <-
    unique(tokens[tokens$value == "::", "row"])
  # from that we can quickly get the tokens around it
  matches <- which(tokens$value == "::")
  libs <- tokens[matches - 1, "value"]
  funs <- tokens[matches + 1, "value"]

  # turn the requirements into a data.frame
  requirements <- data.frame(libs = libs, funs = funs)
  # optionally filter out base package imports
  if( ignore_package_base ) {
    requirements <- requirements[ which(libs != "base"), ]
  }
  return(requirements)
}
# convert parsed imports into roxygen tags
imports_to_roxygen <- function(imports)
{
  return(paste0("#' @importFrom ", imports$libs, " ", imports$funs))
}
