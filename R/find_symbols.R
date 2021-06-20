

find_symbols <- function( file )
{
  tokens <- sourcetools::tokenize_file(file)
  # get rid of non-operators
  operators <- unlist(tokens[tokens$type == "operator","value"])
  operators <- unique( operators )

  foreign <- unlist( sapply(loadedNamespaces(),
                    function(i){
                      ns <- asNamespace(i)
                      if( !isBaseNamespace(ns)){
                        return(ls(ns))
                      }
                      return()
                    }))
  # either the operator is not found, or it is a foreign operator
  operators <- operators[ !sapply(operators, exists) | operators %in% foreign ]
  return( operators )
}

register_symbols <- function( symbols )
{

  symbol_table <- data.frame( funs = c("%>%",":=",".SD"),
                              libs = c("magrittr","data.table","data.table")  )
  # return the symbols to register - this can be conveniently used with
  # imports_to_roxygen
  return( symbol_table[ which(symbol_table$funs %in% symbols),] )
}
