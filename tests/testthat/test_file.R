# test_file no.1

whatever <- function()
{
  dt <- data.table::data.table( a = 5, b = 3 )
  # data.table::fread might be better
  data.table::setkey(dt, "a")

  cat( "data.table::fcase and data.table::between are the coolest" )
  do.call( data.table::as.data.table, list( a = 5, b = 3 ) )
  "data.table::fwrite is cool"; data.table::frollmean()

}
