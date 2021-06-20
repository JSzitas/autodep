
test_fun <- function()
{
  iris %>%
    unlist()

  cars <- data.table::as.data.table(cars)

  cars[, dist_sqrd := dist^2  ]
}
