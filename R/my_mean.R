#' Calculate a Mean
#'
#' This function functions as the mean function, only calculates the mean with
#' NAs removed. If data has NAs, there will no longer be an error.
#'
#' @param x Input list or set of values
#' @return mean of x
#' @export
my_mean <- function(x){mean(x, na.rm = TRUE)}
