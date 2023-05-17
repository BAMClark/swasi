#' Bar plot
#'
#'
#'
#' This function creates an output of bar plots.
#'
#'
#' @param df1,year,path Input dataframe, year, and path is default to current
#' @return bar plots
#' @export
bar_fun <- function(df, year, index, path = "output/EDA/plots/", folder = "assessment/", na.rm = T, ...) {

  nm <- names(df)

  for(i in seq_along(nm)) {
    bars <- ggplot(df, aes_string(nm[i])) +
      stat_count(width = 1) +
      coord_flip()

    ggsave(
      bars,
      filename = paste0(
        path,
        folder[index],
        nm[i],
        "_bl_",
        year,
        ".png"
      )
    )
  }
}

#' Histogram
#'
#'
#'
#' This function creates an output of hist plots.
#'
#'
#' @param df1,year,path Input dataframe, year, and path is default to current
#' @return histogram plots
#' @export
hist_fun <- function(df, year, index, path = "output/EDA/plots/", folder = "assessment/", na.rm = T, ...) {

  nm <- names(df)

  for(i in seq_along(nm)) {
    histos <- ggplot(df, aes_string(nm[i])) +
      geom_histogram(binwidth = 1)

    ggsave(
      histos,
      filename = paste0(
        path,
        folder[index],
        nm[i],
        "_bl_",
        year,
        ".png"
      )
    )
  }
}
