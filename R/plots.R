#' Bar plot
#'
#' This function creates an output of bar plots.
#'
#'
#' @param df,cat_vars,year,path Input dataframe, vector of variable names, year, and path is default to "output/EDA/plots/barplots/"
#' @return bar plots
#' @export
bar_fun <- function(df, cat_vars, year, path = "output/EDA/plots/barplots/", na.rm = T, ...) {

  cat_vars <- df %>%
    select(
      all_of(
        cat_vars
      )
    )

  nm <- names(cat_vars)

  for(i in seq_along(nm)) {
    bars <- ggplot(df, aes_string(nm[i])) +
      stat_count(width = 1) +
      coord_flip()

    ggsave(
      bars,
      filename = paste0(
        path,
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
#' This function creates an output of hist plots.
#'
#'
#' @param df, quant_vars,year,path Input dataframe, vector of variable names, year, and path is default to "output/EDA/plots/histograms/"
#' @return histogram plots
#' @export
hist_fun <- function(df, quant_vars, year, path = "output/EDA/plots/histograms/", na.rm = T, ...) {

  df <- df %>%
    mutate(across(all_of(quant_vars), ~as.numeric(.x)))

  quant_vars <- df %>%
    select(
      all_of(
        quant_vars
      )
    )

  nm <- names(quant_vars)

  for(i in seq_along(nm)) {
    histos <- ggplot(df, aes_string(nm[i])) +
      geom_histogram(binwidth = 1)

    ggsave(
      histos,
      filename = paste0(
        path,
        nm[i],
        "_bl_",
        year,
        ".png"
      )
    )
  }
}
