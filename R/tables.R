
#' Create quality control
#'
#'
#' This function creates a quality control dataframe. User has the option to use
#' the full dataframe or filter based on the dataframe of students who have
#' consented to using their data for research. The consent_df argument is optional.
#'
#' @param df,year,consent_df,path Input dataframe, lower_bound defaults to 3, upper_bound defaults to 95
#' @return df with additional consent variable
#' @export
quality_control <- function(df, year, consent_df = NULL, path = "") {
  qc <- df %>%
    select(
      id_bl, cohort, start_date:browser_resolution,
      starts_with("t"),
      fq_d, fq_td, mccm,
      pbi_1:pbi_3, pbs_1:pbs_3,
      status, cond
    ) %>%
    mutate(
      mccm_correct = case_when(
        is.na(cond) ~ NA_character_,
        cond == "Control" & mccm == 2 ~ "correct",
        str_detect(cond, "Difference") & mccm == 4 ~ "correct",
        cond == "Social-Belonging" & mccm == 1 ~ "correct",
        !is.na(mccm) ~ "incorrect"
      )
    ) %>%
    select(-cond)

  if (is.null(consent_df)) {
    joined <- qc
  } else {
    shared_colnames <- names(consent_df)[names(consent_df) %in% names(qc)]
    shared_coltypes <- sapply(consent_df[,shared_colnames], class)

    for (n in shared_colnames) {
      class(qc[,n]) <- shared_coltypes[n]
    }

    joined <- left_join(consent_df, qc)
  }

  write_csv(
    joined,
    paste0(
      path,
      "qc_bl_",
      year,
      ".csv"
    ))
}
