
#' Create codebook/ key file
#'
#' Step 1
#'
#' This function creates a codebook with the first two rows from the survey,
#' which has the name and description of each variable. This function
#' should be run first before any cleaning or manipulation.
#'
#' @param df,year,path Input dataframe, year of analysis, and path to save file
#' @return codebook dataframe
#' @export
create_codebook <- function(df, year, path = ""){
  key_code = data.frame(unlist(colnames(df)), unlist(df[1, ], use.names = FALSE))
  names(key_code) = c("name", "description")

  key_code

  write_csv(
    key_code,
    paste0(
      path,
      "codebook_",
      year,
      ".csv"
    )
  )

}


#' Clean data
#'
#' Step 2
#'
#' This function removes the first two rows of the dataset which has name and
#' description information for each variable. All variable names are
#' standardized to lowercase and separated by underscores using the janitor
#' package dependency. Repeated IDs are removed.
#'
#' @param df,beg_row,end_row,lower_bound,upper_bound Input dataframe, start row defaults to 1 and end row defaults to 2, lower bound defaults to 3 and upper to 95
#' @return cleaned dataframe
#' @export
clean_data <- function(df, beg_row = 1, end_row = 2, lower_bound = 3, upper_bound = 95) {

  df_clean <- df %>%
    janitor::clean_names() %>%
    filter(!row_number() %in% c(beg_row:end_row)) %>%
    distinct(external_reference, .keep_all = TRUE) %>%
    mutate(external_reference = as.numeric(external_reference))

  df_clean$progress <- as.numeric(df_clean$progress)

  df_clean %>%
    mutate(
      completion_status = factor(
        case_when(
          progress == lower_bound ~ "consent only",
          progress > lower_bound & progress < upper_bound ~ "partial",
          progress >= upper_bound ~ "complete"
        ),
        levels = c(
          "complete",
          "partial",
          "consent only"
        )
      )
    )

}


#' Baseline reference file
#'
#' Step 3
#'
#' This function updates and creates new reference files using the original
#' key and new dataframe. The new key with added rows and IDs is produced, and
#' the original key file is modified with the year of update.
#'
#' @param df1,df2,year,period,path Input df1 = original key, df2 = new dataframe, year of analysis, and path to save new files
#' @return original llave, modifed, and updated llave with added rows and IDs
#' @export
update_reference <- function(df1, df2, year, period, path = ""){

  original_llave <- df1 %>%
    clean_names() %>%
    rename(UOID = uoid,
           PIDM = pidm)

  #  names(original_llave)[names(original_llave) == "id_eoy"] <- paste0("id_eoy_", year)

  lastmax<- max(
    original_llave$id_bl[
      original_llave$id_bl<86000001 & !is.na(original_llave$id_bl)
    ]
  )

  #create id_bl var in original data
  df_updated <- df2 %>%
    rename(UOID = uoid) %>%
    mutate(cohort = year,
           id_bl = as.numeric(rownames(df2)) + lastmax,
           UOID = as.numeric(external_reference),
           PIDM = as.numeric(pidm)) %>%
    filter(id_bl < 86000001) %>%
    select(-pidm)


  id <- df_updated %>%
    select(UOID, PIDM, id_bl, cohort)

  update <- bind_rows(original_llave, id) #86 numbers kept in the updated key



  write_csv(
    original_llave,
    paste0(
      path,
      "SWaSI_llave_pre",
      period,
      ".csv" ))

  write_csv(
    update,
    paste0(
      path,
      "SWaSI_llave.csv"
    ))

  df_updated

}


#' Create De-identified file
#'
#' Step 4
#'
#' This function removed identifiers from the dataframe.
#'
#' @param df,year,path Input file, year of analysis, and path
#' @return df, also exported file
#' @export
deidentify <- function(df, path = "", year) {

  raw <- df %>%
    select(
      -ip_address,
      -recipient_last_name, -recipient_first_name,
      -recipient_email, -uo_id,
      -location_latitude, -location_longitude, -pidm, -external_reference
    )

  write_csv(
    raw,
    paste0(
      path,
      "did_bl_",
      year,
      ".csv"
    ))
}


#' Separate files by consent status
#'
#' Step 5
#'
#' This function separates the dataframe into three files: it writes two files
#' that separates groups by whether they consented or not. This function adds
#' a consent column to the original dataset and keeps all values in it.
#'
#' @param df,year,path, consent_status Input dataframe, year of analysis, path, and consent_status can be specified "yes" or "no" but defaults to all
#' @return df with additional consent variable
#' @export
separate_consent <- function(df, year, path = "", consent_status = "") {

  consent_prep <- df %>%
    mutate(mc_1_providecontact = ifelse(mc_1 == 1 & !is.na(mc_1_1_text),
                                        "Y", "N")) %>%
    mutate(c_dat = ifelse(c_dat == 1, "Yes", "No"),
           mc_1 = ifelse(mc_1 == 1, "Yes", "No"),
           mc_2 = ifelse(mc_2 == 1, "Yes", "No"))

  consented <- consent_prep %>%
    filter(c_dat == "Yes")

  not_consented <- consent_prep %>%
    filter(c_dat != "Yes")


  write_csv(
    consented,
    paste0(
      path,
      "consented_bl_",
      year,
      ".csv"
    ))

  write_csv(
    not_consented,
    paste0(
      path,
      "noconsent_bl_",
      year,
      ".csv"
    ))

  if(consent_status == "yes" | consent_status == "Yes") {
    consented
  } else if(consent_status == "no" | consent_status == "No") {
    not_consented
  } else {
    consent_prep
  }
}

