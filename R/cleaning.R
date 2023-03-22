
#' Create codebook/ key file
#'
#' Step 1
#'
#' This function creates a codebook with the first two rows from the survey,
#' which has the name and description of each variable. This function
#' should be run first.
#'
#' @param df,year,path Input dataframe, year of analysis, and path to save file
#' @return codebook dataframe
#' @export
create_codebook <- function(df, year, path = ""){
  key_code = data.frame(unlist(colnames(df)), unlist(df[1, ], use.names = FALSE))
  names(key_code) = c("name", "description")

  write.csv(
    key_code,
    paste0(
      path,
      "codebook_",
      year,
      ".csv"
    )
  )
 return(key_code)
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
#' @param df,beg_row,end_row Input dataframe, start row defaults to 1 and end row defaults to 2
#' @return cleaned dataframe
#' @export
clean_data <- function(df, beg_row = 1, end_row = 2) {

  dat_clean <- df %>%
    janitor::clean_names() %>%
    filter(!row_number() %in% c(beg_row:end_row)) %>%
    distinct(external_reference, .keep_all = TRUE)

  return(dat_clean)
}


#' Baseline reference file
#'
#' Step 3
#'
#' This function updates and creates new reference files using the original
#' key and new dataframe. The new key with added rows and IDs is produced, and
#' the original key file is modified with the year of update.
#'
#' @param df1,df2,year,path Input df1 = original key, df2 = new dataframe, year of analysis, and path to save new files
#' @return original llave, modifed, and updated llave with added rows and IDs
#' @export
update_reference <- function(df1, df2, year, path = ""){

  original_llave <- df1 %>%
    clean_names() %>%
    rename(uo_id = uoid,
           id_eoy = id_eoy22)

  names(original_llave)[names(original_llave) == "id_eoy"] <- paste0("id_eoy_", year)

  lastmax<- max(
    original_llave$id_bl[
      original_llave$id_bl<86000001 & !is.na(original_llave$id_bl)
    ]
  )

  id <- df2 %>%
    select(external_reference, pidm) %>%
    rename(uo_id = external_reference) %>%
    mutate(cohort = year,
           id_bl = as.numeric(rownames(df2)) + lastmax)

  id$uo_id <- as.numeric(id$uo_id)
  id$pidm <- as.numeric(id$pidm)

  update <- bind_rows(original_llave, id)

  return(update)

  write.csv(
    original_llave,
    paste0(
      path,
      "reference_key_",
      year,
      ".csv" ))

  write.csv(
    update,
    paste0(
      path,
      "reference_key_update.csv"
    ))
}


#' Create De-identified file
#'
#' Step 4
#'
#' This function removed identifiers from the dataframe and adds baseline ID and
#' cohort column, useful for future analysis. Extraneous 86000000 numbers are
#' also removed from the ID column.
#'
#' @param df1,df2,year,path Input df1 = original key, df2 = new dataframe, year of analysis, and path
#' @return df, also exported file
#' @export
deidentify <- function(df1, df2, year, path = "") {

  original_llave <- df1 %>%
    clean_names() %>%
    rename(uo_id = uoid,
           id_eoy = id_eoy22)

  names(original_llave)[names(original_llave) == "id_eoy"] <- paste0("id_eoy_", year)

  lastmax<- max(
    original_llave$id_bl[
      original_llave$id_bl<86000001 & !is.na(original_llave$id_bl)
    ]
  )

  id <- df2 %>%
    rename(uo_id = external_reference) %>%
    mutate(cohort = year,
           id_bl = as.numeric(rownames(df2)) + lastmax)

  id$uo_id <- as.numeric(id$uo_id)
  id$pidm <- as.numeric(id$pidm)

  raw <- id %>%
    select(
      -ip_address,
      -recipient_last_name, -recipient_first_name,
      -recipient_email, -uo_id,
      -location_latitude, -location_longitude, -pidm
    )

  write.csv(
    raw,
    paste0(
      path,
      "did_bl_",
      year,
      ".csv"
    ))

  return(raw)
}


#' Separate files by consent status
#'
#' Step 5
#'
#' This function separates the dataframe into three files: it writes two files
#' that separates groups by whether they consented or not. This function adds
#' a consent column to the original dataset and keeps all values in it.
#'
#' @param df,year,path Input dataframe, lower_bound defaults to 3, upper_bound defaults to 95
#' @return df with additional consent variable
#' @export
separate_consent <- function(df, year, path = "") {

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


  write.csv(
    consented,
    paste0(
      path,
      "consented_bl_",
      year,
      ".csv"
    ))

  write.csv(
    not_consented,
    paste0(
      path,
      "noconsent_bl_",
      year,
      ".csv"
    ))

  return(consent_prep)
}



#' Separate files by completion status
#'
#' Step 6
#'
#' This function adds a completion status column based on the progress variable.
#' The lower and upper bounds on what is considered complete can be manipulated.
#'
#' @param df,lower_bound,upper_bound Input dataframe, lower_bound defaults to 3, upper_bound defaults to 95
#' @return df with additional status variable
#' @export
completion_status <- function(df, lower_bound = 3, upper_bound = 95) {

  df$progress <- as.numeric(df$progress)

  df_complete <- df %>%
    mutate(
      status = factor(
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
  return(df_complete)
}
