
#' Create codebook/ key file
#'
#' This function functions as the mean function, only calculates the mean with
#' NAs removed. If data has NAs, there will no longer be an error.
#'
#' @param df,year,path Input dataframe, year of analysis, and path to save file
#' @return df
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
#' This function functions as the mean function, only calculates the mean with
#' NAs removed. If data has NAs, there will no longer be an error.
#'
#' @param df,beg_row,end_row Input dataframe, start row defaults to 1 and end row defaults to 2
#' @return df
#' @export
clean_data <- function(df, beg_row = 1, end_row = 2) {
  
  df %>% 
    janitor::clean_names() %>% 
    filter(!row_number() %in% c(beg_row:end_row)) %>% 
    distinct(external_reference, .keep_all = TRUE)
}


#' Baseline reference file
#'
#' This function updates and creates new reference files.
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
  update
  
  write_csv(
    original_llave,
    paste0(
      path,
      "reference_key_",
      year,
      ".csv" ))
  
  write_csv(
    update,
    paste0(
      path,
      "reference_key.csv" 
    ))
}


#' Create Deidentified file
#'
#' This function removed identifiers from the dataframe.
#'
#' @param df,year,path Input dataframe, year of analysis, and path to save file
#' @return df, also exported file
#' @export
deidentify <- function(df, year, path = "") {
  raw <- blorig %>%
    select(
      -ip_address,
      -recipient_last_name, -recipient_first_name,
      -recipient_email, -external_reference, 
      -location_latitude, -location_longitude, -pidm
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
#' This function adds a consent status column based on the progress variable.
#'
#' @param df,lower_bound,upper_bound Input dataframe, lower_bound defaults to 3, upper_bound defaults to 95
#' @return df with additional status variable 
#' @export
id_consent <- function(df, lower_bound = 3, upper_bound = 95) {
  
  df$progress <- as.numeric(df$progress)
  
  df %>% 
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
}