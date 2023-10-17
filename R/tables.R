#' Identify intervention condition
#'
#'
#' This function adds mccm_correct column.
#' In other words, the intervention condition is specified.
#'
#'
#' @param df,year,path,name Input dataframe, year, path is default to current, name default saves to "qc_bl_"
#' @return df with additional intervention condition variable
#' @export
quality_control <- function(df, year, path = "", name = "qc_bl_") {

  qc <- df %>%
    select(
      id_bl, cohort, start_date:browser_resolution,
      starts_with("t"),
      fq_d, fq_td, mccm,
      pbi_1:pbi_3, pbs_1:pbs_3,
      status, cond, completion_status
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
    select(-external_reference, -ip_address,
           -response_id, -recipient_email, -location_latitude, -location_longitude)


  write_csv(
    qc,
    paste0(
      path,
      name,
      year,
      ".csv"
    ))

}


#' Essay Tab
#'
#'
#' This function cleans up the essay response.
#'
#'
#' @param df1,df2,year,path,name Input df1 = original/consent df, df2 = panel data, year, path is default to current, name default to "ie_bl_"
#' @return df with additional intervention condition variable
#' @export
intervention_essay <- function(df1, df2, year, path = "", name = "ie_bl_") {

  essay_tab <- left_join(df1, df2, by = c("external_reference" = "UOID")) %>%
    rename(cond_desc = cond) %>%
    mutate(cond = recode(
      cond_desc,
      "Control" = 1,
      "Social-Belonging" = 2,
      "Difference Education (First-Generation)" = 3,
      "Difference Education (International)" = 4,
      "Difference Education (Nontraditional/Transfer)" = 5
    ),
    essay = case_when(
      !is.na(ce) ~ ce,
      !is.na(sbe) ~ sbe,
      !is.na(defge) ~ defge,
      !is.na(dente) ~ dente,
      !is.na(deie) ~ deie
    ),
    essay = gsub("won't", "will not", essay),
    essay = gsub("can't", "can not", essay),
    essay = gsub("n't", " not", essay),
    essay = gsub("'ll", " will", essay),
    essay = gsub("'re", " are", essay),
    essay = gsub("'ve", " have", essay),
    essay = gsub("'m", " am", essay),
    essay = gsub("'d", " would", essay),
    essay = gsub("'s", "", essay),
    essay = sapply(essay, function(x) gsub("[^a-zA-Z0-9 ]", " ", x)),
    essay = str_squish(essay),
    essay = str_to_lower(essay),
    intl = if_else(is.na(intl),"us", "intl")
    ) %>%
    within(essay[essay == ""]<- NA_character_) %>%
    within(essay[essay == "90oi8plk m"]<- NA_character_) %>%
    mutate(essay_has_data = if_else(!is.na(essay),"Y", "N")) %>%
    select(id_bl, cohort, cond, cond_desc, tfer, nontrad, ntt, intl, essay, essay_has_data, completion_status)

  write_csv(
    essay_tab,
    paste0(
      path,
      name,
      year,
      ".csv"
    ))
}

#' Items Tab
#'
#'
#' This function cleans up the response items.
#'
#'
#' @param df1,year,path,name Input dataframe, year, path is default to current, name is set to "item_bl_"
#' @return df with items cleaned up
#' @export
itemify <- function(df, year, path = "", name = "item_bl_") {
  items <- df %>%
    select(id_bl, cohort,
           au_1:au_5, bu_1:bu_4, l_1:l_3,
           ss_1:ss_4, stt_1:stt_4, gh_1:gh_4,
           ls_1:ls_5, ps_1:ps_4, se, sas_1:sas_11, completion_status) %>%
    mutate_if(is.character, as.numeric) %>%
    rename(sa_1 = sas_1,
           sa_2 = sas_2,
           sa_3 = sas_3,
           sa_4 = sas_4,
           sa_5 = sas_5,
           sa_6 = sas_6) %>%
    rename(s_1 = sas_7,
           s_2 = sas_8,
           s_3 = sas_9,
           s_4 = sas_10,
           s_5 = sas_11)

  write_csv(
    items,
    paste0(
      path,
      name,
      year,
      ".csv"
    ))
}

#' Demographic Tab
#'
#'
#' This function cleans up the demographic variables
#'
#'
#' @param df1,df2,year,path,name Input df1 = original/consent df, df2 = panel data, year, path is default to current, name is set to "ds_bl_"
#' @return df with demo vars cleaned up
#' @export
demos <- function(df1, df2, df3, year, path = "", name = "ds_bl_") {

  df3 <- df3 %>%
    select(external_reference, id_bl, cohort)

  df <- left_join(df1, df2, by = c("external_reference" = "UOID")) %>%
    left_join(., df3, by = "external_reference") %>%
    #this is new because I want to make sure data types are appropriate for code below
    #gender
    mutate(gi_agender = if_else(str_detect(gi, "Agender"), 1, 0),
           gi_cisgender = if_else(str_detect(gi, "Cisgender"), 1, 0),
           gi_genderfluid = if_else(str_detect(gi, "Genderfluid"), 1, 0),
           gi_nonconforming = if_else(str_detect(gi, "nonconforming"), 1, 0),
           gi_genderqueer = if_else(str_detect(gi, "Genderqueer"), 1, 0),
           gi_man = if_else(str_detect(gi, "Man"), 1, 0),
           gi_multigender = if_else(str_detect(gi, "Multigender"), 1, 0),
           gi_nonbinary = if_else(str_detect(gi, "Nonbinary"), 1, 0),
           gi_thirdgender = if_else(str_detect(gi, "Third"), 1, 0),
           gi_transgender = if_else(str_detect(gi, "Transgender"), 1, 0),
           gi_twospirit = if_else(str_detect(gi, "Two-Spirit"), 1, 0),
           gi_woman = if_else(str_detect(gi, "Woman"), 1, 0),
           gi_questioning = if_else(str_detect(gi, "Questioning"), 1, 0),
           gi_other = if_else(str_detect(gi, "listed"), 1, 0),
           gi_pntr = if_else(str_detect(gi, "Prefer"), 1, 0),
           gi_other_text = manipulate_text(gi_14_text)) %>%
    within(gi_genderqueer[str_detect(gi_other_text, "queer")]<- 1) %>%
    within(gi_woman[str_detect(gi_other_text, "woman")]<- 1) %>%
    within(gi_woman[str_detect(gi_other_text, "women")]<- 1) %>%
    within(gi_woman[str_detect(gi_other_text, "fem")]<- 1) %>% #covers femme and transfeminine
    within(gi_woman[str_detect(gi_other_text, "masc")]<- 1) %>% #covers masculine, transmasc, and trans-masculine
    within(gi_nonconforming[str_detect(gi_other_text, "do not care")]<- 1) %>%
    within(gi_nonconforming[str_detect(gi_other_text, "label")]<- 1) %>% #whether or not the "no label" folks want to admit it, they are
    #nonconforming pretty much by definition
    mutate(gi_agender_desc = if_else(gi_agender == 1, "Agender", NA_character_),
           gi_cisgender_desc = if_else(gi_cisgender == 1, "Cisgender", NA_character_),
           gi_genderfluid_desc = if_else(gi_genderfluid == 1, "Genderfluid", NA_character_),
           gi_nonconforming_desc = if_else(gi_nonconforming == 1, "Gender Nonconforming", NA_character_),
           gi_genderqueer_desc = if_else(gi_genderqueer == 1, "Genderqueer", NA_character_),
           gi_man_desc = if_else(gi_man == 1, "Man", NA_character_),
           gi_multigender_desc = if_else(gi_multigender == 1,"Multigender (including bigender, polygender, pangender, etc.)", NA_character_),
           gi_nonbinary_desc = if_else(gi_nonbinary == 1, "Nonbinary", NA_character_),
           gi_thirdgender_desc = if_else(gi_thirdgender == 1, "Third Gender", NA_character_),
           gi_transgender_desc = if_else(gi_transgender == 1, "Transgender", NA_character_),
           gi_twospirit_desc = if_else(gi_twospirit == 1, "Two-Spirit", NA_character_),
           gi_woman_desc = if_else(gi_woman == 1, "Woman", NA_character_),
           gi_questioning_desc = if_else(gi_questioning == 1, "Questioning or Exploring", NA_character_),
           gi_other_desc = if_else(gi_other == 1, "Other", NA_character_),
           gi_pntr_desc = if_else(gi_pntr == 1, "Prefer not to respond", NA_character_),
           gi_cats = gsub(",", "|", gi),
           gi_check = rowSums(across(gi_agender:gi_questioning)),
           gi_desc = case_when(gi_check == 1 & gi_agender == 1 ~ "Agender",
                               gi_check == 1 & gi_cisgender == 1 ~ "Cisgender",
                               gi_check == 1 & gi_genderfluid == 1 ~ "Genderfluid",
                               gi_check == 1 & gi_nonconforming == 1 ~ "Gender Nonconforming",
                               gi_check == 1 & gi_genderqueer == 1 ~ "Genderqueer",
                               gi_check == 1 & gi_man == 1 ~ "Man",
                               gi_check == 1 & gi_multigender == 1 ~ "Multigender",
                               gi_check == 1 & gi_nonbinary == 1 ~ "Nonbinary",
                               gi_check == 1 & gi_thirdgender == 1 ~ "Third Gender",
                               gi_check == 1 & gi_transgender == 1 ~ "Transgender",
                               gi_check == 1 & gi_twospirit == 1 ~ "Two-Spirit",
                               gi_check == 1 & gi_woman == 1 ~ "Woman",
                               gi_check == 1 & gi_questioning == 1 ~ "Questioning or Exploring",
                               gi_check > 1 & str_detect(gi_cats, "Questioning") ~ "Questioning or Exploring",
                               gi_check > 1 & !str_detect(gi_cats, "Questioning") ~ "Mixture"),
           gi_desc_bin_1 = factor(case_when(gi_desc %in% c("Agender",
                                                           "Genderfluid",
                                                           "Gender Nonconforming",
                                                           "Genderqueer",
                                                           "Mixture",
                                                           "Multigender",
                                                           "Nonbinary",
                                                           "Third Gender",
                                                           "Transgender",
                                                           "Two-Spirit",
                                                           "Questioning or Exploring") ~ "Nonbinary",
                                            gi_desc == "Man" ~ "Man",
                                            gi_desc == "Cisgender" & gender_desc == "Male" ~ "Man",
                                            gi_desc == "Woman" ~ "Woman",
                                            gi_desc == "Cisgender" & gender_desc == "Female" ~ "Woman"),
                                  levels = c("Man", "Nonbinary", "Woman")),
           gi_desc_bin_2 = factor(case_when(gi_desc %in% c("Agender",
                                                           "Genderfluid",
                                                           "Gender Nonconforming",
                                                           "Genderqueer",
                                                           "Multigender",
                                                           "Nonbinary",
                                                           "Third Gender",
                                                           "Transgender",
                                                           "Two-Spirit",
                                                           "Questioning or Exploring") ~ "Nonbinary",
                                            gi_desc == "Man" ~ "Man",
                                            gi_desc == "Cisgender" & gender_desc == "Male" ~ "Man",
                                            gi_desc == "Woman" ~ "Woman",
                                            gi_desc == "Cisgender" & gender_desc == "Female" ~ "Woman",
                                            gi_desc == "Mixture" ~ "Mixture"),
                                  levels = c("Man", "Mixture", "Nonbinary", "Woman")),
           gi_desc_bin_3 = factor(case_when(gi_desc %in% c("Agender",
                                                           "Genderfluid",
                                                           "Gender Nonconforming",
                                                           "Genderqueer",
                                                           "Multigender",
                                                           "Nonbinary",
                                                           "Third Gender",
                                                           "Transgender",
                                                           "Two-Spirit") ~ "Nonbinary",
                                            gi_desc == "Man" ~ "Man",
                                            gi_desc == "Cisgender" & gender_desc == "Male" ~ "Man",
                                            gi_desc == "Woman" ~ "Woman",
                                            gi_desc == "Cisgender" & gender_desc == "Female" ~ "Woman",
                                            gi_desc == "Mixture" ~ "Mixture",
                                            gi_desc == "Questioning or Exploring" ~ "Questioning or Exploring"),
                                  levels = c("Man", "Mixture", "Nonbinary", "Questioning or Exploring", "Woman"))) %>%
    #sexuality
    mutate(so_asexual = if_else(str_detect(so, "Asexual"), 1, 0),
           so_bisexual = if_else(str_detect(so, "Bisexual"), 1, 0),
           so_fluid = if_else(str_detect(so, "Fluid"), 1, 0),
           so_gay = if_else(str_detect(so, "Gay"), 1, 0),
           so_graysexual = if_else(str_detect(so, "Graysexual"), 1, 0),
           so_hetero = if_else(str_detect(so, "Heterosexual"), 1, 0),
           so_lesbian = if_else(str_detect(so, "Lesbian"), 1, 0),
           so_pansexual = if_else(str_detect(so, "Pansexual"), 1, 0),
           so_queer = if_else(str_detect(so, "Queer"), 1, 0),
           so_sgl = if_else(str_detect(so, "Same-gender"), 1, 0),
           so_questioning = if_else(str_detect(so, "Questioning"), 1, 0),
           so_other = if_else(str_detect(so, "listed"), 1, 0),
           so_pntr = if_else(str_detect(so, "Prefer"), 1, 0),
           so_other_text = manipulate_text(so_12_text)) %>%
    within(so_asexual[str_detect(so_other_text, "asexual")]<- 1) %>%
    within(so_asexual[str_detect(so_other_text, "ace")]<- 1) %>%
    within(so_asexual[str_detect(so_other_text, "demi")]<- 1) %>%
    within(so_bisexual[str_detect(so_other_text, "bisexual")]<- 1) %>%
    within(so_lesbian[str_detect(so_other_text, "sapphic")]<- 1) %>%
    within(so_hetero[str_detect(so_other_text, "straight")]<- 1) %>%
    within(so_hetero[str_detect(so_other_text, "heterosexual")]<- 1) %>%
    within(so_pansexual[str_detect(so_other_text, "whomever")]<- 1) %>%
    within(so_questioning[str_detect(so_other_text, "curious")]<- 1) %>%
    within(so_questioning[str_detect(so_other_text, "do not know")]<- 1) %>%
    within(so_pntr[str_detect(so_other_text, "pntr")]<- 1) %>%
    mutate(so_asexual_desc = if_else(so_asexual == 1, "Asexual", NA_character_),
           so_bisexual_desc = if_else(so_bisexual == 1, "Bisexual", NA_character_),
           so_fluid_desc = if_else(so_fluid == 1, "Fluid or Flexible", NA_character_),
           so_gay_desc = if_else(so_gay == 1, "Gay", NA_character_),
           so_graysexual_desc = if_else(so_graysexual == 1, "Graysexual", NA_character_),
           so_hetero_desc = if_else(so_hetero == 1, "Straight or Heterosexual", NA_character_),
           so_lesbian_desc = if_else(so_lesbian == 1, "Lesbian", NA_character_),
           so_pansexual_desc = if_else(so_pansexual == 1, "Pansexual", NA_character_),
           so_queer_desc = if_else(so_queer == 1, "Queer", NA_character_),
           so_sgl_desc = if_else(so_sgl == 1, "Same-Gender Loving", NA_character_),
           so_questioning_desc = if_else(so_questioning == 1, "Questioning or Exploring", NA_character_),
           so_other_desc = if_else(so_other == 1, "Other", NA_character_),
           so_pntr_desc = if_else(so_pntr == 1, "Prefer not to respond", NA_character_),
           so_cats = gsub(",", "|", so),
           so_check = rowSums(across(so_asexual:so_questioning)),
           so_desc = case_when(so_check == 1 & so_asexual == 1 ~ "Asexual",
                               so_check == 1 & so_bisexual == 1 ~ "Bisexual",
                               so_check == 1 & so_fluid == 1 ~ "Fluid or Flexible",
                               so_check == 1 & so_gay == 1 ~ "Gay",
                               so_check == 1 & so_graysexual == 1 ~ "Graysexual",
                               so_check == 1 & so_hetero == 1 ~ "Straight or Heterosexual",
                               so_check == 1 & so_lesbian == 1 ~ "Lesbian",
                               so_check == 1 & so_pansexual == 1 ~ "Pansexual",
                               so_check == 1 & so_queer == 1 ~ "Queer",
                               so_check == 1 & so_sgl == 1 ~ "Same-Gender Loving",
                               so_check == 1 & so_questioning == 1 ~ "Questioning or Exploring",
                               so_check > 1 & str_detect(so_cats, "Questioning") ~ "Questioning or Exploring",
                               so_check > 1 & !str_detect(so_cats, "Questioning") ~ "Mixture"),
           so_desc_bin_1 = factor(if_else(so_desc == "Straight or Heterosexual",
                                                     "Straight or Heterosexual",
                                                     "LGBQ+"),
                                  levels = c("Straight or Heterosexual", "LGBQ+")),
           so_desc_bin_2 = factor(case_when(so_desc %in% c("Asexual",
                                                           "Bisexual",
                                                           "Fluid or Flexible",
                                                           "Gay",
                                                           "Graysexual",
                                                           "Lesbian",
                                                           "Pansexual",
                                                           "Queer",
                                                           "Same-Gender Loving",
                                                           "Questioning or Exploring") ~ "LGBQ+",
                                            so_desc == "Straight or Heterosexual" ~ "Straight or Heterosexual",
                                            so_desc == "Mixture" ~ "Mixture"),
                                  levels = c("Straight or Heterosexual", "Mixture", "LGBQ+")),
           so_desc_bin_3 = factor(case_when(so_desc %in% c("Asexual",
                                                           "Bisexual",
                                                           "Fluid or Flexible",
                                                           "Gay",
                                                           "Graysexual",
                                                           "Lesbian",
                                                           "Pansexual",
                                                           "Queer",
                                                           "Same-Gender Loving") ~"LGBQ+",
                                            so_desc == "Straight or Heterosexual" ~ "Straight or Heterosexual",
                                            so_desc == "Mixture" ~ "Mixture",
                                            so_desc == "Questioning or Exploring" ~ "Questioning or Exploring"),
                                  levels = c("Questioning or Exploring", "Straight or Heterosexual", "Mixture", "LGBQ+")),
           lgbtqia_desc = factor(case_when(gi_desc_bin_1 == "Nonbinary" |
                                             so_desc_bin_1 == "LGBQ+" ~ "LGBTQIA+",
                                           !is.na(gi_desc_bin_1) |
                                             !is.na(so_desc_bin_1) ~ "Not LGBTQIA+"),
                                 levels = c("Not LGBTQIA+","LGBTQIA+"))) %>%
    #race and ethnicity
    mutate(re = case_when(federal_ethnic_desc == "American Indian or Alaska Native" ~"Native American/Alaskan",
                          federal_ethnic_desc == "Asian" ~ "Asian",
                          federal_ethnic_desc == "Black or African American" ~ "Black",
                          federal_ethnic_desc == "Hispanic or Latino" ~ "Latin*",
                          federal_ethnic_desc == "Native Hawaiian or Other Pacific Islander" ~ "Native Hawaiian/Pacific Islander",
                          federal_ethnic_desc == "Nonresident alien" ~ "International",
                          federal_ethnic_desc == "Race and ethnicity unknown" ~ NA_character_,
                          federal_ethnic_desc == "Two or more races" ~ "Multiracial/ethnic",
                          federal_ethnic_desc == "White" ~ "White")) %>%
    within(re[international_student_flag == "Y"]<- "International") %>%
    mutate(re = factor(re, levels = c("White", "Native Hawaiian/Pacific Islander", "Native American/Alaskan", "Multiracial/ethnic", "Latin*", "International", "Black", "Asian"))
    ) %>%
    #socioeconomic status
    mutate(pe_1_desc = recode(pe_1, "Don't Know/Not Applicable" = NA_character_),
           pe_2_desc = recode(pe_2, "Don't Know/Not Applicable" = NA_character_),
           pe_1 = case_when(pe_1 == "some high school, no diploma" ~ 1,
                            pe_1 == "high school diploma, GED" ~ 2,
                            pe_1 == "some college credit, no degree" ~ 3,
                            pe_1 == "2-year technical / Associate's degree" ~ 4,
                            pe_1 == "4-year college / university degree" ~ 5,
                            pe_1 == "graduate degree (Masters, Doctorate, Law)" ~ 6,
                            pe_1 == "don't know / not applicable" ~ 7),
           pe_2 = case_when(pe_2 == "some high school, no diploma" ~ 1,
                            pe_2 == "high school diploma, GED" ~ 2,
                            pe_2 == "some college credit, no degree" ~ 3,
                            pe_2 == "2-year technical / Associate's degree" ~ 4,
                            pe_2 == "4-year college / university degree" ~ 5,
                            pe_2 == "graduate degree (Masters, Doctorate, Law)" ~ 6,
                            pe_2 == "don't know / not applicable" ~ 7),
           rc_pe_1 = recode(pe_1, `7` = NA_real_),
           rc_pe_2 = recode(pe_2, `7` = NA_real_),
           fg_surv = case_when(pe_1 == 5 | pe_1 == 6 | pe_2 == 5 | pe_2 == 6 ~ "Continuing-Generation",
                               pe_1 < 5 & pe_2 < 5 ~ "First-Generation",
                               pe_2 == 7 & pe_1 < 5 ~ "First-Generation",
                               pe_1 == 7 & pe_2 < 5 ~ "First-Generation",
                               pe_1 < 5 & is.na(pe_2) ~ "First-Generation",
                               pe_2 < 5 & is.na(pe_1) ~ "First-Generation"),
           fg_combo = factor(if_else(is.na(fg) | fg == "Unknown", fg_surv, fg),
                             levels = c("Continuing-Generation", "First-Generation")),
           sss_desc = factor(case_when(sss == "1 - worst off" ~ "Bottom Rung",
                                       sss == "2" ~ "9th Rung",
                                       sss == "3" ~ "8th Rung",
                                       sss == "4" ~ "7th Rung",
                                       sss == "5" ~ "6th Rung",
                                       sss == "6" ~ "5th Rung",
                                       sss == "7" ~ "4th Rung",
                                       sss == "8" ~ "3rd Rung",
                                       sss == "9" ~ "2nd Rung",
                                       sss == "10 - best off" ~ "Top Rung"),
                             levels = c( "Bottom Rung",
                                         "9th Rung",
                                         "8th Rung",
                                         "7th Rung",
                                         "6th Rung",
                                         "5th Rung",
                                         "4th Rung",
                                         "3rd Rung",
                                         "2nd Rung",
                                         "Top Rung")),
           sss = case_when(sss_desc == "Bottom Rung" ~ 1,
                           sss_desc == "9th Rung" ~ 2,
                           sss_desc == "8th Rung" ~ 3,
                           sss_desc == "7th Rung" ~ 4,
                           sss_desc == "6th Rung" ~ 5,
                           sss_desc == "5th Rung" ~ 6,
                           sss_desc == "4th Rung" ~ 7,
                           sss_desc == "3rd Rung" ~ 8,
                           sss_desc == "2nd Rung" ~ 9,
                           sss_desc == "Top Rung" ~ 10),
           sss_desc_bin = factor(case_when(sss < 5 ~ "Lower",
                                           sss > 4 & sss < 8 ~ "Middle",
                                           sss > 7 ~ "Upper"),
                                 levels = c("Lower", "Middle", "Upper")),
           fsc_desc = fsc,
           fsc = case_when(fsc_desc == "working class" ~ 1,
                           fsc_desc == "lower-middle class" ~ 2,
                           fsc_desc == "middle class" ~ 3,
                           fsc_desc == "upper-middle class" ~ 4,
                           fsc_desc == "upper class" ~ 5),
           fsc_desc = factor(case_when(fsc_desc == "working class" ~ "Working Class",
                                       fsc_desc == "lower-middle class" ~ "Lower-Middle Class",
                                       fsc_desc == "middle class" ~ "Middle Class",
                                       fsc_desc == "upper-middle class" ~ "Upper-Middle Class",
                                       fsc_desc == "upper class" ~ "Upper Class"),
                             levels = c("Working Class", "Lower-Middle Class",
                                        "Middle Class", "Upper-Middle Class","Upper Class")),
           fsc_desc_bin = factor(case_when(fsc < 3 ~ "Lower",
                                           fsc == 3 ~ "Middle",
                                           fsc > 3 ~ "Upper"),
                                 levels = c("Lower", "Middle", "Upper")),
           hsa_1_desc = hsa_1,
           hsa_1 = case_when(str_detect(hsa_1_desc, "is less advantaged") ~ 1,
                             str_detect(hsa_1_desc, "is neither") ~ 2,
                             str_detect(hsa_1_desc, "is more advantaged") ~ 3),
           hsa_1_desc = factor(case_when(hsa_1 == 1 ~ "Less Advantaged",
                                         hsa_1 == 2 ~ "Equally Advantaged",
                                         hsa_1 == 3 ~ "More Advantaged"),
                               levels = c("Less Advantaged", "Equally Advantaged", "More Advantaged"))) %>%
    #disability
    mutate(d_chronic = if_else(str_detect(d1, "Chronic"), 1, 0),
           d_cognitive = if_else(  str_detect(d1, "Cognitive"), 1, 0),
           d_upperlimb = if_else(  str_detect(d1, "Fingers"), 1, 0),
           d_hearing = if_else(  str_detect(d1, "Hearing"), 1, 0),
           d_lowerbody = if_else(  str_detect(d1, "Lower body"), 1, 0),
           d_mental = if_else(  str_detect(d1, "Mental health"), 1, 0),
           d_neurodiv = if_else(  str_detect(d1, "Neurodivergence"), 1, 0),
           d_neurolog = if_else(  str_detect(d1, "Neurological"), 1, 0),
           d_speech = if_else(  str_detect(d1, "Speech"), 1, 0),
           d_vision = if_else(  str_detect(d1, "Vision"), 1, 0),
           d_na = if_else(  str_detect(d1, "applicable"), 1, 0),
           d_other = if_else(  str_detect(d1, "Other"), 1, 0),
           d_pntr = if_else(  str_detect(d1, "Prefer"), 1, 0),
           d_other_text = manipulate_text(d1_11_text)) %>%
    within(d_chronic[str_detect(d_other_text, "cancer")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "heart")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "cardio")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "pulmon")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "tissue")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "immune")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "chron")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "noonan")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "asthma")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "ibs")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "ibd")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "lupus")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "orthostatic")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "chronic")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "tumor")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "syncope")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "ehlers")]<- 1) %>%
    within(d_chronic[str_detect(d_other_text, "cyclical vomiting")]<- 1) %>%
    within(d_cognitive[str_detect(d_other_text, "dementia")]<- 1) %>%
    within(d_cognitive[str_detect(d_other_text, "learning")]<- 1) %>%
    within(d_cognitive[str_detect(d_other_text, "sensory")]<- 1) %>%
    within(d_cognitive[str_detect(d_other_text, "adhd")]<- 1) %>%
    within(d_cognitive[str_detect(d_other_text, "add")]<- 1) %>%
    within(d_cognitive[str_detect(d_other_text, "cognitive")]<- 1) %>%
    within(d_cognitive[str_detect(d_other_text, "processing")]<- 1) %>%
    within(d_cognitive[str_detect(d_other_text, "dyslex")]<- 1) %>%
    within(d_lowerbody[str_detect(d_other_text, "scoliosis")]<- 1) %>%
    within(d_lowerbody[str_detect(d_other_text, "pots")]<- 1) %>%
    within(d_mental[str_detect(d_other_text, "ocd")]<- 1) %>%
    within(d_mental[str_detect(d_other_text, "anxiety")]<- 1) %>%
    within(d_mental[str_detect(d_other_text, "depression")]<- 1) %>%
    within(d_mental[str_detect(d_other_text, "pmdd")]<- 1) %>%
    within(d_mental[str_detect(d_other_text, "agoraphobia")]<- 1) %>%
    within(d_mental[str_detect(d_other_text, "misophonia")]<- 1) %>%
    within(d_mental[str_detect(d_other_text, "panic")]<- 1) %>%
    within(d_mental[str_detect(d_other_text, "ptsd")]<- 1) %>%
    within(d_mental[str_detect(d_other_text, "tic")]<- 1) %>%
    within(d_mental[str_detect(d_other_text, "tourette")]<- 1) %>%
    within(d_neurodiv[str_detect(d_other_text, "adhd")]<- 1) %>%
    within(d_neurodiv[str_detect(d_other_text, "add")]<- 1) %>%
    within(d_neurodiv[str_detect(d_other_text, "autism")]<- 1) %>%
    within(d_neurodiv[str_detect(d_other_text, "dyslex")]<- 1) %>%
    within(d_neurodiv[str_detect(d_other_text, "aphantasia")]<- 1) %>%
    within(d_neurolog[str_detect(d_other_text, "tbi")]<- 1) %>%
    within(d_neurolog[str_detect(d_other_text, "traumatic brain")]<- 1) %>%
    within(d_neurolog[str_detect(d_other_text, "concussi")]<- 1) %>%
    within(d_neurolog[str_detect(d_other_text, "migraine")]<- 1) %>%
    mutate(d_chronic_desc = if_else(d_chronic == 1,  "Chronic health conditions (diabetes, autoimmune, chronic pain or fatigue, cancer, asthma, etc.)",  NA_character_),
           d_cognitive_desc = if_else(  d_cognitive == 1,  "Cognitive, learning, memory, processing, or reading (ADHD, dyslexia, learning disability, dementia, etc.)",  NA_character_),
           d_upperlimb_desc = if_else(  d_upperlimb == 1,  "Fingers, hands, or arms (carpal tunnel syndrome, arthritis, etc.)",  NA_character_),
           d_hearing_desc = if_else(  d_hearing == 1,  "Hearing (deaf, partial hearing loss, etc.)",  NA_character_),
           d_lowerbody_desc = if_else(  d_lowerbody == 1,  "Lower body, legs, or feet (difficulty walking or climbing stairs, CP, lower back issues, etc.)",  NA_character_),
           d_mental_desc = if_else(  d_mental == 1,  "Mental health, emotional, or psychological (depression, OCD, PTSD, schizophrenia, etc.)",  NA_character_),
           d_neurodiv_desc = if_else(  d_neurodiv == 1,  "Neurodivergence (autism, ADHD, dyslexia, etc.)",  NA_character_),
           d_neurolog_desc = if_else(  d_neurolog == 1,  "Neurological (seizures, migraines, multiple sclerosis, etc.)",  NA_character_),
           d_speech_desc = if_else(  d_speech == 1,  "Speech",  NA_character_),
           d_vision_desc = if_else(  d_vision == 1,  "Vision (blind, colorblind, etc.)",  NA_character_),
           d_na_desc = if_else(  d_na == 1,  "Not applicable / I do not",  NA_character_),
           d_other_desc = if_else(  d_other == 1,  "Other",  NA_character_),
           d_pntr_desc = if_else(  d_pntr == 1,  "Prefer not to respond",  NA_character_),
           d_cats = gsub(",", "|", d1),
           d_check = rowSums(across(d_chronic:d_vision)),
           d_desc = case_when(d_check == 1 & d_chronic == 1 ~"Chronic",
                              d_check == 1 & d_cognitive == 1 ~ "Cognitive",
                              d_check == 1 & d_upperlimb == 1 ~ "Upper Limb",
                              d_check == 1 & d_hearing == 1 ~ "Hearing",
                              d_check == 1 & d_lowerbody == 1 ~ "Lower Body",
                              d_check == 1 & d_mental == 1 ~ "Mental Health",
                              d_check == 1 & d_neurodiv == 1 ~ "Neurodivergent",
                              d_check == 1 & d_neurolog == 1 ~ "Neurological",
                              d_check == 1 & d_speech == 1 ~ "Speech",
                              d_check == 1 & d_vision == 1 ~ "Vision",
                              d_na == 1 ~ "No Disability",
                              d_check > 1 ~ "Mixture"),
           d_desc_bin_1 = factor(if_else(d_desc == "No Disability","No Disability","Has Disability"),
                                 levels = c("No Disability", "Has Disability")),
           d_desc_bin_2 = factor(case_when(d_desc %in% c("Chronic",
                                                         "Cognitive",
                                                         "Upper Limb",
                                                         "Hearing",
                                                         "Lower Body",
                                                         "Mental Health",
                                                         "Neurodivergent",
                                                         "Neurological",
                                                         "Speech",
                                                         "Vision") ~ "Has 1 Disability",
                                           d_desc == "No Disability" ~ "No Disability",
                                           d_desc == "Mixture" ~ "Has More Than 1 Disability"),
                                 levels = c("No Disability", "Has 1 Disability","Has More Than 1 Disability")),
           d_barrier = d2, d_barrier_desc = factor(case_when(d_barrier == "Yes" ~ "Experienced Barriers",
                                                             d_barrier == "No" ~ "Not Experienced Barriers",
                                                             d_barrier == "Unsure" ~ "Unsure about Barriers",
                                                             str_detect(d_barrier, "Prefer") ~ NA_character_),
                                                   levels = c("Not Experienced Barriers", "Unsure about Barriers", "Experienced Barriers")),
           d_service = d3,
           d_service_desc = factor(case_when(d_service == "Yes" ~ "Sought/Seeking Services",
                                             d_service == "No" ~ "Not Seeking Services",
                                             d_service == "Unsure" ~ "Unsure about Seeking Services",
                                             str_detect(d_service, "aware") ~ "Unaware of Services",
                                             str_detect(d_service, "Prefer") ~ NA_character_),
                                   levels = c("Unaware of Services","Not Seeking Services", "Unsure about Seeking Services", "Sought/Seeking Services"))) %>%
    #transfer and nontrad
    mutate(tfer = factor(if_else(tfer == "Y","Transfer","First-Time"),
                         levels = c("First-Time","Transfer")),
           nontrad = factor(if_else(nontrad == "Y","Nontraditional","Traditional"),
                            levels = c("Traditional","Nontraditional")),
           ntt = factor(if_else(ntt == "Y", "Nontraditional/Transfer", "Traditional/First-Time"),
                        levels = c("Traditional/First-Time", "Nontraditional/Transfer"))) %>%
    #residency
    mutate(residency_desc_bin = factor(case_when(residency_desc == "Non-Resident" ~ "Nonresident",
                                                 residency_desc == "Resident" ~ "Resident",
                                                 residency_desc == "Tuition Equity - HB2787" ~ "Resident",
                                                 residency_desc == "Undetermined" ~ NA_character_,
                                                 residency_desc == "Veteran's Res" ~ "Resident"),
                                       levels = c("Nonresident", "Resident"))) %>%
    #foster youth
    mutate(fy_desc = factor(case_when(fy == "No" ~ "Not Foster Youth",
                                      fy == "Yes" ~ "Foster Youth",
                                      str_detect(fy, "Prefer") ~ NA_character_),
        levels = c("Not Foster Youth", "Foster Youth"))) %>%
    #parent
    mutate(p1_desc = p1,
           p1_desc_bin = factor(case_when(str_detect(p1, "I am not a parent") ~ "Not a Parent",
                                          str_detect(p1, "Yes") ~ "Parent",
                                          str_detect(p1, "Prefer") ~ NA_character_),
                                levels = c("Not a Parent","Parent")),
           p2_expecting = if_else(str_detect(p2, "Expecting"), 1, 0),
           p2_1yr = if_else(str_detect(p2, "Birth"), 1, 0),
           p2_2to5yrs = if_else(  str_detect(p2, "2-5"), 1, 0),
           p2_6to11yrs = if_else(  str_detect(p2, "6-11"), 1, 0),
           p2_12to17yrs = if_else(  str_detect(p2, "12-17"), 1, 0),
           p2_none_above = if_else(  str_detect(p2, "Do not"), 1, 0),
           p2_pntr = if_else(  str_detect(p2, "Prefer"), 1, 0),
           p2_expecting_desc = if_else(  p2_expecting == 1,  "Expecting a baby / not yet born",  NA_character_),
           p2_1yr_desc = if_else(  p2_1yr == 1,  "Birth to 1 year old",  NA_character_),
           p2_2to5yrs_desc = if_else(  p2_2to5yrs == 1,  "2-5 years old",  NA_character_),
           p2_6to11yrs_desc = if_else(  p2_6to11yrs == 1,  "6-11 years old",  NA_character_),
           p2_12to17yrs_desc = if_else(  p2_12to17yrs == 1,  "12-17 years old",  NA_character_),
           p2_none_above_desc = if_else(  p2_none_above == 1,  "Do not have children in the above age groups",  NA_character_),
           p2_pntr_desc = if_else(  p2_pntr == 1,  "Prefer not to respond",  NA_character_),
           p2_cats = gsub(",", "|", p2),
           p2_check = rowSums(across(p2_expecting:p2_none_above)),
           p2_desc = case_when(  p2_check == 1 & p2_expecting == 1 ~    "Expecting a Child",
                                 p2_check == 1 & p2_1yr == 1 ~    "One 1 Year-Old",
                                 p2_check == 1 & p2_2to5yrs == 1 ~    "One 2-5 Year-Old",
                                 p2_check == 1 & p2_6to11yrs == 1 ~    "One 6-11 Year-Old",
                                 p2_check == 1 & p2_12to17yrs == 1 ~    "One 12-17 Year-Old",
                                 p2_check == 1 & p2_none_above == 1 ~    "One Older than 17",
                                 p2_check > 1 ~    "More than One Child"),
           p2_desc_bin_1 = factor(  if_else(    p2_desc == "More than One Child", "More than One Child", "One Child"  ),
                                    levels = c(    "One Child",    "More than One Child"  )),
           p2_desc_bin_2 = factor(  case_when(    p1_desc_bin == "Not a Parent" &      is.na(p2_desc_bin_1) ~ "No Children",
                                                  p2_desc_bin_1 == "More than One Child" ~ "More than One Child",
                                                  p2_desc_bin_1 == "One Child" ~ "One Child"  ),
                                    levels = c(    "No Children",    "One Child",    "More than One Child"))) %>%
    #religious affiliation
    mutate(ra_agnostic = if_else(  str_detect(r, "Agnostic"), 1, 0),
           ra_atheist = if_else(  str_detect(r, "Atheist"), 1, 0),
           ra_bahai = if_else(  str_detect(r, "Baha"), 1, 0),
           ra_buddhist = if_else(  str_detect(r, "Buddhist"), 1, 0),
           ra_christian = if_else(  str_detect(r, "Christian"), 1, 0),
           ra_hindu = if_else(  str_detect(r, "Hindu"), 1, 0),
           ra_muslim = if_else(  str_detect(r, "Muslim"), 1, 0),
           ra_witness = if_else(  str_detect(r, "Jehovah"), 1, 0),
           ra_jewish = if_else(  str_detect(r, "Jewish"), 1, 0),
           ra_mormon = if_else(  str_detect(r, "Mormon"), 1, 0),
           ra_sikh = if_else(  str_detect(r, "Sikh"), 1, 0),
           ra_spiritual = if_else(  str_detect(r, "Spiritual"), 1, 0),
           ra_uu = if_else(  str_detect(r, "Unitarian"), 1, 0),
           ra_zoro = if_else(  str_detect(r, "Zoroastrian"), 1, 0),
           ra_other = if_else(  str_detect(r, "None"), 1, 0),
           ra_nothing = if_else(  str_detect(r, "Nothing"), 1, 0),
           ra_dk = if_else(  str_detect(r, "know"), 1, 0),
           ra_pntr = if_else(  str_detect(r, "Prefer"), 1, 0),
           ra_other_text = manipulate_text(r_15_text)) %>%
    mutate(ra_agnostic_desc = if_else(  ra_agnostic == 1, "Agnostic", NA_character_),
           ra_atheist_desc = if_else(  ra_atheist == 1, "Atheist", NA_character_),
           ra_bahai_desc = if_else(  ra_bahai == 1, "Bahá’í", NA_character_),
           ra_buddhist_desc = if_else(  ra_buddhist == 1, "Buddhist", NA_character_),
           ra_christian_desc = if_else(  ra_christian == 1,  "Christian (Baptist, Catholic, Christian Scientist, Episcopalian, Lutheran, Methodist, Nondenominational, Orthodox, Presbyterian, etc.)",  NA_character_),
           ra_hindu_desc = if_else(  ra_hindu == 1, "Hindu", NA_character_),
           ra_muslim_desc = if_else(  ra_muslim == 1, "Muslim", NA_character_),
           ra_witness_desc = if_else(  ra_witness == 1, "Jehovah’s Witness", NA_character_),
           ra_jewish_desc = if_else(  ra_jewish == 1, "Jewish", NA_character_),
           ra_mormon_desc = if_else(  ra_mormon == 1, "Mormon or LDS", NA_character_),
           ra_sikh_desc = if_else(  ra_sikh == 1, "Sikh", NA_character_),
           ra_spiritual_desc = if_else(  ra_spiritual == 1, "Spiritual but not religious", NA_character_),
           ra_uu_desc = if_else(  ra_uu == 1, "Unitarian/Universalist", NA_character_),
           ra_zoro_desc = if_else(  ra_zoro == 1, "Zoroastrian", NA_character_),
           ra_other_desc = if_else(  ra_other == 1, "Other", NA_character_),
           ra_nothing_desc = if_else(  ra_nothing == 1, "Nothing in particular", NA_character_),
           ra_dk_desc = if_else(  ra_dk == 1, "Don't know", NA_character_),
           ra_pntr_desc = if_else(  ra_pntr == 1, "Prefer not to respond", NA_character_),
           ra_cats = gsub(",", "|", r),ra_check = rowSums(across(ra_agnostic:ra_dk)),
           ra_desc = case_when(  ra_check == 1 & ra_agnostic == 1 ~ "Agnostic",
                                 ra_check == 1 & ra_atheist == 1 ~ "Atheist",
                                 ra_check == 1 & ra_bahai == 1 ~ "Bahá’í",
                                 ra_check == 1 & ra_buddhist == 1 ~ "Buddhist",
                                 ra_check == 1 & ra_christian == 1 ~ "Christian",
                                 ra_check == 1 & ra_hindu == 1 ~ "Hindu",
                                 ra_check == 1 & ra_muslim == 1 ~ "Muslim",
                                 ra_check == 1 & ra_witness == 1 ~ "Jehovah's Witness",
                                 ra_check == 1 & ra_jewish == 1 ~ "Jewish",
                                 ra_check == 1 & ra_mormon == 1 ~ "Mormon or LDS",
                                 ra_check == 1 & ra_sikh == 1 ~ "Sikh",
                                 ra_check == 1 & ra_spiritual == 1 ~ "Spiritual but not religious",
                                 ra_check == 1 & ra_uu == 1 ~ "Unitarian/Universalist",
                                 ra_check == 1 & ra_uu == 1 ~ "Zoroastrian",
                                 ra_check > 1 ~ "Mixture",
                                 ra_other == 1 ~ "Other Affiliation(s)",
                                 ra_nothing == 1 ~ "Nothing in particular",
                                 ra_dk == 1 ~ "Don't know")) %>%
    #ELL status
    mutate(efl_desc = factor(if_else(efl == "Yes", "English", "Not English"),
                             levels = c("Not English", "English"))) %>%
    select(
      id_bl, cohort, completion_status,
      gender_desc,
      gi_cats:gi_desc_bin_3, gi_agender:gi_pntr_desc,
      so_cats:so_desc_bin_3, so_asexual:so_pntr_desc,
      lgbtqia_desc,
      federal_ethnic_desc, re,
      international_student_flag:nation_of_citizenship_desc,
      education_level_desc,
      pe_1, pe_2, rc_pe_1, rc_pe_2, pe_1_desc, pe_2_desc, fg_surv, fg, fg_combo,
      sss, sss_desc, sss_desc_bin, fsc, fsc_desc, fsc_desc_bin, hsa_1, hsa_1_desc,
      d_cats:d_desc_bin_2, d_chronic:d_pntr_desc, d_barrier:d_service_desc,
      tfer:ntt,
      residency_desc, residency_desc_bin,
      fy, fy_desc,
      p1_desc, p1_desc_bin, p2_cats:p2_desc_bin_2, p2_expecting:p2_pntr_desc,
      ra_cats:ra_desc, ra_agnostic:ra_pntr_desc,
      efl, efl_desc,
      session_description
    )

  write_rds(
    df,
    paste0(
      path,
      name,
      year,
      ".csv"
    )
  )
}
