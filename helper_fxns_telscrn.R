# telscrn_helper_fxns.R

# TELEPHONE SCREENING DATA HELPER FUNCTIONS


# **************************************** ----
# CLEAN TELEPHONE SCREENING DATA ----

arch_select_fields <- function(df) { 
  df %>% 
    select(ts_sid = rm_sid,  # participant ID
           ts_dat,     # tel screen date
           ts_elg,     # tel screen elibility 
           ts_en,      # tel screen ineligibility reason
           ts_en2,     # tel screen inelig. 'other' text
           # ts_en3,     # tel screen inelig. 'multiple' text
           telephone_screening_complete # tel screen form complete?
    )
}

curr_select_fields <- function(df) {
  df %>% 
    select(ts_sid,     # participant ID
           ts_dat,     # tel screen date
           ts_elg,     # tel screen elibility
           ts_en___1,  # tel screen ineligibility reason, dummy 1
           ts_en___2,  # tel screen ineligibility reason, dummy 2
           ts_en___3,  # tel screen ineligibility reason, dummy 3
           ts_en___4,  # tel screen ineligibility reason, dummy 4
           ts_en___5,  # tel screen ineligibility reason, dummy 5
           ts_en2,     # tel screen inelig. 'other' text
           telephone_screening_complete # tel screen form complete?
    )
}


# **************************************** ----
# JOIN TELEPHONE SCREENING DATA ----

# _ Convert archive `ts_en` to dummy variables ----
create_ts_en_dummy_vars <- function(df) {
  df %>% 
    mutate(
      ts_en___1 = case_when(
        ts_en == 1L ~ 1L,
        TRUE ~ 0L),
      ts_en___2 = case_when(
        ts_en == 2L ~ 1L,
        TRUE ~ 0L),
      ts_en___3 = case_when(
        ts_en == 3L ~ 1L,
        TRUE ~ 0L),
      ts_en___4 = case_when(
        TRUE ~ 0L),
      ts_en___5 = case_when(
        ts_en == 4L ~ 1L,
        TRUE ~ 0L)
    ) %>% 
    select(-ts_en)
}

# _ Bind rows of df_telscrn_arch + df_telscrn_curr; Order fields ----
bind_arch_curr <- function(df_arch, df_curr) {
  bind_rows(df_arch, df_curr) %>% 
    select(ts_sid, ts_dat,
           ts_elg,
           ts_en___1, ts_en___2, ts_en___3, ts_en___4, ts_en___5,
           ts_en2,
           telephone_screening_complete
    )
}


# **************************************** ----
# MUTATE TELEPHONE SCREENING DATA ----

# _ Add week numbers (and week labels) ----
add_week_numbers <- function(df) { 
  ts_dat_start = as.Date('2018-07-01')
  df %>% 
    mutate(ts_dat_week = 
             # # Option 1:
             # (lubridate::interval(ts_dat_start, ts_dat) %/%
             #    lubridate::weeks(1))) %>%
             # Option 2:
             floor(
               lubridate::interval(ts_dat_start, ts_dat) /
                 lubridate::dweeks(1)
             )) %>% 
    mutate(ts_dat_week_lab = 
             ts_dat_start + lubridate::dweeks(ts_dat_week))
}

# _ Mutate ineligibilty text field ----
# Based on `ts_en___*` dummy variables
mutate_ts_en_txt_field <- function(df) {
  df %>% 
    rowwise() %>%
    mutate(ts_en_txt =
             paste0(c('Medical', 
                      'Social', 
                      'Not Interested', 
                      'Age', 
                      'Other')[
                        as.logical(c(ts_en___1, 
                                     ts_en___2, 
                                     ts_en___3, 
                                     ts_en___4, 
                                     ts_en___5))
                        ], collapse = ', ')) %>% 
    mutate(ts_en_txt = case_when(
      ts_en_txt != '' ~ ts_en_txt,
      ts_en_txt == '' ~ NA_character_
    )) %>% 
    ungroup() # strip off rowwise nature
}

# _ Mutate eligibility text fields to augment `ts_elg`: `ts_elg_txt` ----
mutate_ts_elg_txt_field <- function(df) {
  df %>% 
    mutate(ts_elg_txt = case_when(
      ts_elg == 0 ~ 'No',
      ts_elg == 1 ~ 'Yes',
      ts_elg == 2 ~ 'Not sure',
      TRUE ~ NA_character_
    ))
}


# **************************************** ----
# RECRUITMENT STATUS + LEAD SUMMARY TABLES ----

# _ Eligibility status ----

telscrn_elg_summ_add_missing_categs <- function(df) {
  # Vector of all ts_en categories
  telscrn_en_vct <- c('No',
                      'Not sure',
                      'Yes',
                      NA_character_)
  # Loop over df to ensure all 5 `ts_en_txt*` categ.s are present
  for (t in telscrn_en_vct) {
    if (!(t %in% df$ts_elg_txt)) {
      if (!is.na(t)) {
        df = rbind(
          df,
          tibble::tibble(ts_elg_txt = t,
                         n = 0L))
      } else {
        df = rbind(
          df,
          tibble::tibble(ts_elg_txt = NA_character_,
                         n = 0L)
        )
      }
    }
  }
  return(df)
}

# _ _ Insert "NA" string into NA row ----
telscrn_elg_insert_na_string <- function(df) {
  df %>% 
    mutate(ts_elg_txt = case_when(
      is.na(ts_elg_txt) ~ '[NA]',
      TRUE ~ ts_elg_txt
    ))
}

# _ _ Add eligibility status proportion column ----
telscrn_elg_add_proportion_column <- function(df) {
  df %>% 
    bind_cols(proportion = format(round(.$n / sum(.$n), 2), 2))
}

# _ _ Add eligibility status total row ----
telscrn_elg_add_total_row <- function(df) {
  df %>% 
    bind_rows(list(ts_elg_txt = 'TOTAL',
                   n = sum(.$n)))
}

# _ Ineligibility reason ----

telscrn_en_summ_add_missing_categs <- function(df) {
  # Vector of all ts_en categories
  telscrn_en_vct = c('Medical',
                     'Social',
                     'Age',
                     'Not Interested',
                     'Other',
                     NA_character_)
  # Loop over df to ensure all 5 `ts_en_txt*` categ.s are present
  for (t in telscrn_en_vct) {
    if (!(t %in% df$ts_en_txt)) {
      if (!is.na(t)) {
        df = rbind(
          df,
          tibble::tibble(ts_en_txt = t,
                         n = 0L))
      } else {
        df = rbind(
          df,
          tibble::tibble(ts_en_txt = NA_character_,
                         n = 0L)
        )
      }
    }
  }
  df = df[match(telscrn_en_vct, df$ts_en_txt), ]
  return(df)
}

telscrn_en_summ_week_add_missing_categs <- function(df) {
  # Vector of all ts_en categories
  telscrn_en_vct = c('Medical',
                     'Social',
                     'Age',
                     'Not Interested',
                     'Other',
                     NA_character_)
  dummy_date <- as.Date('2018-07-01')
  # Loop over df to ensure all 5 `ts_en_txt*` categ.s are present
  for (t in telscrn_en_vct) {
    if (!(t %in% df$ts_en_txt)) {
      if (!is.na(t)) {
        df = bind_rows(
          df,
          tibble::tibble(ts_dat_week_lab = dummy_date,
                         ts_en_txt = t,
                         n = 0L))
      } else {
        df = bind_rows(
          df,
          tibble::tibble(ts_dat_week_lab = dummy_date,
                         ts_en_txt = NA_character_,
                         n = 0L)
        )
      }
    }
  }
  return(df)
}

# _ _ Insert "NA" string into NA row ----
telscrn_en_insert_na_string <- function(df) {
  df %>% 
    mutate(ts_en_txt = case_when(
      is.na(ts_en_txt) ~ '[NA]',
      TRUE ~ ts_en_txt
    ))
}

# _ _ Add ineligibility reason proportion column ----
telscrn_en_add_proportion_column <- function(df) {
  df %>% 
    bind_cols(proportion = format(round(.$n / sum(.$n), 2), 2))
}

# _ _ Add ineligibility reason total row ----
telscrn_en_add_total_row <- function(df) {
  df %>% 
    bind_rows(list(ts_en_txt = 'TOTAL',
                   n = sum(.$n)))
}


###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
