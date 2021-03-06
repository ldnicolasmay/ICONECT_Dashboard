# recruit_helper_fxns.R

# RECRUITMENT DATA HELPER FUNCTIONS

library(dplyr)

# **************************************** ----
# CLEAN RECRUITMENT DATA FXNS ----

# Helper function
# Function that returns a logical vector of which rows are non-empty
locate_nonempty_records <- function(df) {
  has_data_lgl <- logical(length = nrow(df))
  for (i in seq_len(nrow(df))) {
    row_list <- as.list(df[i, ])
    if (any(!is.na(row_list))) {
      has_data_lgl[i] <- TRUE
    } else {
      has_data_lgl[i] <- FALSE
    }
  }
  has_data_lgl
}

# Helper function
# Function that cleans out rows where all relevant fields are empty
get_nonempty_records <- function(df, relevant_fields = names(df)) {
  nonempty_rows <- locate_nonempty_records(df[, relevant_fields])
  df[nonempty_rows, ]
}

# _ Remove all NA rows ----
remove_na_rows <- function(df) {
  df %>% 
    filter(!is.na(`Recruitment Source`) | 
                    !is.na(`Recruitment Status`) | 
                    !is.na(`Not Elig. Medical`) |
                    !is.na(`Not Elig. Social`) |
                    !is.na(`Not Elig. Age`) |
                    !is.na(`Not Elig. Other`) | 
                    !is.na(`Date to Call`) | 
                    !is.na(`Date of Initial Contact`))
}

# _ Cast `Date to Call`, `Date of Initial Contact` as Date ----
cast_date_cols <- function(df) {
  df %>% 
    mutate(`Date to Call` = 
                    as.Date(as.integer(`Date to Call`), 
                            origin = '1900-01-01'),
                  `Date of Initial Contact` = 
                    as.Date(as.integer(`Date of Initial Contact`),
                            origin = '1900-01-01'))
}

# _ Mutate `Not Elig. *` fields ----
# TRUE => 1, [blank] => 0
binarize_notelig_fields <- function(df) {
  df %>% 
    mutate(
      `Not Elig. Medical` = case_when(
        `Not Elig. Medical` == 'TRUE' ~ 1L,
        TRUE ~ 0L),
      `Not Elig. Social` = case_when(
        `Not Elig. Social` == 'TRUE' ~ 1L,
        TRUE ~ 0L),
      `Not Elig. Age` = case_when(
        `Not Elig. Age` == 'TRUE' ~ 1L,
        TRUE ~ 0L),
      `Not Elig. Other` = case_when(
        `Not Elig. Other` == 'TRUE' ~ 1L,
        TRUE ~ 0L)
    )
}


# **************************************** ----
# RECRUITMENT STATUS SUMMARY TABLE FXNS ----

# _ Create recruitment status summary table / df ----
create_recruit_status_summ <- function(df) {
  df %>% 
    select(`Recruitment Status`) %>% 
    group_by(`Recruitment Status`) %>% 
    summarize(n = n()) %>% 
    rename(recruit_stat_txt = `Recruitment Status`)
}

# _ Mutate recruit_stat_long field ----
# Recruitment status long text categories
mutate_recruit_stat_long <- function(df) {
  df %>% 
    mutate(recruit_stat_long = case_when(
      recruit_stat_txt == 'UNR' ~ 'Unreachable',
      recruit_stat_txt == 'NNO' ~ 'Number non-operational',
      recruit_stat_txt == 'PCN' ~ 'Pending contact',
      recruit_stat_txt == 'PCF' ~ 'Pending consent',
      recruit_stat_txt == 'NE'  ~ 'Not eligible',
      recruit_stat_txt == 'NIN' ~ 'Not interested',
      recruit_stat_txt == 'PEG' ~ 'Pending eligibility determination',
      recruit_stat_txt == 'PTS' ~ 'Pending telephone screen',
      recruit_stat_txt == 'TSF' ~ 'Telephone screen fail',
      recruit_stat_txt == 'PHS' ~ 'Pending home screening visit',
      recruit_stat_txt == 'ROT' ~ 'Opt-out slip received',
      recruit_stat_txt == 'RTS' ~ 'Opt-out/Opt-in letter returned',
      recruit_stat_txt == 'SOT' ~ 'Opt-out letter sent',
      recruit_stat_txt == 'OIS' ~ 'Opt-in letter sent',
      recruit_stat_txt == 'DEC' ~ 'Deceased',
      recruit_stat_txt == 'ENR' ~ 'Enrolled',
      TRUE ~ NA_character_
    ))
}

# _ Mutate recruit_stat field ----
# Recruitment status integer categories
mutate_recruit_stat <- function(df) {
  # Set up named chr. vectors
  recruit_vct = 
    c('1' = 'PCN', 
      '2' = 'PCF', 
      '3' = 'PEG', 
      '4' = 'PTS', 
      '5' = 'PHS', 
      '6' = 'SOT', 
      '7' = 'OIS', 
      '8' = 'UNR',
      '9' = 'NNO', 
      '10' = 'NE', 
      '11' = 'NIN', 
      '12' = 'TSF', 
      '13' = 'ROT', 
      '14' = 'RTS', 
      '15' = 'DEC', 
      '16' = 'ENR', 
      '17' = NA)
  recruit_vct_long = 
    c('PCN' = 'Pending contact', 
      'PCF' = 'Pending consent', 
      'PEG' = 'Pending elegibility determination', 
      'PTS' = 'Pending telephone screen',
      'PHS' = 'Pending home screening visit', 
      'SOT' = 'Opt-out letter sent',
      'OIS' = 'Opt-in letter sent',
      'UNR' = 'Unreachable', 
      'NNO' = 'Number non-operational', 
      'NE'  = 'Not eligible', 
      'NIN' = 'Not interested', 
      'TSF' = 'Telephone screen fail', 
      'ROT' = 'Opt-out slip received', 
      'RTS' = 'Opt-out/Opt-in letter returned', 
      'DEC' = 'Deceased', 
      'ENR' = 'Enrolled')
  # Mutate `recruit_stat` field
  df = df %>% 
    mutate(recruit_stat = case_when(
      recruit_stat_txt == "PCN" ~ 1L,
      recruit_stat_txt == "PCF" ~ 2L,
      recruit_stat_txt == "PEG" ~ 3L,
      recruit_stat_txt == "PTS" ~ 4L,
      recruit_stat_txt == "PHS" ~ 5L,
      recruit_stat_txt == "SOT" ~ 6L,
      recruit_stat_txt == "OIS" ~ 7L,
      recruit_stat_txt == "UNR" ~ 8L,
      recruit_stat_txt == "NNO" ~ 9L,
      recruit_stat_txt == "NE"  ~ 10L,
      recruit_stat_txt == "NIN" ~ 11L,
      recruit_stat_txt == "TSF" ~ 12L,
      recruit_stat_txt == "ROT" ~ 13L,
      recruit_stat_txt == "RTS" ~ 14L,
      recruit_stat_txt == "DEC" ~ 15L,
      recruit_stat_txt == "ENR" ~ 16L,
      is.na(recruit_stat_txt) ~ NA_integer_
    )) 
  # Loop over df to ensure all 17 `recruit_stat*` categ.s are present
  for (r in recruit_vct) {
    if (!(r %in% df$recruit_stat_txt)) {
      if(!is.na(r)) {
        df = rbind(
          df,
          tibble::tibble(recruit_stat_txt = r,
                         n = 0L,
                         recruit_stat_long = recruit_vct_long[r],
                         recruit_stat = which(recruit_vct == r)))
      } else {
        df = rbind(
          df,
          tibble::tibble(recruit_stat_txt = r,
                         n = 0L,
                         recruit_stat_long = recruit_vct_long[r],
                         recruit_stat = NA_character_))
      } # ... end if-else
    } # ... end if
  } # ... end for loop
  # Arrange df by `recruit_stat` field
  df = df %>% 
    arrange(as.integer(recruit_stat))
  # Return df
  return(df)
}


# **************************************** ----
# RECRUITMENT LEAD SUMMARY TABLE FXNS ----

# Create recruit_lead_summ_* dfs from recruit_status_summ_* dfs ----
create_recruit_lead_summ <- function(df) {
  # Set up named chr. vector
  lead_categ_vct <- c('1' = 'Warm lead', 
                      '2' = 'Cold lead', 
                      '3' = 'Dead lead', 
                      '4' = NA_character_)
  # Associate each recruitment status with a lead categor
  # 1 = warm, 2 = cold, 3 = dead, 4 = enrolled
  df = df %>% 
    mutate(lead_categ = case_when(
      # Warm leads
      recruit_stat == 1L ~ 1L,  # PCN
      recruit_stat == 2L ~ 1L,  # PCF
      recruit_stat == 3L ~ 1L,  # PEG
      recruit_stat == 4L ~ 1L,  # PTS
      recruit_stat == 5L ~ 1L,  # PHS
      recruit_stat == 6L ~ 1L,  # SOT
      # Cold leads
      recruit_stat == 7L ~ 2L,  # OIS
      # Dead leads
      recruit_stat == 8L ~ 3L,  # UNR
      recruit_stat == 9L ~ 3L,  # NNO
      recruit_stat == 10L ~ 3L, # NE
      recruit_stat == 11L ~ 3L, # NIN
      recruit_stat == 12L ~ 3L, # TSF
      recruit_stat == 13L ~ 3L, # ROT
      recruit_stat == 14L ~ 3L, # RTS
      recruit_stat == 15L ~ 3L, # DEC
      # Enrolled
      recruit_stat == 16L ~ 4L  # ENR
    ))
  # Summarize recruit_status_summ df to recruit_lead_summ df
  df = df %>% 
    select(n, lead_categ) %>% 
    group_by(lead_categ) %>% 
    summarize(n = sum(n)) %>% 
    mutate(lead_categ_txt = case_when(
      lead_categ == 1 ~ 'Warm lead',
      lead_categ == 2 ~ 'Cold lead',
      lead_categ == 3 ~ 'Dead lead',
      lead_categ == 4 ~ 'Enrolled',
      TRUE ~ NA_character_
    ))
  # Loop over df to ensure all 5 `lead_categ_txt` categ.s are present
  for (l in lead_categ_vct) {
    if (!(l %in% df$lead_categ_txt)) {
      df <- rbind(
        df,
        data.frame(lead_categ = which(lead_categ_vct == l),
                   n = 0L,
                   lead_categ_txt = l)
      )
    }
  }
  # Arrange df by `lead_categ`
  df = df %>% 
    arrange(lead_categ)
  # Return df
  return(df)
}

# **************************************** ----
# RECRUITMENT STATUS + LEAD SUMMARY TABLES ----

# _ Insert "NA" string to NA row of `recruit_status_*` dfs ----
recruit_status_insert_na_string <- function(df) {
  df %>% 
    mutate(
      recruit_stat_txt = case_when(
        is.na(recruit_stat_txt) ~ "[NA]",
        TRUE ~ recruit_stat_txt),
      recruit_stat_long = case_when(
        is.na(recruit_stat_long) ~ "[NA]",
        TRUE ~ recruit_stat_long
      )
    )
}

# _ Insert "NA" string to NA row of `recruit_lead_*` dfs ----
recruit_lead_insert_na_string <- function(df) {
  df %>% 
    mutate(
      lead_categ_txt = case_when(
        is.na(lead_categ_txt) ~ "[NA]",
        TRUE ~ lead_categ_txt)
    )
}

# _ Add proportion column to recruitment status summary tables ----
rs_add_proportion_column <- function(df) {
  df %>% 
    bind_cols(proportion = format(round(.$n / sum(.$n), 2), 2))
}

# _ Add total row to recruitment status summary tables ----
rs_add_total_row <- function(df) {
  df %>% 
    bind_rows(list(recruit_stat_txt = 'TOTAL',
                   n = sum(.$n),
                   recruit_stat_long = 'TOTAL',
                   recruit_stat = NA_integer_))
}

# _ Add proportion column to recruitment lead summary tables ----
rl_add_proportion_column <- function(df) {
  df %>% 
    bind_cols(proportion = format(round(.$n / sum(.$n), 2), 2))
}

# _ Add total row to recruitment lead summary tables ----
rl_add_total_row <- function(df) {
  df %>% 
    bind_rows(list(lead_categ = NA_integer_,
                   n = sum(.$n),
                   lead_categ_txt = 'TOTAL'))
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
