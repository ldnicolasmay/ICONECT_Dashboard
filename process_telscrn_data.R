#!/usr/bin/env Rscript

# process_telscrn_data.R


# **************************************** ----
# USEFUL LIBRARIES ----
library(dplyr)
library(purrr)


# **************************************** ----
# VARIABLES & HELPER FUNCTIONS ----
source('config.R')
source('helper_fxns_telscrn.R')
get_api_data <- FALSE

# **************************************** ----
# GET TELEPHONE SCREENING DATA ----

# _ Field definition ----

# _ _ Archive fields ----
fields_telscrn_arch_raw <-
  c('rm_sid',     # participant ID
    'ts_dat',     # tel screen date
    'ts_elg',     # tel screen elibility
    'ts_en',      # tel screen ineligibility reason
    'ts_en2',     # tel screen inelig. 'other' text
    'telephone_screening_complete' # tel screen form complete?
  )
fields_telscrn_arch <- fields_telscrn_arch_raw %>% paste(collapse = ',')

# _ _ Current fields ----
fields_telscrn_curr_raw <-
  c('ts_sid',     # participant ID
    'ts_dat',     # tel screen date
    'ts_elg',     # tel screen elibility
    'ts_en',  # tel screen ineligibility reason, dummy variables
    'ts_en2',     # tel screen inelig. 'other' text
    'telephone_screening_complete' # tel screen form complete?
  )
fields_telscrn_curr <- fields_telscrn_curr_raw %>% paste(collapse = ',')

# _ API Data Retrieval ----

# _ _ Archive UM ----
if (get_api_data) {
  json_telscrn_arch_mi <- RCurl::postForm(
    uri=REDCAP_API_URI,
    token=REDCAP_API_TOKEN_SCREEN_ARCHIVE_UM,
    content='record',
    format='json',
    type='flat',
    fields=fields_telscrn_arch,
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='false',
    returnFormat='json'
  )
}
df_telscrn_arch_mi <- jsonlite::fromJSON(json_telscrn_arch_mi) %>%
  dplyr::na_if('')

# _ _ Archive OHSU ----
if (get_api_data) {
  json_telscrn_arch_or <- RCurl::postForm(
    uri=REDCAP_API_URI,
    token=REDCAP_API_TOKEN_SCREEN_ARCHIVE_OHSU,
    content='record',
    format='json',
    type='flat',
    fields=fields_telscrn_arch,
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='false',
    returnFormat='json'
  )
}
df_telscrn_arch_or <- jsonlite::fromJSON(json_telscrn_arch_or) %>%
  dplyr::na_if('')

# _ _ Current UM ----
if (get_api_data) {
  json_telscrn_curr_mi <- RCurl::postForm(
    uri=REDCAP_API_URI,
    token=REDCAP_API_TOKEN_SCREEN_CURRENT_UM,
    content='record',
    format='json',
    type='flat',
    fields=fields_telscrn_curr,
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='false',
    returnFormat='json'
  )
}
df_telscrn_curr_mi <- jsonlite::fromJSON(json_telscrn_curr_mi) %>%
  dplyr::na_if('')

# _ _ Current OHSU ----
if (get_api_data) {
  json_telscrn_curr_or <- RCurl::postForm(
    uri=REDCAP_API_URI,
    token=REDCAP_API_TOKEN_SCREEN_CURRENT_OHSU,
    content='record',
    format='json',
    type='flat',
    fields=fields_telscrn_curr,
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='false',
    returnFormat='json'
  )
}
df_telscrn_curr_or <- jsonlite::fromJSON(json_telscrn_curr_or) %>%
  dplyr::na_if('')

# Temporary data source: XLSX files
# _ Load XLSX ----
df_telscrn_arch_mi <-
  readr::read_csv(paste0('OCTRI5793Internetbas_DATA_',
                         '2018-12-10_0648_Main_UM_Screening_Archive.csv'))
df_telscrn_arch_or <-
  readr::read_csv(paste0('OCTRI5793Internetbas_DATA_',
                         '2018-12-10_0647_Main_OSHU_Screening_Archive.csv'))
df_telscrn_curr_mi <-
  readr::read_csv(paste0('OCTRI5793Internetbas_DATA_',
                         '2018-12-16_1728_Main_UM_Screening_Current.csv'))
df_telscrn_curr_or <-
  readr::read_csv(paste0('OCTRI5793Internetbas_DATA_',
                         '2018-12-10_0650_Main_OHSU_Screening_Current.csv'))


# **************************************** ----
# CLEAN TELEPHONE SCREENING DATA ----

# _ Archive: Select fields ----
df_telscrn_arch_mi <- arch_select_fields(df_telscrn_arch_mi)
df_telscrn_arch_or <- arch_select_fields(df_telscrn_arch_or)

# _ Current: Select fields ----
df_telscrn_curr_mi <- curr_select_fields(df_telscrn_curr_mi)
df_telscrn_curr_or <- curr_select_fields(df_telscrn_curr_or)

# _ Archive: Toss out NAs in `ts_dat` field ----
df_telscrn_arch_mi <- df_telscrn_arch_mi %>% 
  dplyr::filter(!is.na(ts_dat))
df_telscrn_arch_or <- df_telscrn_arch_or %>% 
  dplyr::filter(!is.na(ts_dat))

# _ Current: Toss out NAs in `ts_dat` field ----
df_telscrn_curr_mi <- df_telscrn_curr_mi %>% 
  dplyr::filter(!is.na(ts_dat))
df_telscrn_curr_or <- df_telscrn_curr_or %>% 
  dplyr::filter(!is.na(ts_dat))

# _ Resolve different `ts_sid` nomenclatures ----
df_telscrn_arch_mi <- df_telscrn_arch_mi %>% 
  dplyr::mutate(ts_sid = 
                  as.character(paste0('SCRN', as.integer(ts_sid) + 6000L)))
df_telscrn_curr_mi <- df_telscrn_curr_mi %>% 
  dplyr::mutate(ts_sid = 
                  as.character(paste0('SCRN', as.integer(ts_sid) + 6000L)))


# **************************************** ----
# JOIN TELEPHONE SCREENING DATA ----

# _ Convert archive `ts_en` to dummy variables ----
df_telscrn_arch_mi <- create_ts_en_dummy_vars(df_telscrn_arch_mi)
df_telscrn_arch_or <- create_ts_en_dummy_vars(df_telscrn_arch_or)

# _ Ensure matching types across all fields ----
df_telscrn_arch_mi <- ensure_matching_types(df_telscrn_arch_mi)
df_telscrn_arch_or <- ensure_matching_types(df_telscrn_arch_or)
df_telscrn_curr_mi <- ensure_matching_types(df_telscrn_curr_mi)
df_telscrn_curr_or <- ensure_matching_types(df_telscrn_curr_or)

# _ Bind rows of df_telscrn_arch + df_telscrn_curr; Order fields ----
df_telscrn_mi <- bind_arch_curr(df_telscrn_arch_mi, df_telscrn_curr_mi)
df_telscrn_or <- bind_arch_curr(df_telscrn_arch_or, df_telscrn_curr_or)

# Put df_telscrn_mi, df_telscrn_or dfs in a named list ----
df_telscrn <- list('mi' = df_telscrn_mi,
                   'or' = df_telscrn_or)

# _ Remove unneeded dfs ----
rm(df_telscrn_arch_mi); rm(df_telscrn_arch_or)
rm(df_telscrn_curr_mi); rm(df_telscrn_curr_or)
rm(df_telscrn_mi); rm(df_telscrn_or)

# **************************************** ----
# MUTATE TELEPHONE SCREENING DATA ----

# _ Mutate week numbers (and week labels) ----
df_telscrn <- map(df_telscrn, add_week_numbers)

# _ Mutate ineligibilty text field: ts_en_txt ----
# Based on `ts_en___*` dummy variables
df_telscrn <- map(df_telscrn, mutate_ts_en_txt_field)

# _ Mutate eligibility text fields to augment `ts_elg`: `ts_elg_txt` ----
df_telscrn <- map(df_telscrn, mutate_ts_elg_txt_field)


# **************************************** ----
# SUMMARIZE TELEPHONE SCREENING DATA ----

# _ Eligibility status summary tables ----
telscrn_elg_summ <- map(df_telscrn,
                        ~ .x %>% 
                          group_by(ts_elg_txt) %>% 
                          summarize(n = dplyr::n()))

# _ Eligibility status summary by week tables ----
telscrn_elg_summ_week <- map(df_telscrn,
                             ~ .x %>% 
                               group_by(ts_dat_week_lab, ts_elg_txt) %>% 
                               summarize(n = n()))

# _ Ineligibility reason summary tables ----
telscrn_en_summ <- map(df_telscrn,
                       ~ .x %>% 
                         group_by(ts_en_txt) %>% 
                         summarize(n = n()))

# _ Ineligibility reason summary by week tables ----
telscrn_en_summ_week <- map(df_telscrn,
                            ~ .x %>% 
                              group_by(ts_dat_week_lab, ts_en_txt) %>% 
                              summarize(n = n()))


# **************************************** ----
# RECRUITMENT STATUS + LEAD SUMMARY TABLES ----

# _ Eligibility status ----

# _ _ Add missing ts_elg categories ----
telscrn_elg_summ <- map(telscrn_elg_summ, telscrn_elg_summ_add_missing_categs)

# _ _ Insert "NA" string into NA row ----
telscrn_elg_summ <- map(telscrn_elg_summ, telscrn_elg_insert_na_string)
telscrn_elg_summ_week <-
  map(telscrn_elg_summ_week, telscrn_elg_insert_na_string)

# _ _ Factorize `ts_elg_txt` field ----
telscrn_elg_summ <- map(telscrn_elg_summ, telscrn_elg_factorize_elg_txt)
telscrn_elg_summ_week <- 
  map(telscrn_elg_summ_week, telscrn_elg_factorize_elg_txt)

# _ _ Add eligibility status proportion column ----
telscrn_elg_summ <- map(telscrn_elg_summ, telscrn_elg_add_proportion_column)

# _ _ Add eligibility status total row ----
telscrn_elg_summ <- map(telscrn_elg_summ, telscrn_elg_add_total_row)

# _ Ineligibility reason ----

# _ _ Add missing ts_en categories ----
telscrn_en_summ <- map(telscrn_en_summ, telscrn_en_summ_add_missing_categs)
telscrn_en_summ_week <- 
  map(telscrn_en_summ_week, telscrn_en_summ_week_add_missing_categs)

# _ _ Insert "NA" string into NA row ----
telscrn_en_summ <- map(telscrn_en_summ, telscrn_en_insert_na_string)
telscrn_en_summ_week <-
  map(telscrn_en_summ_week, telscrn_en_insert_na_string)

# _ _ Factorize `ts_en_txt` field ----
telscrn_en_summ <- map(telscrn_en_summ, telscrn_en_factorize_en_txt)
telscrn_en_summ_week <- 
  map(telscrn_en_summ_week, telscrn_en_factorize_en_txt)

# _ _ Add ineligibility reason proportion column ----
telscrn_en_summ <- map(telscrn_en_summ, telscrn_en_add_proportion_column)

# _ _ Add ineligibility reason total row ----
telscrn_en_summ <- map(telscrn_en_summ, telscrn_en_add_total_row)


# **************************************** ----
# SAVE DATA TO RDS ----

# _ Eligibility status summary tables ----
saveRDS(telscrn_elg_summ, 'rds/telscrn_elg_summ.Rds')

# _ Eligibility status summary by week tables ----
saveRDS(telscrn_elg_summ_week, 'rds/telscrn_elg_summ_week.Rds')

# _ Ineligibility reason summary tables ----
saveRDS(telscrn_en_summ, 'rds/telscrn_en_summ.Rds')

# _ Ineligibility reason summary by week tables ----
saveRDS(telscrn_en_summ_week, 'rds/telscrn_en_summ_week.Rds')


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
