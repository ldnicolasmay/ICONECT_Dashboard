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


# **************************************** ----
# GET TELEPHONE SCREENING DATA ----

# fields_telscrn_arch_raw <- 
#   c('ts_sid',     # participant ID
#     'ts_dat',     # tel screen date
#     'ts_elg',     # tel screen elibility
#     'ts_en',      # tel screen ineligibility reason
#     'ts_en2',     # tel screen inelig. 'other' text
#     'ts_en3',     # tel screen inelig. 'multiple' text
#     'telephone_screening_complete' # tel screen form complete?
#   )
# fields_telscrn_arch <- fields_telscrn_arch_raw %>% paste(collapse = ',')
# json_telscrn_arch_mi <- RCurl::postForm(
#   uri=REDCAP_API_URI,
#   token=REDCAP_API_TOKEN_UM_SCREEN_ARCHIVE,
#   conent='record',
#   format='json',
#   type='flat',
#   fields=fields_scr_arc,
#   rawOrLabel='raw',
#   rawOrLabelHeaders='raw',
#   exportCheckboxLabel='false',
#   exportSurveyFields='false',
#   exportDataAccessGroups='false',
#   returnFormat='json'
# )

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
  dplyr::mutate(ts_sid = as.character(paste0('SCRN', ts_sid + 6000L)))
df_telscrn_curr_mi <- df_telscrn_curr_mi %>% 
  dplyr::mutate(ts_sid = as.character(paste0('SCRN', ts_sid + 6000L)))


# **************************************** ----
# JOIN TELEPHONE SCREENING DATA ----

# _ Convert archive `ts_en` to dummy variables ----
df_telscrn_arch_mi <- create_ts_en_dummy_vars(df_telscrn_arch_mi)
df_telscrn_arch_or <- create_ts_en_dummy_vars(df_telscrn_arch_or)

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

# # _ Factor-ize and order factors of `ts_elg_txt` ----
# df_telscrn_mi$ts_elg_txt <- 
#   factor(df_telscrn_mi$ts_elg_txt, levels = c('No', 'Not sure', 'Yes'))
# df_telscrn_or$ts_elg_txt <- 
#   factor(df_telscrn_or$ts_elg_txt, levels = c('No', 'Not sure', 'Yes'))
# 
# # _ Factor-ize and order factors of `ts_en_txt` ----
# df_telscrn_mi$ts_en_txt <- 
#   factor(df_telscrn_mi$ts_en_txt,
#          levels = c('Social', 'Medical', 'Not Interested', 'Age', 'Other'))
# df_telscrn_or$ts_en_txt <- 
#   factor(df_telscrn_or$ts_en_txt,
#          levels = c('Social', 'Medical', 'Not Interested', 'Age', 'Other'))


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
                               # filter(!is.na(ts_elg_txt)) %>%
                               group_by(ts_dat_week_lab, ts_elg_txt) %>% 
                               summarize(n = n()))

# _ Ineligibility reason summary tables ----
telscrn_en_summ <- map(df_telscrn,
                       ~ .x %>% 
                         # filter(ts_elg != 1) %>% 
                         group_by(ts_en_txt) %>% 
                         summarize(n = n()))

# _ Ineligibility reason summary by week tables ----
telscrn_en_summ_week <- map(df_telscrn,
                            ~ .x %>% 
                              # filter(!is.na(ts_en_txt)) %>%
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

# _ _ Factorize ----
telscrn_elg_summ$mi$ts_elg_txt <- factor(telscrn_elg_summ$mi$ts_elg_txt,
                                         levels = c('No',
                                                    'Not sure',
                                                    'Yes',
                                                    '[NA]'))
telscrn_elg_summ$or$ts_elg_txt <- factor(telscrn_elg_summ$or$ts_elg_txt,
                                         levels = c('No',
                                                    'Not sure',
                                                    'Yes',
                                                    '[NA]'))
telscrn_elg_summ_week$mi$ts_elg_txt <- factor(telscrn_elg_summ_week$mi$ts_elg_txt,
                                         levels = c('No',
                                                    'Not sure',
                                                    'Yes',
                                                    '[NA]'))
telscrn_elg_summ_week$or$ts_elg_txt <- factor(telscrn_elg_summ_week$or$ts_elg_txt,
                                              levels = c('No',
                                                         'Not sure',
                                                         'Yes',
                                                         '[NA]'))

# _ _ Add eligibility status proportion column ----
telscrn_elg_summ <- map(telscrn_elg_summ, telscrn_elg_add_proportion_column)

# _ _ Add eligibility status total row ----
telscrn_elg_summ <- map(telscrn_elg_summ, telscrn_elg_add_total_row)

# _ Ineligibility reason ----

# _ _ Add missing ts_en categories ----
telscrn_en_summ <- map(telscrn_en_summ, telscrn_en_summ_add_missing_categs)
telscrn_en_summ_week <- map(telscrn_en_summ_week, telscrn_en_summ_week_add_missing_categs)

# _ _ Insert "NA" string into NA row ----
telscrn_en_summ <- map(telscrn_en_summ, telscrn_en_insert_na_string)
telscrn_en_summ_week <-
  map(telscrn_en_summ_week, telscrn_en_insert_na_string)

# _ _ Factorize ----
telscrn_en_summ$mi$ts_en_txt <- factor(telscrn_en_summ$mi$ts_en_txt,
                                        levels = c('Medical',
                                                   'Social',
                                                   'Age',
                                                   'Not Interested',
                                                   'Other',
                                                   '[NA]'))
telscrn_en_summ$or$ts_en_txt <- factor(telscrn_en_summ$or$ts_en_txt,
                                        levels = c('Medical',
                                                   'Social',
                                                   'Age',
                                                   'Not Interested',
                                                   'Other',
                                                   '[NA]'))
telscrn_en_summ_week$mi$ts_en_txt <- factor(telscrn_en_summ_week$mi$ts_en_txt,
                                        levels = c('Medical',
                                                   'Social',
                                                   'Age',
                                                   'Not Interested',
                                                   'Other',
                                                   '[NA]'))
telscrn_en_summ_week$or$ts_en_txt <- factor(telscrn_en_summ_week$or$ts_en_txt,
                                            levels = c('Medical',
                                                       'Social',
                                                       'Age',
                                                       'Not Interested',
                                                       'Other',
                                                       '[NA]'))

# _ _ Add ineligibility reason proportion column ----
telscrn_en_summ <- map(telscrn_en_summ, telscrn_en_add_proportion_column)

# _ _ Add ineligibility reason total row ----
telscrn_en_summ <- map(telscrn_en_summ, telscrn_en_add_total_row)


# **************************************** ----
# SAVE DATA TO RDS ----

# _ Eligibility status summary tables ----
iwalk(telscrn_elg_summ,
      ~ saveRDS(.x, paste0('rds/telscrn_elg_summ_', .y, '.Rds')))

# _ Eligibility status summary by week tables ----
iwalk(telscrn_elg_summ_week,
      ~ saveRDS(.x, paste0('rds/telscrn_elg_summ_week_', .y, '.Rds')))

# _ Ineligibility reason summary tables ----
iwalk(telscrn_en_summ,
      ~ saveRDS(.x, paste0('rds/telscrn_en_summ_', .y, '.Rds')))

# _ Ineligibility reason summary by week tables ----
iwalk(telscrn_en_summ_week,
      ~ saveRDS(.x, paste0('rds/telscrn_en_summ_week_', .y, '.Rds')))


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
