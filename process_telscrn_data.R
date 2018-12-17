#!/usr/bin/env Rscript

# process_telscrn_data.R


# **************************************** ----
# USEFUL LIBRARIES ----
library(dplyr)


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

# _ Remove unneeded dfs ----
rm(df_telscrn_arch_mi); rm(df_telscrn_arch_or)
rm(df_telscrn_curr_mi); rm(df_telscrn_curr_or)


# **************************************** ----
# MUTATE TELEPHONE SCREENING DATA ----

# _ Mutate week numbers (and week labels) ----
df_telscrn_mi <- add_week_numbers(df_telscrn_mi)
df_telscrn_or <- add_week_numbers(df_telscrn_or)

# _ Mutate ineligibilty text field: ts_en_txt ----
# Based on `ts_en___*` dummy variables
df_telscrn_mi <- mutate_ts_en_txt_field(df_telscrn_mi)
df_telscrn_or <- mutate_ts_en_txt_field(df_telscrn_or)

# _ Mutate eligibility text fields to augment `ts_elg`: `ts_elg_txt` ----
df_telscrn_mi <- mutate_ts_elg_txt_field(df_telscrn_mi)
df_telscrn_or <- mutate_ts_elg_txt_field(df_telscrn_or)

# _ Factor-ize and order factors of `ts_elg_txt` ----
df_telscrn_mi$ts_elg_txt <- 
  factor(df_telscrn_mi$ts_elg_txt, levels = c('No', 'Not sure', 'Yes'))
df_telscrn_or$ts_elg_txt <- 
  factor(df_telscrn_or$ts_elg_txt, levels = c('No', 'Not sure', 'Yes'))

# _ Factor-ize and order factors of `ts_en_txt` ----
df_telscrn_mi$ts_en_txt <- 
  factor(df_telscrn_mi$ts_en_txt,
         levels = c('Social', 'Medical', 'Not Interested', 'Age', 'Other'))
df_telscrn_or$ts_en_txt <- 
  factor(df_telscrn_or$ts_en_txt,
         levels = c('Social', 'Medical', 'Not Interested', 'Age', 'Other'))


# **************************************** ----
# SUMMARIZE TELEPHONE SCREENING DATA ----

# _ Eligibility status summary tables ----

telscrn_elg_summ_mi <- df_telscrn_mi %>% 
  dplyr::group_by(ts_elg_txt) %>% 
  dplyr::summarize(n = dplyr::n())
telscrn_elg_summ_or <- df_telscrn_or %>% 
  dplyr::group_by(ts_elg_txt) %>% 
  dplyr::summarize(n = dplyr::n())

# _ Eligibility status summary by week tables ----

telscrn_elg_summ_week_mi <- df_telscrn_mi %>% 
  dplyr::filter(!is.na(ts_elg_txt)) %>% 
  dplyr::group_by(ts_dat_week_lab, ts_elg_txt) %>% 
  dplyr::summarize(n = dplyr::n())
telscrn_elg_summ_week_or <- df_telscrn_or %>% 
  dplyr::filter(!is.na(ts_elg_txt)) %>% 
  dplyr::group_by(ts_dat_week_lab, ts_elg_txt) %>% 
  dplyr::summarize(n = dplyr::n())

# _ Ineligibility reason summary tables ----

telscrn_en_summ_mi <- df_telscrn_mi %>% 
  dplyr::filter(ts_elg != 1) %>% 
  dplyr::group_by(ts_en_txt) %>% 
  dplyr::summarize(n = dplyr::n())
telscrn_en_summ_or <- df_telscrn_or %>% 
  dplyr::filter(ts_elg != 1) %>% 
  dplyr::group_by(ts_en_txt) %>% 
  dplyr::summarize(n = dplyr::n())

# _ Ineligibility reason summary by week tables ----

telscrn_en_summ_week_mi <- df_telscrn_mi %>% 
  dplyr::filter(!is.na(ts_en_txt)) %>% 
  dplyr::group_by(ts_dat_week_lab, ts_en_txt) %>% 
  dplyr::summarize(n = dplyr::n())
telscrn_en_summ_week_or <- df_telscrn_or %>% 
  dplyr::filter(!is.na(ts_en_txt)) %>% 
  dplyr::group_by(ts_dat_week_lab, ts_en_txt) %>% 
  dplyr::summarize(n = dplyr::n())


# **************************************** ----
# SAVE DATA TO RDS ----

# _ Eligibility status summary tables ----
saveRDS(telscrn_elg_summ_mi, 'rds/telscrn_elg_summ_mi.Rds')
saveRDS(telscrn_elg_summ_or, 'rds/telscrn_elg_summ_or.Rds')

# _ Eligibility status summary by week tables ----
saveRDS(telscrn_elg_summ_week_mi, 'rds/telscrn_elg_summ_week_mi.Rds')
saveRDS(telscrn_elg_summ_week_or, 'rds/telscrn_elg_summ_week_or.Rds')

# _ Ineligibility reason summary tables ----
saveRDS(telscrn_en_summ_mi, 'rds/telscrn_en_summ_mi.Rds')
saveRDS(telscrn_en_summ_or, 'rds/telscrn_en_summ_or.Rds')

# _ Ineligibility reason summary by week tables ----
saveRDS(telscrn_en_summ_week_mi, 'rds/telscrn_en_summ_week_mi.RDS')
saveRDS(telscrn_en_summ_week_or, 'rds/telscrn_en_summ_week_or.RDS')


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
