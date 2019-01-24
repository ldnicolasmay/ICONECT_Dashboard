#!/usr/bin/env Rscript

# process_recruit_data.R


# **************************************** ----
# USEFUL LIBRARIES ----
library(dplyr)
library(purrr)


# **************************************** ----
# VARIABLES & HELPER FUNCTIONS ----
source('config.R')
source('helper_fxns_recruit.R')


# **************************************** ----
# GET RECRUITMENT DATA ----

# Temporary data source: XLSX files
# _ Load XLSX ----

# UM / _mi
recruit_data_mi <- readxl::read_xlsx(
  path = 'Participant Log--UM.xlsx',
  sheet = 'All_Data',
  range = 'A1:H3001',
  col_types = 'text'
) %>% na_if('?') %>% na_if('')

# OHSU / _or
recruit_data_or <- readxl::read_xlsx(
  path = 'Participant Log--OHSU.xlsx',
  sheet = 'All_Data',
  range = 'A1:H3001',
  col_types = 'text'
) %>% na_if('?') %>% na_if('')
recruit_data_or_dead <- readxl::read_xlsx(
  path = 'ARCHIVE DEAD LEAD Participant Log--OHSU.xlsx',
  sheet = 'All_Data',
  range = 'A1:H3001',
  col_types = 'text'
) %>% na_if('?') %>% na_if('')
recruit_data_or <- bind_rows(recruit_data_or, recruit_data_or_dead)

# _ Put recruit_data_mi, recruit_data_or dfs in a named list ----
recruit_data <- list(mi = recruit_data_mi, 
                     or = recruit_data_or)

# _ Remove unneeded dfs ----
rm(recruit_data_mi, recruit_data_or)


# **************************************** ----
# CLEAN RECRUITMENT DATA ----

# _ Remove all NA rows ----
recruit_data <- map(recruit_data, remove_na_rows)

# _ Cast `Date to Call`, `Date of Initial Contact` as Date ----
recruit_data <- map(recruit_data, cast_date_cols)

# _ Mutate `Not Elig. *` fields ----
# TRUE => 1, [blank] => 0
recruit_data <- map(recruit_data, binarize_notelig_fields)

# _ Capitalize all `Recruitment Status` values ----
recruit_data <- map(recruit_data,
                    ~ .x %>% 
                      mutate(`Recruitment Status` = 
                               toupper(`Recruitment Status`)))


# **************************************** ----
# RECRUITMENT STATUS SUMMARY TABLES ----

# _ Create recruitment status summary tables ----
recruit_status_summ <- map(recruit_data, create_recruit_status_summ)

# _ Mutate `recruit_stat_long` field ----
# Recruitment status long text categories
recruit_status_summ <- map(recruit_status_summ, mutate_recruit_stat_long)

# _ Mutate `recruit_stat` field ----
# Recruitment status integer categories
recruit_status_summ <- map(recruit_status_summ, mutate_recruit_stat)


# **************************************** ----
# RECRUITMENT LEAD SUMMARY TABLES ----

# _ Create recruitment lead summary tables ----
recruit_lead_summ <- map(recruit_status_summ, create_recruit_lead_summ)


# **************************************** ----
# RECRUITMENT STATUS + LEAD SUMMARY TABLES ----

# _ Insert "NA" string to NA row ----
recruit_status_summ <- map(recruit_status_summ, recruit_status_insert_na_string)
recruit_lead_summ <- map(recruit_lead_summ, recruit_lead_insert_na_string)

# _ Mutate proportion column ----
recruit_status_summ <- map(recruit_status_summ, rs_add_proportion_column)
recruit_lead_summ <- map(recruit_lead_summ, rl_add_proportion_column)

# _ Add totals row ----
recruit_status_summ <- map(recruit_status_summ, rs_add_total_row)
recruit_lead_summ <- map(recruit_lead_summ, rl_add_total_row)


# **************************************** ----
# SAVE DATA TO RDS ----

# Recruitment status summary tables ----
saveRDS(recruit_status_summ, 'rds/recruit_status_summ.Rds')

# Recruitment lead summary tables ----
saveRDS(recruit_lead_summ, 'rds/recruit_lead_summ.Rds')


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
