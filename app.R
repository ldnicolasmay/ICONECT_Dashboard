# app.R
#
# Shiny web app dashboard for I-CONECT
#

# **************************************** ----
# LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)


# **************************************** ----
# USEFUL GLOBALS ----

# _ Heights & widths ----
SIDEBAR_WIDTH <- 250
TABLE_WIDTH <- 5
PLOT_WIDTH <- 12 - TABLE_WIDTH
PLOT_HEIGHT_LEAD <- 450
PLOT_HEIGHT_SUMM <- 600
TS_DAT_START <- as.Date('2018-07-01')

# _ Site names
SITE_NAMES <- c('mi', 'or')

# _ Data Table (DT) options ----
DT_OPTIONS <- list(paging = FALSE,
                   searching = FALSE,
                   ordering = FALSE,
                   info = FALSE)

# _ Colors ----

# _ _ Recruitment Lead colors ----
cols_rec_lead_summ_bright_txt <-
  c('red', 'dodgerblue1', 'dodgerblue4', 'green3', 'gray30')
cols_rec_lead_summ_bright_srgb <-
  colorspace::sRGB( (t(col2rgb(cols_rec_lead_summ_bright_txt)) / 255) )
cols_rec_lead_summ_bright <- colorspace::hex( cols_rec_lead_summ_bright_srgb )
# Use when DT table selections interactively colorize pie slices
# cols_rec_lead_summ <-
#   colorspace::desaturate(
#     colorspace::hex( cols_rec_lead_summ_bright_srgb )
#   )
# Use when DT table selections do nothing
cols_rec_lead_summ <- cols_rec_lead_summ_bright

# _ _ Recruitment Status colors ----
cols_rec_stat_summ_bright_srgb <- colorspace::hex2RGB( rainbow(n = 17) )
cols_rec_stat_summ_bright <- colorspace::hex( cols_rec_stat_summ_bright_srgb )
# Use when DT table selections interactively colorize pie slices
cols_rec_stat_summ <-
  colorspace::desaturate(
    colorspace::hex( cols_rec_stat_summ_bright_srgb )
  )
# Use when DT table selections do nothing
# colors_rec_stat_summ <- cols_rec_stat_summ_bright

# _ _ Telephone Screening Eligibility colors ----
colors_telscrn_elg_bright_txt <- c('red3', 'orange', 'green3', 'gray')
colors_telscrn_elg_scrn_bright_srgb <-
  colorspace::sRGB( t(col2rgb(colors_telscrn_elg_bright_txt)) / 255 )
colors_telscrn_elg_scrn_bright <-
  colorspace::hex(colors_telscrn_elg_scrn_bright_srgb)
# Use when DT table selections interactively colorize pie slices
# colors_telscrn_elg <-
#   colorspace::desaturate(
#     colorspace::hex( colors_telscrn_elg_bright_srgb )
#   )
# Use when DT table selections do nothing
colors_telscrn_elg <- colors_telscrn_elg_scrn_bright

# _ _ Telephone Screening Ineligibility Reason colors ----
# colors_telscrn_en_bright_srgb <- colorspace::hex2RGB( rainbow(n = 6) )
colors_telscrn_en_bright_srgb <-
  colorspace::hex2RGB(c('#d53e4f', '#fc8d59', '#fee08b',
                        '#e6f598', '#99d594', '#3288bd'))
colors_telscrn_en_scrn_bright <-
  colorspace::hex( colors_telscrn_en_bright_srgb )
# Use when DT table selections interactively colorize pie slices
# colors_telscrn_en <-
#   colorspace::desaturate(
#     colorspace::hex( colors_telscrn_en_bright_srgb )
#   )
# Use when DT table selections do nothing
colors_telscrn_en <- colors_telscrn_en_scrn_bright

# **************************************** ----
# DEFINE UI ----
ui <- dashboardPage(
  
  # _ Header ----
  dashboardHeader(title = 'I-CONECT Dashboard', titleWidth = SIDEBAR_WIDTH),
  
  # _ Sidebar ----
  dashboardSidebar(
    
    # textOutput('res'),
    
    # _ _ Sidebar menu ----
    sidebarMenu(
      id = 'tabs',
      menuItem(text = 'Recruitment', tabName = 'recruitment',
               icon = icon('bullhorn')),
      menuItem(text = 'Screening', # tabName = 'screening',
               icon = icon('filter'),
               menuSubItem('Telephone Screening',
                           tabName = 'tele_screening'),
               menuSubItem('Home Screening',
                           tabName = 'home_screening')),
      menuItem(text = 'Blah 2', tabName = 'blah2',
               icon = icon('cube')),
      menuItem(text = 'Blah 3', tabName = 'blah3',
               icon = icon('wrench'))
    ),
    width = SIDEBAR_WIDTH
  ), # end dashboardSidebar
  
  # _ Body ----
  dashboardBody(
    
    # Set colors for font awesome icons
    # tags$style('.fa-ban {color:#333333}'),
    tags$style('.fa-ban {color:#4286f4}'),
    
    
    # Layout tabItems to correspond to sidebarMenu
    tabItems(
      tabItem(
        # _ _ Recruitment tab ----
        tabName = 'recruitment',
        h1('Recruitment'),
        fluidRow(
          tabBox(
            title = h3('Recruitment by Site'),
            id = 'recruitment',
            width = 12,
            tabPanel(
              # _ _ _ OHSU Recruitment tab panel ----
              title = h4('OHSU'),
              fluidRow(
                box( h3('Recruitment Leads'), width = 12 ),
                box( dataTableOutput('recruit_lead_summ_or_tbl'),
                     hr(),
                     tags$b('Warm leads:'),
                     tags$p('PCN, PCF, PEG, PTS, PHS, SOT'),
                     tags$b('Cold leads:'),
                     tags$p('OIS'),
                     tags$b('Dead leads:'),
                     tags$p('UNR, NNO, NE, NIN, TSF, ROT, RTS, DEC, ENR'),
                     width = TABLE_WIDTH ),
                box( plotOutput('recruit_lead_summ_or_plot',
                                click = 'plot_click'),
                     width = PLOT_WIDTH,
                     height = PLOT_HEIGHT_LEAD + 25 )
              ),
              fluidRow(
                box( h3('Recruitment Statuses'), width = 12),
                box( dataTableOutput('recruit_status_summ_or_tbl'),
                     width = TABLE_WIDTH),
                box( plotOutput('recruit_status_summ_or_plot'),
                     width = PLOT_WIDTH,
                     height = PLOT_HEIGHT_SUMM + 25 )
              )
            ), # END OHSU Recruitment tab panel
            tabPanel(
              # _ _ _ UM Recruitment tab panel ----
              title = h4('UMich'),
              fluidRow(
                box( h3('Recruitment Leads'), width = 12 ),
                box( dataTableOutput('recruit_lead_summ_mi_tbl'),
                     hr(),
                     tags$b('Warm leads:'),
                     tags$p('PCN, PCF, PEG, PTS, PHS, SOT'),
                     tags$b('Cold leads:'),
                     tags$p('OIS'),
                     tags$b('Dead leads:'),
                     tags$p('UNR, NNO, NE, NIN, TSF, ROT, RTS, DEC'),
                     width = TABLE_WIDTH),
                box( plotOutput('recruit_lead_summ_mi_plot'),
                     width = PLOT_WIDTH,
                     height = PLOT_HEIGHT_LEAD + 25 )
              ),
              fluidRow(
                box( h3('Recruitment Statuses'), width = 12),
                box( dataTableOutput('recruit_status_summ_mi_tbl'),
                     width = TABLE_WIDTH ),
                box( plotOutput('recruit_status_summ_mi_plot'),
                     width = PLOT_WIDTH,
                     height = PLOT_HEIGHT_SUMM + 25 )
              )
            ) # -- END UM Recruitment tabPanel
          ) # -- END tabBox
        ) # -- END fluidRow
      ), # -- END Recruitment tabItem
      # _ _ Telephone Screening tab ----
      tabItem(
        tabName = 'tele_screening',
        h1('Telephone Screening'),
        fluidRow(
          tabBox(
            width = 12,
            tabPanel(
              title = h4('OHSU'),
              fluidRow(
                box( h3('Telephone Screening Eligibility'),
                     width = 12),
                box( dataTableOutput('telscrn_elg_summ_or_tbl'),
                     width = TABLE_WIDTH),
                box( plotOutput('telscrn_elg_summ_or_plot'),
                     width = PLOT_WIDTH,
                     height = PLOT_HEIGHT_LEAD + 25)
              ),
              fluidRow(
                box( h3('Telephone Screening Eligibility by Week'),
                     width = 12 ),
                box( plotOutput('telscrn_elg_summ_week_or_plot'),
                     width = 12,
                     height = PLOT_HEIGHT_LEAD + 25)
              ),
              fluidRow(
                box( h3('Telephone Screening Ineligibility Reason'),
                     width = 12),
                box( dataTableOutput('telscrn_en_summ_or_tbl'),
                     width = TABLE_WIDTH),
                box( plotOutput('telscrn_en_summ_or_plot'),
                     width = PLOT_WIDTH,
                     height = PLOT_HEIGHT_LEAD + 25)
              ),
              fluidRow(
                box( h3('Telephone Screening Ineligibility Reason by Week'),
                     width = 12),
                box( plotOutput('telscrn_en_summ_week_or_plot'),
                     width = 12,
                     height = PLOT_HEIGHT_LEAD + 25)
              )
            ), # -- END OHSU Telephone Screening tab panel
            tabPanel(
              title = h4('UMich'),
              fluidRow(
                box( h3('Telephone Screening Eligibility'),
                     width = 12),
                box( dataTableOutput('telscrn_elg_summ_mi_tbl'),
                     width = TABLE_WIDTH),
                box( plotOutput('telscrn_elg_summ_mi_plot'),
                     width = PLOT_WIDTH,
                     height = PLOT_HEIGHT_LEAD + 25)
              ),
              fluidRow(
                box( h3('Telephone Screening Eligibility by Week'),
                     width = 12 ),
                box( plotOutput('telscrn_elg_summ_week_mi_plot'),
                     width = 12,
                     height = PLOT_HEIGHT_LEAD + 25)
              ),
              fluidRow(
                box( h3('Telephone Screening Ineligibility Reason'),
                     width = 12),
                box( dataTableOutput('telscrn_en_summ_mi_tbl'),
                     width = TABLE_WIDTH),
                box( plotOutput('telscrn_en_summ_mi_plot'),
                     width = PLOT_WIDTH,
                     height = PLOT_HEIGHT_LEAD + 25)
              ),
              fluidRow(
                box( h3('Telephone Screening Ineligibility Reason by Week'),
                     width = 12),
                box( plotOutput('telscrn_en_summ_week_mi_plot'),
                     width = 12,
                     height = PLOT_HEIGHT_LEAD + 25)
              )
            ) # -- END UM Telephone Screening tabPanel
          ) # -- END tabBox
        ) # -- END fluidRow
      ), # -- END Telephone Screening tabItem
      # _ _ Home Screening tab ----
      tabItem(
        tabName = 'home_screening',
        h1('Home Screening')
      ), # -- END Home Screening tabItem
      # _ _ Blah 2 tab ----
      tabItem(
        tabName = 'blah2',
        h1('Blah 2')
      ), # -- END Blah 2 tabItem
      # _ _ Blah 3 tab ----
      tabItem(
        tabName = 'blah3',
        h1('Blah 3')
      ) # -- END Blah 3 tabItem
      
    ) # -- END tabItems
    
  ) # -- END dashboardBody
  
) # -- END dashboardPage


# **************************************** ----
# DEFINE SERVER LOGIC ----
server <- function(input, output, session) {
  
  # _ Load Data ----
  
  # _ _ Load recruitment status summary data ----
  recruit_status_summ_list <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = 'rds/recruit_status_summ.Rds',
    session = NULL,
    readFunc = readRDS
  )
  
  # _ _ Load recruitment lead summary data ----
  recruit_lead_summ_list <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = 'rds/recruit_lead_summ.Rds',
    session = NULL,
    readFunc = readRDS
  )
  
  # _ _ Load telscrn eligibility summary data ----
  telscrn_elg_summ_list <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = 'rds/telscrn_elg_summ.Rds',
    session = NULL,
    readFunc = readRDS
  )
  
  # _ _ Load telscrn eligibility summary by week data ----
  telscrn_elg_summ_week_list <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = 'rds/telscrn_elg_summ_week.Rds',
    session = NULL,
    readFunc = readRDS
  )
  
  # _ _ Load telscrn ineligibility reason summary data ----
  telscrn_en_summ_list <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = 'rds/telscrn_en_summ.Rds',
    session = NULL,
    readFunc = readRDS
  )
  
  # _ _ Load telscrn ineligibility reason summary by week data ----
  telscrn_en_summ_week_list <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = 'rds/telscrn_en_summ_week.Rds',
    session = NULL,
    readFunc = readRDS
  )
  
  # _ Render Tables ----
  
  # _ _ Recruitment tables ----
  
  # _ _ _ Recruitment Status tables ----
  # Using `observe({ lapply(...) })` fxnl prog approach allows for extensiblity
  observe({
    lapply(
      SITE_NAMES, # SITE_NAMES <- c('mi', 'or')
      function(site_name) {
        # Below line emulates: output$recruit_status_summ_mi_tbl <-
        output[[paste0('recruit_status_summ_', site_name, '_tbl')]] <-
          renderDataTable({
            recruit_status_summ_list()[[site_name]] %>%
              select(recruit_stat_txt,
                     recruit_stat_long,
                     n,
                     proportion) %>%
              rename(`Recruitment Status Code` = recruit_stat_txt,
                     `Recruitment Status Text` = recruit_stat_long,
                     `Prop` = proportion) %>%
              datatable(rownames = FALSE, options = DT_OPTIONS) %>%
              formatStyle( # DT package
                columns = 'Recruitment Status Code',
                target = 'row',
                backgroundColor = styleEqual(c('TOTAL'), c('#ECF0F5')),
                fontWeight = styleEqual(c('TOTAL'), c('bold'))
              )
          })
      })
  })
  
  # _ _ _ Recruitment Lead tables ----
  # Using `observe({ lapply(...) })` fxnl prog approach allows for extensiblity
  observe({
    lapply(
      SITE_NAMES, # SITE_NAMES <- c('mi', 'or')
      function(site_name) {
        # Below line emulates: output$recruit_lead_summ_mi <-
        output[[paste0('recruit_lead_summ_', site_name, '_tbl')]] <-
          renderDataTable({
            recruit_lead_summ_list()[[site_name]] %>%
              select(lead_categ_txt, n, proportion) %>%
              rename(`Lead Category` = lead_categ_txt,
                     `Prop` = proportion) %>%
              datatable(rownames = FALSE, options = DT_OPTIONS) %>%
              formatStyle( # DT package
                columns = 'Lead Category',
                target = 'row',
                backgroundColor = styleEqual(c('TOTAL'), c('#ECF0F5')),
                fontWeight = styleEqual(c('TOTAL'), c('bold'))
              )
          })
      })
  })
  
  # _ _ Telephone Screening tables ----
  
  # _ _ _ Eligibility tables ----
  # Using `observe({ lapply(...) })` fxnl prog approach allows for extensiblity
  observe({
    lapply(
      SITE_NAMES, # SITE_NAMES <- c('mi', 'or')
      function(site_name) {
        # Below line emulates: output$telscrn_elg_summ_mi_tbl <-
        output[[paste0('telscrn_elg_summ_', site_name, '_tbl')]] <-
          renderDataTable({
            telscrn_elg_summ_list()[[site_name]] %>%
              rename(`Eligibilty Status` = ts_elg_txt,
                     `Prop` = proportion) %>%
              datatable(rownames = FALSE, options = DT_OPTIONS) %>%
              formatStyle( # DT package
                columns = 'Eligibilty Status',
                target = 'row',
                backgroundColor = styleEqual(c('TOTAL'), c('#ECF0F5')),
                fontWeight = styleEqual(c('TOTAL'), c('bold'))
              )
          })
      })
  })
  
  # _ _ _ Ineligibility tables ----
  # Using `observe({ lapply(...) })` fxnl prog approach allows for extensiblity
  observe({
    lapply(
      SITE_NAMES, # SITE_NAMES <- c('mi', 'or')
      function(site_name) {
        # Below line emulates: output$telscrn_en_summ_mi <-
        output[[paste0('telscrn_en_summ_', site_name, '_tbl')]] <-
          renderDataTable({
            telscrn_en_summ_list()[[site_name]] %>%
              rename(`Ineligibility Reason` = ts_en_txt,
                     `Prop` = proportion) %>%
              datatable(rownames = FALSE, options = DT_OPTIONS) %>%
              formatStyle( # DT package
                columns = 'Ineligibility Reason',
                target = 'row',
                backgroundColor = styleEqual(c('TOTAL'), c('#ECF0F5')),
                fontWeight = styleEqual(c('TOTAL'), c('bold'))
              )
          })
      })
  })
  
  # _ Render Plots ----
  
  # _ _ Recruitment Plots ----
  
  # _ _ _ Recruitment Lead plots ----
  # Using `observe({ lapply(...) })` fxnl prog approach allows for extensiblity
  observe({
    lapply(
      SITE_NAMES, # SITE_NAMES <- c('mi', 'or')
      function(site_name) {
        # # `selex` used for interactive selection from table to pie plot
        # selex = input[[paste0('recruit_lead_summ_',
        #                       site_name, '_tbl_rows_selected')]]
        # if (length(selex)) {
        #   cols_rec_lead_summ =
        #     replace(cols_rec_lead_summ, 
        #             selex, 
        #             cols_rec_lead_summ_bright[selex])
        # }
        recruit_lead_summ_sn = recruit_lead_summ_list()[[site_name]] %>%
          dplyr::filter(lead_categ_txt != 'TOTAL')
        # Below line emulates: output$recruit_lead_summ_mi_plot <-
        output[[paste0('recruit_lead_summ_', site_name, '_plot')]] <-
          renderPlot({
            pie(x = recruit_lead_summ_sn$n,
                labels = recruit_lead_summ_sn$lead_categ_txt,
                col = cols_rec_lead_summ,
                main = 'Lead Categories',
                radius = 1)
          }, height = PLOT_HEIGHT_LEAD)
      })
  })
  
  # _ _ _ Recruitment Status plots ----
  # Using `observe({ lapply(...) })` fxnl prog approach allows for extensiblity
  observe({
    lapply(
      SITE_NAMES, # SITE_NAMES <- c('mi', 'or')
      function(site_name) {
        # `selex` used for interactive selection from table to pie plot
        selex = input[[paste0('recruit_status_summ_', 
                              site_name, '_tbl_rows_selected')]]
        if (length(selex)) {
          cols_rec_stat_summ =
            replace(cols_rec_stat_summ, 
                    selex, 
                    cols_rec_stat_summ_bright[selex])
        }
        recruit_status_summ_sn = 
          recruit_status_summ_list()[[site_name]] %>% 
          dplyr::filter(recruit_stat_txt != 'TOTAL')
        # Below line emulates: output$recruit_status_summ_mi_plot <-
        output[[paste0('recruit_status_summ_', site_name, '_plot')]] <-
          renderPlot({
            pie(x = recruit_status_summ_sn$n,
                labels = recruit_status_summ_sn$recruit_stat_txt,
                col = cols_rec_stat_summ,
                main = 'Recruitment Statuses',
                radius = 1)
          }, height = PLOT_HEIGHT_SUMM)
      })
  })
  
  # _ _ Telephone Screening plots ----
  
  # _ _ _ Eligibility Status plots ----
  # Using `observe({ lapply(...) })` fxnl prog approach allows for extensiblity
  observe({
    lapply(
      SITE_NAMES, # SITE_NAMES <- c('mi', 'or')
      function(site_name) {
        telscrn_elg_summ_sn = telscrn_elg_summ_list()[[site_name]] %>% 
          dplyr::filter(ts_elg_txt != 'TOTAL')
        # Below line emulates: output$telscrn_elg_summ_mi_plot <-
        output[[paste0('telscrn_elg_summ_', site_name, '_plot')]] <- 
          renderPlot({
            pie(x = telscrn_elg_summ_sn$n,
                labels = paste0(telscrn_elg_summ_sn$ts_elg_txt, ', ',
                                telscrn_elg_summ_sn$n),
                col = colors_telscrn_elg,
                main = 'Telephone Screening Eligibility',
                radius = 1)
          }, height = PLOT_HEIGHT_LEAD)
      })
  })
  
  # _ _ _ Weekly Eligibility Status plots ----
  # Using `observe({ lapply(...) })` fxnl prog approach allows for extensiblity
  observe({
    lapply(
      SITE_NAMES,
      function(site_name) {
        # Below line emulates: output$telscrn_elg_summ_week_mi_plot <-
        output[[paste0('telscrn_elg_summ_week_', site_name, '_plot')]] <-
          renderPlot({
            ggplot(data = telscrn_elg_summ_week_list()[[site_name]],
                   aes(x = ts_dat_week_lab, y = n, fill = ts_elg_txt)) +
              geom_bar(stat = 'identity') +
              scale_x_date(breaks = seq(TS_DAT_START, Sys.Date(), by = 7),
                           limits = c(TS_DAT_START-4, Sys.Date())) +
              theme(axis.text.x = 
                      element_text(angle = 45, vjust = 1, hjust = 1)) +
              labs(x = 'Week of Telephone Screening',
                   y = 'Count',
                   fill = 'Eligibility Category') +
              scale_fill_manual(values = colors_telscrn_elg) +
              theme(legend.position = 'bottom')
          }, height = PLOT_HEIGHT_LEAD)
      })
  })
  
  # _ _ _ Ineligibility Reason plots ----
  # Using `observe({ lapply(...) })` fxnl prog approach allows for extensiblity
  observe({
    lapply(
      SITE_NAMES, # SITE_NAMES <- c('mi', 'or')
      function(site_name) {
        telscrn_en_summ_sn = telscrn_en_summ_list()[[site_name]] %>% 
          dplyr::filter(ts_en_txt != 'TOTAL')
        # Below line emulates: output$telscrn_en_summ_mi_plot <-
        output[[paste0('telscrn_en_summ_', site_name, '_plot')]] <-
          renderPlot({
            pie(x = telscrn_en_summ_sn$n,
                labels = paste0(telscrn_en_summ_sn$ts_en_txt, ', ',
                                telscrn_en_summ_sn$n),
                col = colors_telscrn_en,
                main = 'Telephone Screening Ineligibility Reason',
                radius = 1)
          }, height = PLOT_HEIGHT_LEAD)
      })
  })
  
  # _ _ _ Weekly Ineligibility Reason plots ----
  # Using `observe({ lapply(...) })` fxnl prog approach allows for extensiblity
  observe({
    lapply(
      SITE_NAMES,
      function(site_name) {
        # Below line emulates: output$telscrn_en_summ_week_mi_plot <- 
        output[[paste0('telscrn_en_summ_week_', site_name, '_plot')]] <-
          renderPlot({
            ggplot(data = telscrn_en_summ_week_list()[[site_name]],
                   aes(x = ts_dat_week_lab, y = n, fill = ts_en_txt)) +
              geom_bar(stat = 'identity') +
              scale_x_date(breaks = seq(TS_DAT_START, Sys.Date(), by = 7),
                           limits = c(TS_DAT_START-4, Sys.Date())) +
              theme(axis.text.x = 
                      element_text(angle = 45, vjust = 1, hjust = 1)) +
              labs(x = 'Week of Telephone Screening',
                   y = 'Count',
                   fill = 'Ineligibility Reason Category') +
              scale_fill_manual(values = colors_telscrn_en) +
              theme(legend.position = 'bottom')
          }, height = PLOT_HEIGHT_LEAD)
      })
  })
  
} # -- END server

# **************************************** ----
# RUN THE APP ----
shinyApp(ui = ui, server = server)


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
