# app.R
#
# Shiny web app dashboard for I-CONECT
#

# **** ----
# LOAD LIBRARIES ----
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
# library(DT)


# **** ----
# USEFUL GLOBALS ----

# _ Heights & widths ----
SIDEBAR_WIDTH <- 250
TABLE_WIDTH <- 5
PLOT_WIDTH <- 12 - TABLE_WIDTH
PLOT_HEIGHT_LEAD <- 450
PLOT_HEIGHT_SUMM <- 600
TS_DAT_START <- as.Date('2018-07-01')

# _ Data Table (DT) options ----
DT_OPTIONS <- list(paging = FALSE,
                   searching = FALSE,
                   ordering = FALSE,
                   info = FALSE)

# _ Source helper functions ----
# source('recruit_helper_fxns.R')

# _ Colors ----

# dim_factor <- 0.9

# _ _ Recruitment Lead colors ----
colors_lead_summ_bright_txt <- 
  c("red", "dodgerblue1", "lightsteelblue2", "green3", "gray30")
colors_lead_summ_bright_srgb <- 
  colorspace::sRGB( (t(col2rgb(colors_lead_summ_bright_txt)) / 255) )
colors_lead_summ_bright <- colorspace::hex( colors_lead_summ_bright_srgb )
# Use when DT table selections interactively colorize pie slices
# colors_lead_summ <-
#   colorspace::desaturate(
#     colorspace::hex( colors_lead_summ_bright_srgb )
#   )
# Use when DT table selections do nothing
colors_lead_summ <- colors_lead_summ_bright

# _ _ Recruitment Summary colors ----
colors_summ_bright_srgb <- colorspace::hex2RGB( rainbow(n = 17) )
colors_summ_bright <- colorspace::hex( colors_summ_bright_srgb )
colors_summ <-
  colorspace::desaturate(
    colorspace::hex( colors_summ_bright_srgb )
  )

# _ _ Telephone Screening Eligibility colors ----
colors_telescrn_elg_bright_txt <- c('red2', 'orange', 'green3', 'gray')
colors_telescrn_elg_scrn_bright_srgb <- 
  colorspace::sRGB( t(col2rgb(colors_telescrn_elg_bright_txt)) / 255 )
colors_telescrn_elg_scrn_bright <- 
  colorspace::hex(colors_telescrn_elg_scrn_bright_srgb)
# Use when DT table selections interactively colorize pie slices
# colors_telescrn_elg <-
#   colorspace::desaturate(
#     colorspace::hex( colors_telescrn_elg_bright_srgb )
#   )
# Use when DT table selections do nothing
colors_telescrn_elg <- colors_telescrn_elg_scrn_bright

# _ _ Telephone Screening Ineligibility Reason colors ---- 
colors_telescrn_en_bright_srgb <- colorspace::hex2RGB( rainbow(n = 6) )
colors_telescrn_en_scrn_bright <- 
  colorspace::hex( colors_telescrn_en_bright_srgb )
# Use when DT table selections interactively colorize pie slices
# colors_telescrn_en <-
#   colorspace::desaturate(
#     colorspace::hex( colors_telescrn_en_bright_srgb )
#   )
# Use when DT table selections do nothing
colors_telescrn_en <- colors_telescrn_en_scrn_bright

# **** ----
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
                box( DT::dataTableOutput('recruit_lead_summ_or_tbl'),
                     hr(),
                     tags$b("Warm leads:"),
                     tags$p("PCN, PCF, PEG, PTS, PHS, SOT"),
                     tags$b("Cold leads:"),
                     tags$p("OIS"),
                     tags$b("Dead leads:"),
                     tags$p("UNR, NNO, NE, NIN, TSF, ROT, RTS, DEC, ENR"),
                     width = TABLE_WIDTH ),
                box( plotOutput('recruit_lead_summ_or_plot',
                                click = 'plot_click'),
                     width = PLOT_WIDTH,
                     height = PLOT_HEIGHT_LEAD + 25 ) #,
                # box( verbatimTextOutput('x2'), width = 12 ),
                # box( verbatimTextOutput('x3'), width = 12),
                # box( verbatimTextOutput('x4'), width = 12),
                # box( DT::dataTableOutput('x5'), 
                #      width = 12, height = 250),
                # box( verbatimTextOutput('x6'), width = 12)
              ),
              fluidRow(
                box( h3('Recruitment Statuses'), width = 12),
                box( DT::dataTableOutput('recruit_status_summ_or_tbl'),
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
                box( DT::dataTableOutput('recruit_lead_summ_mi_tbl'),
                     hr(),
                     tags$b("Warm leads:"),
                     tags$p("PCN, PCF, PEG, PTS, PHS, SOT"),
                     tags$b("Cold leads:"),
                     tags$p("OIS"),
                     tags$b("Dead leads:"),
                     tags$p("UNR, NNO, NE, NIN, TSF, ROT, RTS, DEC"),
                     width = TABLE_WIDTH),
                box( plotOutput('recruit_lead_summ_mi_plot'), 
                     width = PLOT_WIDTH, 
                     height = PLOT_HEIGHT_LEAD + 25 )
              ),
              fluidRow(
                box( h3('Recruitment Statuses'), width = 12),
                box( DT::dataTableOutput('recruit_status_summ_mi_tbl'),
                     width = TABLE_WIDTH ),
                box( plotOutput('recruit_status_summ_mi_plot'),
                     width = PLOT_WIDTH,
                     height = PLOT_HEIGHT_SUMM + 25 )
              )
            ) # -- END UM Recruitment tab panel
          )
        )
      ),
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
                box( DT::dataTableOutput('telscrn_elg_summ_or_tbl'),
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
                box( DT::dataTableOutput('telscrn_en_summ_or_tbl'),
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
                box( DT::dataTableOutput('telscrn_elg_summ_mi_tbl'),
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
                box( DT::dataTableOutput('telscrn_en_summ_mi_tbl'),
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
            ) # -- END UM Telephone Screening tab panel
          )
        )
      ),
      # _ _ Home Screening tab ----
      tabItem(
        tabName = 'home_screening',
        h1('Home Screening')
      ),
      tabItem(
        tabName = 'blah2',
        h1('Blah 2')
      ),
      tabItem(
        tabName = 'blah3',
        h1('Blah 3')
      )
    ) # end tabItems
    
  ) # end dashboardBody
  
)

# **** ----
# DEFINE SERVER LOGIC ----
server <- function(input, output, session) {
  
  # _ Load Data ----
  
  # _ _ Load recruitment status summary data ----
  recruit_status_summ_or <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = './rds/recruit_status_summ_or.Rds',
    session = NULL,
    readFunc = readRDS
  )
  recruit_status_summ_mi <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = './rds/recruit_status_summ_mi.Rds',
    session = NULL,
    readFunc = readRDS
  )
  
  # _ _ Load recruitment lead summary data ----
  recruit_lead_summ_mi <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = './rds/recruit_lead_summ_mi.Rds', 
    session = NULL,
    readFunc = readRDS
  )
  recruit_lead_summ_or <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = './rds/recruit_lead_summ_or.Rds', 
    session = NULL,
    readFunc = readRDS
  )
  
  # # _ _ Load raw telephone screening data ----
  # telscrn <- reactiveFileReader(
  #   intervalMillis = 1000 * 60 * 60, # 1 hour
  #   filePath = './rds/telscrn.Rds',
  #   session = NULL,
  #   readFunc = readRDS
  # )
  
  # _ _ Load telscrn eligibility summary data ----
  telscrn_elg_summ_mi <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = './rds/telscrn_elg_summ_mi.Rds',
    session = NULL,
    readFunc = readRDS
  )
  telscrn_elg_summ_or <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = './rds/telscrn_elg_summ_or.Rds',
    session = NULL,
    readFunc = readRDS
  )
  
  # _ _ Load telscrn eligibility summary by week data ----
  telscrn_elg_summ_week_mi <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = './rds/telscrn_elg_summ_week_mi.Rds',
    session = NULL,
    readFunc = readRDS
  )
  telscrn_elg_summ_week_or <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = './rds/telscrn_elg_summ_week_or.Rds',
    session = NULL,
    readFunc = readRDS
  )
  
  # _ _ Load telscrn ineligibility reason summary data ----
  telscrn_en_summ_mi <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = './rds/telscrn_en_summ_mi.Rds',
    session = NULL,
    readFunc = readRDS
  )
  telscrn_en_summ_or <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = './rds/telscrn_en_summ_or.Rds',
    session = NULL,
    readFunc = readRDS
  )
  
  # _ _ Load telscrn ineligibility reason summary by week data ----
  telscrn_en_summ_week_mi <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = './rds/telscrn_en_summ_week_mi.RDS',
    session = NULL,
    readFunc = readRDS
  )
  telscrn_en_summ_week_or <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60, # 1 hour
    filePath = './rds/telscrn_en_summ_week_or.RDS',
    session = NULL,
    readFunc = readRDS
  )
  
  # _ _ Load UM recruitment lead summary data ----
  
  # _ Render Tables ----
  
  # _ _ Recruitment tables ----
  
  # # input_plot_click <- reactive(input$plot_click)
  # recruit_lead_summ_or_tbl_row_selex <- reactive({
  #   temp <- input$recruit_lead_summ_or_tbl_rows_selected # c(3, 1)
  #   # temp <- ifelse(!is.null(input_plot_click()),
  #   #               temp, NULL)
  #   return(temp)
  # })
  
  output$recruit_status_summ_mi_tbl <- DT::renderDataTable(
    # recruit_status_summ_mi %>% 
    #   select(recruit_stat_txt, n) %>% 
    #   rename(`Recruitment Status` = recruit_stat_txt)
    rename(
      select(
        recruit_status_summ_mi(),
        recruit_stat_txt, recruit_stat_long, n, proportion
      ),
      `Recruitment Status Code` = recruit_stat_txt,
      `Recruitment Status Text` = recruit_stat_long,
      `Prop` = proportion
    ),
    rownames = FALSE,
    options = DT_OPTIONS
  )
  
  output$recruit_status_summ_or_tbl <- DT::renderDataTable(
    rename(
      .data = select(
        .data = recruit_status_summ_or(),
        recruit_stat_txt, recruit_stat_long, n, proportion
      ),
      `Recruitment Status Code` = recruit_stat_txt,
      `Recruitment Status Text` = recruit_stat_long,
      `Prop` = proportion
    ),
    rownames = FALSE,
    options = DT_OPTIONS
  )
  
  output$recruit_lead_summ_mi_tbl <- DT::renderDataTable(
    rename(
      .data = select(
        .data = recruit_lead_summ_mi(),
        lead_categ_txt, n, proportion
      ),
      `Lead Category` = lead_categ_txt,
      `Prop` = proportion
    ),
    rownames = FALSE,
    options = DT_OPTIONS
  )
  
  output$recruit_lead_summ_or_tbl <- DT::renderDataTable(
    rename(
      .data = select(
        .data = recruit_lead_summ_or(),
        lead_categ_txt, n, proportion
      ),
      `Lead Category` = lead_categ_txt,
      `Prop` = proportion
    ),
    rownames = FALSE,
    options = DT_OPTIONS
  )
  
  # _ _ Telephone Screening tables ----
  
  # _ _ _ Eligibility tables ----
  output$telscrn_elg_summ_or_tbl <- DT::renderDataTable(
    telscrn_elg_summ_or(),
    rownames = FALSE,
    options = DT_OPTIONS
  )
  
  output$telscrn_elg_summ_mi_tbl <- DT::renderDataTable(
    telscrn_elg_summ_mi(),
    rownames = FALSE,
    options = DT_OPTIONS
  )
  
  # _ _ _ Ineligibility tables ----
  output$telscrn_en_summ_or_tbl <- DT::renderDataTable(
    telscrn_en_summ_or(),
    rownames = FALSE,
    options = DT_OPTIONS
  )
  
  output$telscrn_en_summ_mi_tbl <- DT::renderDataTable(
    telscrn_en_summ_mi(),
    rownames = FALSE,
    options = DT_OPTIONS
  )
  
  # _ Render Plots ----
  
  # _ _ Recruitment Plots ----
  
  # _ _ _ Recruitment Lead plots ----
  output$recruit_lead_summ_or_plot <- renderPlot({
    # selex = input$recruit_lead_summ_or_tbl_rows_selected # sel'd DT rows
    # # selex = recruit_lead_summ_or_tbl_row_selex()
    # if (length(selex)) {
    #   colors_lead_summ = 
    #     replace(colors_lead_summ, selex, colors_lead_summ_bright[selex])
    # }
    recruit_lead_summ_or = recruit_lead_summ_or() %>% 
      dplyr::filter(lead_categ_txt != 'TOTAL')
    pie(x = recruit_lead_summ_or$n, 
        labels = recruit_lead_summ_or$lead_categ_txt,
        col = colors_lead_summ, 
        main = "Lead Categories",
        radius = 1)
  }, height = PLOT_HEIGHT_LEAD)
  
  output$recruit_lead_summ_mi_plot <- renderPlot({
    # selex = input$recruit_lead_summ_mi_tbl_rows_selected # sel'd DT rows
    # if (length(selex)) {
    #   colors_lead_summ =
    #     replace(colors_lead_summ, selex, colors_lead_summ_bright[selex])
    # }
    recruit_lead_summ_mi = recruit_lead_summ_mi() %>% 
      dplyr::filter(lead_categ_txt != 'TOTAL')
    pie(x = recruit_lead_summ_mi$n, 
        labels = recruit_lead_summ_mi$lead_categ_txt,
        col = colors_lead_summ, 
        main = "Lead Categories",
        radius = 1)
  }, height = PLOT_HEIGHT_LEAD)
  
  # _ _ _ Recruitment Status plots ----
  output$recruit_status_summ_or_plot <- renderPlot({
    selex = input$recruit_status_summ_or_tbl_rows_selected # sel'd DT rows
    if (length(selex)) {
      colors_summ =
        replace(colors_summ, selex, colors_summ_bright[selex])
    }
    recruit_status_summ_or = recruit_status_summ_or() %>% 
      dplyr::filter(recruit_stat_txt != 'TOTAL')
    pie(x = recruit_status_summ_or$n,
        labels = recruit_status_summ_or$recruit_stat_txt,
        col = colors_summ,
        main = "Recruitment Statuses",
        radius = 1)
  }, height = PLOT_HEIGHT_SUMM)
  
  output$recruit_status_summ_mi_plot <- renderPlot({
    selex = input$recruit_status_summ_mi_tbl_rows_selected # sel'd DT rows
    if (length(selex)) {
      colors_summ =
        replace(colors_summ, selex, colors_summ_bright[selex])
    }
    recruit_status_summ_mi = recruit_status_summ_mi() %>% 
      dplyr::filter(recruit_stat_txt != 'TOTAL')
    pie(x = recruit_status_summ_mi$n,
        labels = recruit_status_summ_mi$recruit_stat_txt,
        col = colors_summ,
        main = "Recruitment Statuses",
        radius = 1)
  }, height = PLOT_HEIGHT_SUMM)
  
  # _ _ Telephone Screening plots ----
  
  # _ _ _ Eligibility Status plots ----
  
  output$telscrn_elg_summ_or_plot <- renderPlot({
    pie(x = telscrn_elg_summ_or()$n,
        labels = telscrn_elg_summ_or()$ts_elg_txt,
        col = colors_telescrn_elg,
        main = "Telephone Screening Eligibility",
        radius = 1)
  }, height = PLOT_HEIGHT_LEAD)
  
  output$telscrn_elg_summ_mi_plot <- renderPlot({
    pie(x = telscrn_elg_summ_mi()$n,
        labels = telscrn_elg_summ_mi()$ts_elg_txt,
        col = colors_telescrn_elg,
        main = "Telephone Screening Eligibility",
        radius = 1)
  }, height = PLOT_HEIGHT_LEAD)
  
  # _ _ _ Weekly Eligibility Status plots ----
  
  output$telscrn_elg_summ_week_or_plot <- renderPlot({
    ggplot(data = telscrn_elg_summ_week_or(),
           aes(x = ts_dat_week_lab, y = n, fill = ts_elg_txt)) +
      geom_bar(stat = "identity") +
      scale_x_date(breaks = seq(TS_DAT_START, Sys.Date(), by = 7),
                   limits = c(TS_DAT_START, Sys.Date())) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(x = "Week of Telephone Screening",
           y = "Count",
           fill = "Elegibility Category") +
      scale_fill_manual(values = colors_telescrn_elg) +
      theme(legend.position = 'bottom')
  }, height = PLOT_HEIGHT_LEAD)
  
  output$telscrn_elg_summ_week_mi_plot <- renderPlot({
    ggplot(data = telscrn_elg_summ_week_mi(),
           aes(x = ts_dat_week_lab, y = n, fill = ts_elg_txt)) +
      geom_bar(stat = "identity") +
      scale_x_date(breaks = seq(TS_DAT_START, Sys.Date(), by = 7),
                   limits = c(TS_DAT_START, Sys.Date())) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(x = "Week of Telephone Screening",
           y = "Count",
           fill = "Elegibility Category") +
      scale_fill_manual(values = colors_telescrn_elg) +
      theme(legend.position = 'bottom')
  }, height = PLOT_HEIGHT_LEAD)
  
  # _ _ _ Ineligibility Reason plots ----
  
  output$telscrn_en_summ_or_plot <- renderPlot({
    pie(x = telscrn_en_summ_or()$n,
        labels = telscrn_en_summ_or()$ts_en_txt,
        col = colors_telescrn_en,
        main = "Telephone Screening Ineligibility Reason",
        radius = 1)
  }, height = PLOT_HEIGHT_LEAD)
  
  output$telscrn_en_summ_mi_plot <- renderPlot({
    pie(x = telscrn_en_summ_mi()$n,
        labels = telscrn_en_summ_mi()$ts_en_txt,
        col = colors_telescrn_en,
        main = "Telephone Screening Ineligibility Reason",
        radius = 1)
  }, height = PLOT_HEIGHT_LEAD)
  
  # _ _ _ Weekly Ineligibility Reason plots ----
  output$telscrn_en_summ_week_or_plot <- renderPlot({
    ggplot(data = telscrn_en_summ_week_or(),
           aes(x = ts_dat_week_lab, y = n, fill = ts_en_txt)) +
      geom_bar(stat = "identity") +
      scale_x_date(breaks = seq(TS_DAT_START, Sys.Date(), by = 7),
                   limits = c(TS_DAT_START, Sys.Date())) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(x = "Week of Telephone Screening",
           y = "Count",
           fill = "Inelegibility Reason Category") +
      scale_fill_manual(values = colors_telescrn_en) +
      theme(legend.position = 'bottom')
  }, height = PLOT_HEIGHT_LEAD)
  
  output$telscrn_en_summ_week_mi_plot <- renderPlot({
    ggplot(data = telscrn_en_summ_week_mi(),
           aes(x = ts_dat_week_lab, y = n, fill = ts_en_txt)) +
      geom_bar(stat = "identity") +
      scale_x_date(breaks = seq(TS_DAT_START, Sys.Date(), by = 7),
                   limits = c(TS_DAT_START, Sys.Date())) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(x = "Week of Telephone Screening",
           y = "Count",
           fill = "Inelegibility Reason Category") +
      scale_fill_manual(values = colors_telescrn_en) +
      theme(legend.position = 'bottom')
  }, height = PLOT_HEIGHT_LEAD)
  
  # # _ Simple text fields ----
  # output$lead_categs_descrip <- renderText()
  
  # # _ Table => Plot interaction ----
  # output$x2 <- renderText({
  # str1 <- paste0(input$recruit_lead_summ_or_tbl_rows_selected,
  #                sep = '-')
  # str2 <- paste0(input$recruit_lead_summ_mi_tbl_rows_selected,
  #                sep = '-')
  # 
  #     paste0(
  #       str1,
  #       str2,
  #       sep = "<br/>"
  #     )
  # 
  # })
  # output$x3 <- renderText({
  #   paste(# "x:", round(as.numeric(input$plot_hover$x), 2),
  #         # "y:", round(as.numeric(input$plot_hover$y), 2),
  #         names(input$plot_hover) #,
  #         # unlist(input$plot_hover)
  #         )
  # })
  # output$x3 <- renderPrint({
  #   str(input$plot_click)
  # })
  # output$x4 <- renderPrint({
  #   ifelse(!is.null(input$plot_click),
  #          paste(round(as.numeric(input$plot_click$x), 2), 
  #                round(as.numeric(input$plot_click$y), 2),
  #                point_in_sector(input$plot_click$x, 
  #                                input$plot_click$y,
  #                                recruit_lead_summ_or()$n)),
  #          NA_character_
  #   )
  # })
  # output$x5 <- DT::renderDataTable(
  #   DT::datatable(
  #     rename(
  #       .data = select(
  #         .data =
  #           recruit_lead_summ_or(),
  #         lead_categ_txt, n
  #       ),
  #       `Lead Category` = lead_categ_txt
  #     ),
  #     rownames = FALSE,
  #     options = DT_OPTIONS,
  #     selection = 
  #       list(target = "row",
  #            selected = recruit_lead_summ_or_tbl_row_selex())
  #   ),
  #   server = FALSE
  # )
  # output$x6 <- renderPrint({
  #   recruit_lead_summ_or_tbl_row_selex()
  # })
  
}

# **** ----
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
