library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(clusterPower)
library(CRTSize)
library(data.table)
library(databrew)

cp_choices <- c('Alpha' = 'cp_alpha',
                          'Power' = 'cp_power',
                          'M' = 'cp_m',
                          # 'N' = 'cp_n',
                          # 'CV' = 'cp_cv',
                          'D' = 'cp_d',
                          'ICC' = 'cp_icc',
                          'Within-cluster variation' = 'cp_varw')
method_choices <- c("Taylor" = "taylor", "Weighted" = "weighted")

source('theme_dashboard.R')
source('functions.R')
header <- dashboardHeader(title="Sample size")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Hayes & Bennet",
      tabName="hayes",
      icon=icon("calculator")),
    menuItem(
      text="Rotundi and Donner",
      tabName="rotundi",
      icon=icon("list-ol")),
    menuItem(
      text = 'clusterPower (CRAN)',
      tabName = 'cluster_power',
      icon = icon("cog", lib = "glyphicon")),
    menuItem(
      text = 'About',
      tabName = 'about',
      icon = icon("info"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(
      tabName="hayes",
      fluidPage(
        column(4,
               sliderInput('exp_group',
                           'Rate in experimental group',
                           min = 0,
                           max = 1,
                           value = 0.01),
               sliderInput('con_group',
                           'Rate in control group',
                           min = 0.0,
                           max = 0.2,
                           value = 0.005),
               fluidRow(
                 column(6,
                        sliderInput('alpha_num',
                                    'Significance level (alpha)',
                                    min = 0.0,
                                    max = 1,
                                    value = 0.05)),
                 column(6,
                        sliderInput('beta_num',
                                    'Significance level (beta)',
                                    min = 0.0,
                                    max = 1,
                                    value = 0.8))
               ),
               sliderInput('cv',
                           'CV',
                           min = 0.0,
                           max = 1,
                           value = 0.29),
               sliderInput('cluster_size',
                           'Cluster size',
                           min = 0,
                           max = 500,
                           value =200),
               sliderInput('followup',
                           'Follow ups',
                           min = 1,
                           max = 10,
                           value = 1)
        ),
        column(8,
               plotOutput('hayes_plot'),
               h3(textOutput('sample_size')))
      )),
    
    tabItem(
      tabName="rotundi",
      fluidPage(
        fluidRow(
          column(2,
                 sliderInput('exp_group_2',
                             'Rate in experimental group',
                             min = 0.001,
                             max = 0.02,
                             value = 0.01036)),
          column(2,
                 sliderInput('con_group_2',
                             'Rate in control group',
                             min = 0.001,
                             max = 0.02,
                             value = 0.0148)),
          column(2,
                 sliderInput('exp_size',
                             'Size of experimental group',
                             min = 200,
                             max = 600,
                             value = 424)),
          column(3,
                 sliderInput('time_period',
                             'Time periods',
                             min = 1,
                             max = 4,
                             value = 1)),
          column(3,
                 sliderInput('cv_2',
                             'CV',
                             min = 0.1,
                             max = 0.4,
                             value = 0.29))
        ),
        fluidRow(
          column(12, align = 'center',
                 h3(textOutput('text_rotundi')))
        )
        
        
      )
    ),
    
    tabItem(
      tabName = 'cluster_power',
      fluidPage(

        column(4,
               sliderInput('cp_alpha',
                           'Alpha (probability of type 1 error)',
                           min = 0.01,
                           max = 0.25,
                           value = 0.05),
               sliderInput('cp_power',
                           'Power (probability of type 2 error)',
                           min = 0.5,
                           max = 0.99,
                           value = 0.8),
               sliderInput('cp_m',
                           'M (number of clusters per condition)',
                           min = 2,
                           max = 100,
                           value = 35),
               sliderInput('cp_n',
                           'N (average cluster size)',
                           min = 100,
                           max = 1000,
                           value = 428)),
        column(4,
               sliderInput('cp_cv',
                           'CV (coefficient of variation of cluster sizes)',
                           min = 0,
                           max = 0.99,
                           value = 0.29),
               sliderInput('cp_d',
                           'D (diference in condition menas)',
                           min = 0.0001,
                           max = 0.01,
                           value = 0.00444),
               sliderInput('cp_icc',
                           'ICC (intraclass correlation)',
                           min = 0.01,
                           max = 0.2,
                           value = 0.05),
               sliderInput('cp_varw',
                           'Within-cluster variation',
                           min = 0.01,
                           max = 0.2,
                           value = 0.05)),

      column(4,
             selectInput('cp_method',
                         'Method',
                         choices = method_choices),
             selectInput('cp_to_calculate',
                         'Which parameter do you want to calculate?',
                         choices = cp_choices),
             helpText('NOTE: The input on the left for the parameter selected above will be ignored.'),
             h1(textOutput('cp_text')))
    )),
    
    
    tabItem(
      tabName = 'about',
      fluidPage(
        fluidRow(
          div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
          h4('Built in partnership with ',
             a(href = 'http://databrew.cc',
               target='_blank', 'Databrew'),
             align = 'center'),
          p('Empowering research and analysis through collaborative data science.', align = 'center'),
          div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                             icon = icon("envelope", lib = "font-awesome")),
                href="mailto:info@databrew.cc",
                align = 'center')), 
          style = 'text-align:center;'
        )
      )
    ))
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  showModal(modalDialog(title = 'Heads-up!',
                        fluidPage(
                          p('This application is just a prototype. It was built for internal project use. It is not yet intended for public consumption.')
                        ),
                        footer = modalButton('Got it.')))
  
  get_hayes_data <- reactive({ 
    # set default to NULL
    con_group <- exp_group <- alpha_num <- beta_num <- cv <- cluster_size <- follow_up <- NULL
    
    exp_group <- input$exp_group
    con_group <- input$con_group
    alpha_num <- input$alpha_num
    beta_num <- input$beta_num
    cv <- input$cv
    cluster_size <- input$cluster_size
    follow_up <- input$followup
    
    num <-round(hayes_rates( leff= exp_group, lcont= con_group, alpha=alpha_num, beta=beta_num, CV= cv, clustersize= cluster_size, followup=follow_up),2)
    
    return(num)
    
  })
  #   
  
  hayes_df <- reactive({
    cv <- NULL
    cv <- input$cv
    CVs <- seq(0,1, by=0.025)
    exp_group <- input$exp_group
    con_group <- input$con_group
    alpha_num <- input$alpha_num
    beta_num <- input$beta_num
    cv <- input$cv
    cluster_size <- input$cluster_size
    follow_up <- input$followup
    dat <- data.frame(
      CV = CVs,
      clusters =sapply(CVs,
                       function(x) round(hayes_rates( leff= exp_group, lcont= con_group, alpha=alpha_num, 
                                                      beta=beta_num, CV= x, clustersize= cluster_size, followup=follow_up),2)))
    return(dat)
  })
  
  output$sample_size <- renderText({
    
    
    hayes_num <- get_hayes_data()
    text_output <- paste0('The sample size for the given parameters is ', hayes_num,
                          '. Assess sensitivity CV (kappa) estimates by plotting the number of clusters 
                          per arm against CV.')
    return(text_output)
    
  })
  
  output$hayes_plot <- renderPlot({
    hayes_num <- get_hayes_data()
    cv <- input$cv
    df <- hayes_df()
    hn <- get_hayes_data()
    the_point <- tibble(x = cv, y = hn)
    
    p <- ggplot(data = df, aes(CV, clusters)) + 
      geom_area(fill = '#333333', alpha = 0.2) +
      geom_line() +
      labs( title="Influence of CV (kappa) on number of clusters per arm",
            subtitle="using Hayes & Bennet (Int. J. Epidem. 1999;28:319-326)" ) +
      geom_vline( xintercept = cv, linetype="dotted" ) +
      geom_hline(yintercept = hn, linetype = 'dotted') +
      geom_point(data = the_point, aes(x = x, y = y)) +
      theme_dashboard() +
      labs(y = 'Number of clusters per arm')
    return(p)
    
    
  })
  
  
  get_rotundi_data <- reactive({ 
    con_group_2 <- exp_group_2 <- exp_size <- time_period <- cv_2  <- NULL
    exp_group_2 <- input$exp_group_2
    con_group_2 <- input$con_group_2
    exp_size <- input$exp_size
    time_period <- input$time_period
    cv_2 <- input$cv_2
    
    save(con_group_2,
         exp_group_2,
         exp_size,
         time_period,
         cv_2,
         file = 'temp.RData')
    
    
    text_rotundi <- n4incidence(le=exp_group_2, lc=con_group_2, m=exp_size, t=time_period, CV=cv_2)
    # sample_size <- text_rotundi[11]
    text_rotundi <- unlist(paste0('Per Rotundi/Donner (2009): The required sample size is a minimum of ', round(unlist(text_rotundi[11])), ' clusters in the experimental group and a 
                                  minimum of ', round(unlist(text_rotundi[12])), ' in the control group, followed for ',
                                  round(unlist(text_rotundi[3])),
                                  ' time periods.'))
    
    return(text_rotundi)
    
  })
  
  output$text_rotundi <- renderText({
    
    
    text_rotundi <- get_rotundi_data()
    
    return(text_rotundi)
    
  })
  
  get_cluster_power_data <- reactive({
    
    
    mean_clusters <- input$mean_cluster_size
    diff <- input$diff
    cluster_per_condition <- input$cluster_per_condition
    varw <- input$varw
    
    df <-
      expand.grid( ICC = c(seq(0.01,0.05,by=0.01), seq(0.05, 0.5, by=0.05 )),
                   CV = c(0.1, 0.2, 0.29, seq(0.4, 3, by=0.1)),
                   varw = c(0.001, seq(0.01, 1, by=0.01))) %>%
      mutate( power = crtpwr.2mean(power=NA,
                                   n=mean_clusters,
                                   cv=CV,
                                   d=diff,
                                   icc=ICC,
                                   m=cluster_per_condition,
                                   varw=varw,
                                   method = c("taylor", "weighted")) ) %>%
      filter( !is.na(power)) %>%
      as.data.table()
    
    return(df)
  })
  
  
  output$cp_text <- renderText({
    cp_alpha <- input$cp_alpha
    cp_power <- input$cp_power
    cp_m <- input$cp_m
    cp_n <- input$cp_n
    cp_cv <- input$cp_cv
    cp_d <- input$cp_d
    cp_icc <- input$cp_icc
    cp_varw <- input$cp_varw
    cp_method <- input$cp_method
    
    cp_to_calculate <- input$cp_to_calculate
    message('Cp to calculate is ', cp_to_calculate)
    message('Value of Cp to calculate is ', get(cp_to_calculate))
    
    assign(cp_to_calculate, NA)
    message('New value of Cp to calculate is ', get(cp_to_calculate))
    
    
    varw <- crtpwr.2mean(alpha = cp_alpha,
                         power = cp_power,
                         m = cp_m,
                         n=cp_n, 
                         cv=cp_cv, 
                         d=cp_d, 
                         icc = cp_icc,
                         varw = cp_varw,
                         method = cp_method,
                         tol = .Machine$double.eps^0.25 )
    out <- as.character(round(varw, digits = 6))
    out <- paste0(names(cp_choices)[cp_choices == input$cp_to_calculate],
                  ' is\n', out)
    message('out is ', out)
    return(out)
    
  })
  
  
  
}

shinyApp(ui, server)