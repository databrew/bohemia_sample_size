library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(clusterPower)
library(CRTSize)
library(data.table)


source('functions.R')
header <- dashboardHeader(title="Sample size")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Hayes & Bennet",
      tabName="hayes",
      icon=icon("eye")),
    menuItem(
      text="Rotundi and Donner",
      tabName="rotundi",
      icon=icon("eye")),
    menuItem(
      text = 'clusterPower (CRAN)',
      tabName = 'cluster_power',
      icon = icon("cog", lib = "glyphicon")),
      menuItem(
        text = 'About databrew',
        tabName = 'about',
        icon = icon("cog", lib = "glyphicon"))
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
        column(2,
               sliderInput('exp_group',
                           'Experimental group',
                           min = 0,
                           max = 1,
                           value = 0.01)),
        column(2,
               sliderInput('con_group',
                           'Control group',
                           min = 0.0,
                           max = 0.2,
                           value = 0.005)),
        column(1,
               sliderInput('alpha_num',
                           'Significan level (alpha)',
                           min = 0.0,
                           max = 1,
                           value = 0.05)),
        column(1,
               sliderInput('beta_num',
                           'Significan level (beta)',
                           min = 0.0,
                           max = 1,
                           value = 0.8)),
        column(2,
               sliderInput('cv',
                           'CV',
                           min = 0.0,
                           max = 1,
                           value = c(0.015, 0.5))),
        column(2,
               sliderInput('cluster_size',
                           'Cluster size',
                           min = 0,
                           max = 500,
                           value =200)),
        column(2,
               sliderInput('followup',
                           'Follow ups',
                           min = 1,
                           max = 10,
                           value = 1))
        
        ),
      br(), br(),
      fluidPage( 
        column(4,
               h3(textOutput('sample_size'))),
        column(8,
               plotOutput('hayes_plot')))
       
      
    ),
    
    tabItem(
      tabName="rotundi",
      fluidPage(
        fluidRow(
          column(2,
                 sliderInput('exp_group_2',
                             'Experimental group',
                             min = 0,
                             max = 1,
                             value = 0.01036)),
          column(2,
                 sliderInput('con_group_2',
                             'Control group',
                             min = 0.0,
                             max = 1,
                             value = 0.0148)),
          column(2,
                 sliderInput('exp_size',
                             'Size of experimental group',
                             min = 1,
                             max = 1000,
                             value = 424)),
          column(2,
                 sliderInput('time_period',
                             'Time periods',
                             min = 0,
                             max = 5,
                             value = 1)),
          column(2,
                 sliderInput('cv_2',
                             'CV',
                             min = 0.0,
                             max = 1,
                             value = 0.29))
        ),
        fluidRow(
          column(8,
                 textOutput('text_rotundi'))
        )
       
        
      )
    ),
    
    tabItem(
      tabName="cluster_power",
      fluidPage(
        fluidRow(
          column(2,
                 sliderInput('mean_cluster_size',
                             'Mean cluster size',
                             min = 0,
                             max = 500,
                             value = 428)),
          column(2,
                 sliderInput('diff',
                             'Difference in conditional means',
                             min = 0.0,
                             max = 1,
                             value = 0.0148)),
          column(2,
                 sliderInput('cluster_per_condition',
                             'Cluster per condition',
                             min = 1,
                             max = 50,
                             value = 35)),
          column(2,
                 sliderInput('varw',
                             'Within cluster variation',
                             min = 0,
                             max = 0.1,
                             value = 0.001)),
          column(2,
                 sliderInput('icc',
                             'Choose ICC',
                             min = 0,
                             max = 0.1,
                             value = 0.05)),
          column(2,
                 sliderInput('cv_3',
                             'CV',
                             min = 0,
                             max = 1,
                             value = 0.2))
          
        ),
        fluidRow(
          column(6,
                 plotOutput('icc_power_plot')),
          column(6,
                plotOutput('cv_power_plot'))
        )
        
        
      )
    ),
    
    
    tabItem(
      tabName = 'about',
      fluidPage(
        fluidRow(
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
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  
  
  
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
    
    mean_cv <- mean(cv)

    num <-round(hayes_rates( leff= exp_group, lcont= con_group, alpha=alpha_num, beta=beta_num, CV= mean_cv, clustersize= cluster_size, followup=follow_up),2)

    return(num)
   
  })
  #   
  
  hayes_df <- reactive({
    cv <- NULL
    cv <- input$cv
    CVs <- seq(cv[1], cv[2], by=0.025)
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

    df <- hayes_df()
    
    p <- ggplot(data = df, aes(CV, clusters)) + geom_line() +
      labs( title="Influence of CV (kappa) on number of clusters per arm",
            subtitle="using Hayes & Bennet (Int. J. Epidem. 1999;28:319-326)" ) +
      geom_vline( xintercept = 0.29, linetype="dotted" ) +
      theme_classic()
    return(p)

    
  })
  
  
  get_rotundi_data <- reactive({ 
    # set default to NULL
    con_group_2 <- exp_group_2 <- exp_size <- time_period <- cv_2  <- NULL
    
    exp_group_2 <- 0.01
    con_group_2 <- 0.012
    exp_size <- 200
    time_period <- 1
    cv_2 <- 0.29
    
    exp_group_2 <- input$exp_group_2
    con_group_2 <- input$con_group_2
    exp_size <- input$exp_size
    time_period <- input$time_period
    cv_2 <- input$cv_2
    

    text_rotundi <- n4incidence(le=exp_group_2, lc=con_group_2, m=exp_size, t=time_period, CV=cv_2)
    # sample_size <- text_rotundi[11]
    text_rotundi <- unlist(paste0('The Rotundi/Donner (2009): The required sample size is a minimum of ', round(unlist(text_rotundi[11])), ' clusters in the experimental group and a 
                           minimum of ', round(unlist(text_rotundi[12])), ' in the control group, followed for the time period of length ', 
                                  round(unlist(text_rotundi[3]))))

    return(text_rotundi)
    
  })
  
  output$text_rotundi <- renderText({
    
    
    text_rotundi <- get_rotundi_data()
    
    return(text_rotundi)

  })
  
  get_cluster_power_data <- reactive({
    
    mean_clusters <- 428
    diff <- 0.02
    cluster_per_condition <- 35
    varw <- 0.0008
    
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
  
  output$cv_power_plot <- renderPlot({
    
    varw <- 0.001
    cv_3 <- 0.29
    varw <- input$varw
    cv_3 <- input$cv_3
    cv_dat <- get_cluster_power_data()
    
    ggplot( cv_dat[CV==cv_3 & varw==varw], aes(ICC, power) ) +
      geom_line() +
      scale_y_continuous( limits=c(0,1))+
      labs( title="Influence of ICC on power") +
      theme_classic()
    
    
    
    
    
    
  })
  
  output$icc_power_plot <- renderPlot({
    icc_dat <- get_cluster_power_data()
    
    icc_level <- input$icc
    varw <- input$varw
    cv_dat <- get_cluster_power_data()
    
    ggplot( cv_dat[ICC==icc_level & varw==varw], aes(CV, power) ) +
      geom_line() +
      scale_y_continuous( limits=c(0,1))+
      labs( title="Influence of CV (kappa) on power") +
      theme_classic()
    
  
    
  })
  
  
}

shinyApp(ui, server)