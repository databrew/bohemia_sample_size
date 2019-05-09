library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)

header <- dashboardHeader(title="Sample size")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Main",
      tabName="main",
      icon=icon("eye")),
    menuItem(
      text = 'About',
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
      tabName="main",
      fluidPage(
        column(4,
               sliderInput('the_number',
                           'The number',
                           min = 1,
                           max = 20,
                           value = 5)),
        column(8,
               plotOutput('the_plot'))
      )
    ),
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
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  output$the_plot <- renderPlot({
    n <- input$the_number
    x <- rnorm(mean = 10, sd = 5, n = 1000)
    y <- rnorm(mean = 10, sd = 5, n = 1000)
    plot(x,y) 
    abline(v = n, col = 'red')
  })
}

shinyApp(ui, server)