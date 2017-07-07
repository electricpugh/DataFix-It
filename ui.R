library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Data Fix-it"),
  sidebarLayout(
    sidebarPanel(
      fileInput('rawfile', 'Input *.CSV'),
      helpText("Files downloaded from ADRS Console must be saved as *.csv for import."),
      h4("Other Parameters"),
      tags$body("Only the 'Big 7' parameters will be retained, unless specified."),
      br(),
      checkboxInput('AddCustomCheck', 'Add custom parameters'),
      uiOutput('AddCustom'),
      hr(),
      numericInput('sequnceminutes', 'Water Quality Interval (mins)', value = 60),
      actionButton('groomgo', 'Groom'),
      hr(),
      h4("Saved Data"),
      uiOutput('finaldatareport')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 br(),
                 uiOutput('selector'),
                 actionButton('finaldata_include', 'Save'),
                 plotOutput('figure', click = "figure_click", brush = "figure_brush"),
                 actionButton('exclude_toggle', 'Remove Multiple Points'),
                 actionButton('exclude_reset', 'Reset')
        ),
        tabPanel("Statistics",
                 br(),
                 actionButton('makestats', 'Display Satistics'), 
                 br(),
                 hr(),
                 tableOutput('stats')
        ),
        tabPanel("Final Data",
                 br(),
                 tags$body("Download final data."),
                 br(),
                 downloadButton('downloadfinaldata', 'Download')
        ), 
        tabPanel("Help",
                 br(),
                 HTML('<iframe width="640" height="360" src="https://www.youtube.com/embed/PmPraEYbmP0" frameborder="0" allowfullscreen></iframe>')
                 )
      )
    )
  )))