# Program Notes: Currently, data can be added to finaldata infinite times, resutling copies of the same parameter.
# There has to be a process to eliminate multiple copies in finaldata



# Set maximum filesize to 30 MB
shiny.maxRequestSize = 30*1024^2

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)

source("scripts.R")

# finaldata <- data.frame(NST_DATI = as.POSIXct(character()), variable = character(), value = numeric(0)) # I don't know what this does...

shinyServer(function(input, output) {
  #####################################################################################
  #                              Main data input process                              #
  # Reads in a csv file, eliminates unneeded columns and sets proper date-time format #
  # Returns dataframe 'rawdata()' for further processing                              #
  #####################################################################################
  
  # Create dataframe for final data
  finaldata <- reactiveValues()
  finaldata$values <- data.frame()
  
  # Read in CSV file, set date as POSIXct, and save as rawdata()
  rawdata <- reactive({
    if(!is.null(input$rawfile)) {
      # Set NAs as *, NA, or blanks
      rawdata <- read.csv((input$rawfile)$datapath, na.strings = c("*", "NA", ""))
      
      # Set columns to retain in variable "Keep" and remove others from dataframe
      keep.columns <- c("NST_DATI", "WATER_TEMP", "PH", "SPEC_CONDUCT", "DO", "PER_SATUR", "TURBIDITY", "TDS", "STAGE", "FLOW", input$custominput1.column, input$custominput2.column, input$custominput3.column)
      rawdata <- rawdata[,(names(rawdata) %in% keep.columns)]
      
      # Search for frontslash character and execute MDY POSIXct transformation if true, YMD POSIXct transformation if false
      if(grepl("/", rawdata$NST_DATI[1])) {{
        if(grepl("m", rawdata$NST_DATI[1], ignore.case = TRUE)) {
          rawdata$NST_DATI <- as.POSIXct(rawdata$NST_DATI, "%m/%d/%Y %I:%M %p", tz = "UTC")
        } else {
          rawdata$NST_DATI <- as.POSIXct(rawdata$NST_DATI, "%m/%d/%Y %H:%M", tz = "UTC")
        }
      }} else {
        rawdata$NST_DATI <- as.POSIXct(rawdata$NST_DATI, "%Y-%m-%d %H:%M", tz = "UTC")
      }
    }
    return(rawdata)
  })
  
  # Define inputs needed to enable custom parameters as used in output$AddCustom below
  AddCustomInputs <- reactive(
    if(input$AddCustomCheck == TRUE) {
      list(
        textInput('custominput1.column', 'Custom  1: ADRS Column Name', value = NULL),
        textInput('custominput1.label', 'Custom  1: Desired Label', value = NULL),
        hr(),
        textInput('custominput2.column', 'Custom  2: ADRS Column Name', value = NULL),
        textInput('custominput2.label', 'Custom  2: Desired Label', value = NULL),
        hr(),
        textInput('custominput3.column', 'Custom  3: ADRS Column Name', value = NULL),
        textInput('custominput3.label', 'Custom  3: Desired Label', value = NULL)
      )
    })
  
  output$AddCustom <- renderUI(
    AddCustomInputs()
  )
  
  ####################################################################################
  #                           Data grooming process                                  #
  # Processes raw datafile and removes transmission gaps                             #
  # Returns rawdata.fill()                                                           #
  ####################################################################################
  
  # Set seq() 'by' argument used groom()
  interval <- eventReactive (input$groomgo, {
    as.character(paste(input$sequnceminutes, "min"))
  })
  
  # Identify and fill transmission gaps. Gather into Long-Format
  rawdata.fill <- eventReactive(input$groomgo, {
    if(!is.null(rawdata())) {
      rawdata.fill_ <- groom(rawdata(), interval())
      rawdata.fill_ <- gather(rawdata.fill_, variable, value, -1)
      rawdata.fill_$variable <- factor(rawdata.fill_$variable, levels = c("WATER_TEMP", "PH", "SPEC_CONDUCT", "DO", "PER_SATUR", "TURBIDITY", "TDS", "STAGE", "FLOW", input$custominput1.column, input$custominput2.column, input$custominput3.column), labels = c("Temperature (C)", "pH (Units)", "Conductivity (uS/cm)", "DO (mg/l)", "DO ('%-Sat')", "Turb (NTU)", "TDS (g/l)", "Stage (m)", "Flow (m3/s)", input$custominput1.label, input$custominput2.label, input$custominput3.label))
      return(rawdata.fill_)
    }
  })
  
  ####################################################################################
  #                                Graphing Routine                                  #
  # Present a dropbox from rawdata.fill() and create a dataframe from the selection. #
  # Graph the selection and allow edits to the frame. Provide buttons to reset edits,#
  # toggle points, and save results to be appended to finaldata                      #
  ####################################################################################
  
  # Based on rawdata.fill()$variables column, populate a selectInput called "GraphSubset" with variable data
  output$selector <- renderUI({
    selectInput('GraphSubset', 'Graph Parameter', choices = levels(rawdata.fill()$variable))
  })
  
  
  vals <- reactiveValues()
  
  df <- reactive({
    vals$keeprows <- rep(TRUE, nrow(rawdata.fill()[rawdata.fill()$variable %in% input$GraphSubset, ]))
    return(rawdata.fill()[rawdata.fill()$variable %in% input$GraphSubset, ])
  })
  
  # Create frame that holds data to be graphed
  keep <- reactive(
    df()[vals$keeprows, , drop = FALSE]
  )
  
  output$figure <- renderPlot({
    #exclude <- df()[!vals$keeprows, , drop = FALSE] # uncomment line if excluded values are desired
    
    if(!is.null(df())) {
      plot <- ggplot(data = na.omit(keep()), aes(x = NST_DATI, y = value)) +
        geom_line() +
        geom_smooth( colour = "gray50") +
        labs(x = "Date", y = input$GraphSubset, title = paste(input$GraphSubset, "vs. Date"))
      return(plot)
    }
  })
  
  # Observe for Plot Edit functions, save data to finaldata$values, and generate table indicating what parameters are in finaldata$values
  observeEvent(input$figure_click, {
    res <- nearPoints(df(), input$figure_click, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints (df(), input$figure_brush, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(df()))
    finaldata$values <- finaldata$values[finaldata$values$variable != input$GraphSubset, ]
    reportvars <- data.frame("Parameter" = unique(finaldata$values$variable))
    output$finaldatareport_table <- renderTable({reportvars}, include.rownames = FALSE)
    output$finaldatareport <- renderUI(
      tableOutput('finaldatareport_table')
    )
  })
  observeEvent(input$finaldata_include, {
    #When user clicks "Save" check if data is already in finaldata$values. If it is, display js box. If it isn't add and remove lines of NAs.
    if(any(keep()$variable %in% finaldata$values$variable)) {
      info("This Parameter was already saved. Please Reset and edit to save again.")
    } else {
      finaldata$values <- rbind(finaldata$values, keep())
      finaldata$values <- finaldata$values[!(rowSums(is.na(finaldata$values)) == 3),]
      reportvars <- data.frame("Parameter" = unique(finaldata$values$variable))
      output$finaldatareport_table <- renderTable({reportvars}, include.rownames = FALSE)
      output$finaldatareport <- renderUI(
        tableOutput('finaldatareport_table')
      )
    }
  })
  
  ########################################
  # Generate Stats from finaldata$values #
  ########################################

  observeEvent(input$makestats, {
    isolate({
      output$stats <- renderTable({
        stats_ <- finaldata$values %>%
          select(variable, value) %>%
          group_by(variable) %>%
          summarise(Mean = mean(value, na.rm = TRUE), Median = median(value, na.rm = TRUE), Min = min(value, na.rm = TRUE), Max = max(value, na.rm = TRUE))
        return(stats_)
      }, include.rownames = FALSE)
    })
  })
  
  ######################
  # Final Data Download#
  ######################
  output$downloadfinaldata <- downloadHandler(
    filename = function() {
      paste('final-', format(Sys.time(), "%Y-%m-%d %H%M"), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(spread(finaldata$values, variable, value), file, row.names = FALSE)
    }
  )
})