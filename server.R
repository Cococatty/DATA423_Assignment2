#


# source("global.R")
# library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ###########             About Project              ###########    
    output$edaTextAboutProject <- renderText({
        paste("Amazon is on fire."
              , "https://www.teururakau.govt.nz/news-and-resources/open-data-and-forecasting/forestry/new-zealands-forests/"
              , "background info etc"
              , sep = "\n")
    })
    output$edaTextFirstImpression <- renderText({
    paste("Though this is an assignment, but because of its openness, I really wanted to make it 
          a fun project instead."
          , "Test this"
          , sep = "\n"
          )})
    
    ###########             EDA Source Data              ###########    
    output$edaSourceSummary <- renderPrint(
        print(dfSummary(canterSource, graph.magnif = 0.8), 
              method = "render",
              headings = TRUE,
              bootstrap.css = FALSE)
        )
    
    output$edaSourcePlan <- renderText({
      paste("Few Characters"
              , "background info etc"
              , sep = "\n")
    })
    
    #######    ANALYSE NUMERIC VARIABLES OF SOURCE DATA    #######
    output$edaSourceBoxplot <- renderPlot({
        standarizedData <- scale(canterSource[ , canterSourceColsType$numericList, drop = FALSE]
                                 , center = input$edaSourcePlotCenter, scale = input$edaSourcePlotScale)
        keyValues <- tidyr::gather(as.data.frame(standarizedData))
        keyValuesDT <- data.table(keyValues)
        
        ggplot(mapping = aes(x = keyValuesDT$key, y = keyValuesDT$value, fill = keyValuesDT$key)) +
            geom_boxplot(coef = input$edaSourcePlotMultiplier, outlier.colour = "red") +
            labs(title = paste("Boxplots at IQR multiplier of", input$edaSourcePlotMultiplier),
                 x = "Standardised variable value", y = "Std Value") +
            coord_flip()
    })
  
    #######    ANALYSE FACTOR VARIABLES OF CLEANSED DATA    #######
    output$edaSourceBarchart <- renderGvis({
      visPlotList <- plotGVisColChart(canterSource, canterSourceColsType$factorList)
      return(visPlotList)
    })
    
    
    
    ###########             EDA Cleansed Data              ###########    
    output$edaCleansedSummary <- renderPrint(
        print(dfSummary(canterCleansed, graph.magnif = 0.8), 
              method = "render",
              headings = TRUE,
              bootstrap.css = FALSE)
        )
    
    #######    ANALYSE NUMERIC VARIABLES OF CLEANSED DATA    #######
    output$edaCleansedBoxplot <- renderPlot({
        standarizedData <- scale(canterCleansed[ , canterCleansedColsType$numericList, drop = FALSE]
                                 , center = input$edaCleansedPlotCenter, scale = input$edaCleansedPlotScale)
        keyValues <- tidyr::gather(as.data.frame(standarizedData))
        keyValuesDT <- data.table(keyValues)
        
        ggplot(mapping = aes(x = keyValuesDT$key, y = keyValuesDT$value, fill = keyValuesDT$key)) +
            geom_boxplot(coef = input$edaCleansedPlotMultiplier, outlier.colour = "red") +
            labs(title = paste("Boxplots at IQR multiplier of", input$edaCleansedPlotMultiplier),
                 x = "Standardised variable value", y = "Std Value") +
            coord_flip()
    })

    #######    ANALYSE FACTOR VARIABLES OF CLEANSED DATA    #######
    output$edaCleansedBarchart <- renderGvis({
      visPlotList <- plotGVisColChart(canterCleansed, canterCleansedColsType$factorList)
      return(visPlotList)
    })
    
    
    ###########             Definition Tab              ###########    
    output$defTextCoreQuestion <- renderText({ 
    paste("The second milestone is to setup the CORE question: what do I want to solve in this project?"
          , "Inspired"                                       
          , sep = "\n"
    )})
    
    # output$summaryFormattedDT <- renderPrint(
    #     print(dfSummary(formattedDT, graph.magnif = 0.8), 
    #           method = "render",
    #           headings = TRUE,
    #           bootstrap.css = FALSE)
    # )
    
    ###########             Project End              ###########    
    output$endMoreWork <- renderText({
        paste("Blah"
              , "Blah"
              , sep = "\n")
    })
    
    output$endPotentialExtension <- renderText({
        paste("Blah"
              , "Blah"
              , sep = "\n")
    })
})
