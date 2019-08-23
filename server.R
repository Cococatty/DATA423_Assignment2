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
        print(dfSummary(canterburyWood, graph.magnif = 0.8), 
              method = "render",
              headings = TRUE,
              bootstrap.css = FALSE)
        )
    
    output$edaSourcePlan <- renderText({
      paste("Few Characters"
              , "background info etc"
              , sep = "\n")
    })
    
    output$edaSourceBoxplot <- renderPlot({
        standarizedData <- scale(canterburyWood[ , numericColsCanta, drop = FALSE]
                                 , center = input$edaSourcePlotCenter, scale = input$edaSourcePlotScale)
        keyValues <- tidyr::gather(as.data.frame(standarizedData))
        keyValuesDT <- data.table(keyValues)
        
        ggplot(mapping = aes(x = keyValuesDT$key, y = keyValuesDT$value, fill = keyValuesDT$key)) +
            geom_boxplot(coef = input$edaSourcePlotMultiplier, outlier.colour = "red") +
            labs(title = paste("Boxplots at IQR multiplier of", input$edaSourcePlotMultiplier),
                 x = "Standardised variable value", y = "Std Value") +
            coord_flip()
    })
  
   
    output$edaSourceBarchart <- renderPlot({
        
    })
    
    
    
    ###########             EDA Cleansed Data              ###########    
    output$edaCleansedSummary <- renderPrint(
        print(dfSummary(canterburyWoodCleaned, graph.magnif = 0.8), 
              method = "render",
              headings = TRUE,
              bootstrap.css = FALSE)
        )
    
        
    output$edaCleansedBoxplot <- renderPlot({
        standarizedData <- scale(canterburyWoodCleaned[ , numericColsCleansed, drop = FALSE]
                                 , center = input$edaCleansedPlotCenter, scale = input$edaCleansedPlotScale)
        keyValues <- tidyr::gather(as.data.frame(standarizedData))
        keyValuesDT <- data.table(keyValues)
        
        ggplot(mapping = aes(x = keyValuesDT$key, y = keyValuesDT$value, fill = keyValuesDT$key)) +
            geom_boxplot(coef = input$edaCleansedPlotMultiplier, outlier.colour = "red") +
            labs(title = paste("Boxplots at IQR multiplier of", input$edaCleansedPlotMultiplier),
                 x = "Standardised variable value", y = "Std Value") +
            coord_flip()
    })

    
    output$edaCleansedBarchart <- renderGvis({
      visPlotList <- plotGVisColChart(canterburyWoodCleaned, factorColsCleansed)
      return(visPlotList)
      # cat(visPlotList$html$chart, file = "GoogleVis/R.html") 
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
