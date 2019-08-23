#


# source("global.R")
# library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ################## *******             ABOUT PROJECT *******              ##################
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
    
    ################## *******             SOURCE DATA *******              ##################
    #######    EDA of Cleansed Data    #######
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
    
    #######    Analysis Numeric Variables of Source Data    #######
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
  
    #######    Analysis Factor Variables of Source Data    #######
    output$edaSourceBarchart <- renderGvis({
      visPlotList <- plotGVisColChart(canterSource, canterSourceColsType)
      return(visPlotList)
    })
    
    #######    Text of Source Data    #######
    output$edaSourceBoxDesc <- renderText({
      paste(
        "Unpruned.thinnings.m3.ha., Thinnings.m3.ha., Pulplog.thinnings.m3.ha. variables have 0 or 1 level of value."
        , "Without Centering or Scaling data, variables Unpruned.logs.m3.ha., TRV.m3.ha., Pulplogs.m3.ha., Pruned.logs.m3.ha., Age.years. have very different value ranges."
        , "With Centering and Scaling enabled, these variables are more normally distributed, except for Pruned.logs.m3.ha.."
        , "Pruned.logs.m3.ha. has a lot of values outside of maximum boundary. We shall have a further check to confirm if they are true outliers."
        , "At the same time, first quartile and median of Pruned.logs.m3.ha. minimum value are very close"
        , sep = "\n"
      )
    })
    
    output$edaSourceBarDesc <- renderText({
      paste(
        "Wood.Supply.Region and Thinning variables have only one level of value."
        , "Therefore, I believe they can be excluded in further analysis."
        , "Severe level of Class imbalance exsits in all factor variables: Species, Pruning, Planting.coverage, Owner.size."
        , "Solution"
        , sep = "\n"
      )
    })
    
    
    ################## *******             CLEANSED DATA *******              ##################
    
    #######    EDA of Cleansed Data    #######
    output$edaCleansedSummary <- renderPrint(
        print(dfSummary(canterCleansed, graph.magnif = 0.8), 
              method = "render",
              headings = TRUE,
              bootstrap.css = FALSE)
        )
    
    #######    Analysis Numeric Variables of Cleansed Data    #######
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

    #######    Analysis Factor Variables of Cleansed Data    #######
    output$edaCleansedBarchart <- renderGvis({
      visPlotList <- plotGVisColChart(canterCleansed, canterCleansedColsType)
      return(visPlotList)
    })
    
    
    #######    Text of Cleansed Data    #######
    # output$edaCleansedVisDesc <- renderText({
    #   paste(
    #     " "Solution"
    #     , sep = "\n"
    #   )
    # })
    
    # output$edaCleansedResult <- renderText({
    #   paste(
    #     "Class imbalance"
    #     , " "
    #     , sep = "\n"
    #   )
    # })
    
    
    
    ################## *******             DEFINITION *******              ##################
    output$defTextCoreQuestion <- renderText({ 
    paste("The second milestone is to setup the CORE question: what do I want to solve in this project?"
          , "Inspired"                                       
          , sep = "\n"
    )})
    
   
    ################## *******             PROJECT END *******              ##################
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
