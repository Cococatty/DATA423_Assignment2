#


# source("global.R")
# library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ################## *******             ABOUT PROJECT *******              ##################
    # output$edaTextAboutProject <- renderPrint({
    #     paste("Amazon is on fire."
    #           , "https://www.teururakau.govt.nz/news-and-resources/open-data-and-forecasting/forestry/new-zealands-forests/"
    #           , "background info etc"
    #           , sep = "\n")
    # })
  
    # output$edaTextFirstImpression <- renderPrint({
    # paste("Though this is an assignment, but because of its openness, I really wanted to make it 
    #       a fun project instead."
    #       , "Test this"
    #       , sep = "\n"
    #       )})
    # 
    ################## *******             SOURCE DATA *******              ##################
    #######    EDA of Source Data    #######
    output$edaSourceSummary <- renderPrint(
        print(dfSummary(canterSource, graph.magnif = 0.8), 
              method = "render",
              headings = TRUE,
              bootstrap.css = FALSE)
        )
    
    output$edaSourcePlan <- renderPrint({
      paste("Few Characters"
              , "background info etc"
              , sep = "\n")
    })
    
    #######    Analysis Numeric Variables of Source Data    #######
    output$edaSourceBoxplot <- renderPlot({
        standarizedData <- scale(canterSource[ , canterSourceColsType$numericList, drop = FALSE]
                                 , center = input$edaPlotCenter, scale = input$edaPlotScale)
        keyValues <- tidyr::gather(as.data.frame(standarizedData))
        keyValuesDT <- data.table(keyValues)
        
        ggplot(mapping = aes(x = keyValuesDT$key, y = keyValuesDT$value, fill = keyValuesDT$key)) +
            geom_boxplot(coef = input$edaSourcePlotMultiplier, outlier.colour = "red") +
            labs(title = paste("Boxplots at IQR multiplier of", input$edaPlotMultiplier),
                 x = "Standardised variable value", y = "Std Value") +
            coord_flip()
    })
  
    #######    Analysis Factor Variables of Source Data    #######
    output$edaSourceBarchart <- renderGvis({
      visPlotList <- plotGVisColChart(canterSource, canterSourceColsType)
      return(visPlotList)
    })
    
    #######    Missing Data of Source Data    #######
    output$edaSourceMissingData <- renderPlot({visdat::vis_dat(canterSource)})
    
    output$edaSourceMissDataPattern <- renderPlot({ naniar::gg_miss_upset(canterSource) })
    
    #######    Text of Source Data    #######
    output$edaSourceBoxDesc <- renderPrint({
      cat(
        "Unpruned.thinnings.m3.ha., Thinnings.m3.ha., Pulplog.thinnings.m3.ha. variables have 0 or 1 level of value."
        , "Without Centering or Scaling data, variables Unpruned.logs.m3.ha., TRV.m3.ha., Pulplogs.m3.ha., Pruned.logs.m3.ha., Age.years. have very different value ranges."
        , "With Centering and Scaling enabled, these variables are more normally distributed, except for Pruned.logs.m3.ha.."
        , "Pruned.logs.m3.ha. has a lot of values outside of maximum boundary. We shall have a further check to confirm if they are true outliers."
        , "At the same time, first quartile and median of Pruned.logs.m3.ha. minimum value are very close"
        , sep = "\n"
      )
    })
    
    output$edaSourceBarDesc <- renderPrint({
      cat(
        "Wood.Supply.Region and Thinning variables have only one level of value."
        , "Therefore, I believe they can be excluded in further analysis."
        , "Severe level of Class imbalance exsits in all factor variables: Species, Pruning, Planting.coverage, Owner.size."
        , "Class imbalance should be fixed before actual modelling."
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
                                 , center = input$edaPlotCenter, scale = input$edaPlotScale)
        keyValues <- tidyr::gather(as.data.frame(standarizedData))
        keyValuesDT <- data.table(keyValues)
        
        ggplot(mapping = aes(x = keyValuesDT$key, y = keyValuesDT$value, fill = keyValuesDT$key)) +
            geom_boxplot(coef = input$edaCleansedPlotMultiplier, outlier.colour = "red") +
            labs(title = paste("Boxplots at IQR multiplier of", input$edaPlotMultiplier),
                 x = "Standardised variable value", y = "Std Value") +
            coord_flip()
    })

    #######    Analysis Factor Variables of Cleansed Data    #######
    output$edaCleansedBarchart <- renderGvis({
      visPlotList <- plotGVisColChart(canterCleansed, canterCleansedColsType)
      return(visPlotList)
    })
    
    
    #######    Missing Data of Cleansed Data    #######
    output$edaCleansedMissingData <- renderPlot({visdat::vis_dat(canterCleansed)})
    
    output$edaCleansedMissDT <- renderDataTable(
      canterCleansed[!complete.cases(canterCleansed), ]
      , options = list(scrollX = TRUE, pageLength = 10)
      )
   
    ################## *******             IMPUTATION *******              ##################
    output$imputationResultDT <- renderDataTable({
      ##  Call all required functions to try different imputations
      # splitTrainTestImpute(canterCleansed, input$imputeTrainRatio, "Pruning")
      splitTrainTestImpute(canterCleansed, 0.75, "Pruning")  
      imputeWithKNN()
      imputeWithRPart()
      imputeWithMICE()
      imputeWithRecipe()

      imputationAccuracyDT <<- data.table(method = names(imputationAccuracyList)
                                        , accuracy = NA_real_  )
      
      masterImputationResultDT <<- data.table(method = character(), expectedResult = character()
                                        , predictResult = character())
      # attributes(imputationAccuracyList)$names
      for ( i in seq(1:length(names(imputationAccuracyList))) ) {
        # i <- imputationAccuracyList[[1]]
        # i <- 1
        currentSet <- imputationAccuracyList[i]
        methodName <- as.character(names(currentSet))
        
        unlistDT <- as.data.table(unlist(currentSet, recursive = FALSE))
        v <- as.numeric(unique(unlistDT[, 3]))
        
        resultDT <- unlistDT[, -3]
        names(resultDT) <- c("expectedResult", "predictResult")
        resultDT[, method := rep(methodName, nrow(resultDT)) ]
        # print(resultDT)
        resultDT <- setcolorder(resultDT, c("method","expectedResult","predictResult"))

        masterImputationResultDT <<- base::rbind(masterImputationResultDT, resultDT)
        imputationAccuracyDT[ method == methodName, accuracy := v ]
      }
     imputationAccuracyDT
    })
    
    ################## *******             MODELLING *******              ##################
    
    
    ################## *******             CLASSIFICATION *******              ##################
    
    
    ################## *******             DEFINITION *******              ##################
    # output$defTextCoreQuestion <- renderPrint({ 
    # paste("The second milestone is to setup the CORE question: what do I want to solve in this project?"
    #       , "Inspired"                                       
    #       , sep = "\n"
    # )})
    # 
    # 
    ################## *******             PROJECT END *******              ##################
    

})

    # output$endPotentialExtension <- renderPrint({
    #     paste("Blah"
    #           , "Blah"
    #           , sep = "\n")
    # })