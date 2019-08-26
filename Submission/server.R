# Author: Yongyan (Carina) Zheng
# Student ID 85424581
# DATA423 Assignment 2#


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ################## *******             ABOUT PROJECT *******              ##################
   
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

    ##    Re-apply imputation learning when ratio is changed
    observeEvent( input$imputeTrainRatio, {
      ## The ratio is set to percentage for the ease of user, therefore, it needs to be divided by 100
      ## before further calculations
      imputeMaster(canterCleansed, input$imputeTrainRatio/100, "Pruning")
      output$imputationResultTable <- renderDataTable({
        masterImputationResultDT[ method %in% input$imputeMethods, ]
        })
      
      output$imputationAccuracyTable <- renderDataTable({
        imputationAccuracyDT[ method %in% input$imputeMethods, ]
      ## TBD  - to change var to input?
    })
    })
    
    output$imputationResultTable <- renderDataTable({
      masterImputationResultDT[ method %in% input$imputeMethods, ]
      ## TBD  - to change var to input?
    })
    
    output$imputationAccuracyTable <- renderDataTable({
      imputationAccuracyDT[ method %in% input$imputeMethods, ]
      ## TBD  - to change var to input?
    })
    
    output$imputeResultBarchart <- renderPlot({
      ggplot(imputationAccuracyDT[ method %in% input$imputeMethods, ], aes(method, weight = accuracy)) +
        geom_bar(fill = "#FF6666") +
        xlab("Selected Imputation Methods") +
        ylab("Accuracy in Percentage") +
        labs(title = "Accuracy visual comparsion between selected methods")
    })


    ################## *******             MODELLING *******              ##################
    ## The ratio is set to percentage for the ease of user, therefore, it needs to be divided by 100
    ## before further calculations
    ## input$modelTrainRatio
    ## Re-apply modelling learning when ratio is changed
    observeEvent( input$modelTrainRatio, {
      ## The ratio is set to percentage for the ease of user, therefore, it needs to be divided by 100
      ## before further calculations
      modelMaster(input$modelTrainRatio/100)
      output$masterModelResultDT <- renderDataTable({
        masterModelResultDT[ method %in% input$modelMethods, ]
        })

      output$modelAccuracyTable <- renderDataTable({
        modelAccuracyDT[ method %in% input$modelMethods, ]
      ## TBD  - to change var to input?
      })
      
      output$modelConfMatROSE <- renderPrint({modelResultConfMatROSE})
      output$modelConfMatWeighted <- renderPrint({modelResultConfMatWeighted})
      output$modelConfMatRecipe <- renderPrint({modelResultConfMatRecipe})
      output$modelSimpleTable <- renderPrint({modelTabSimple})
      
      output$modelResultBarchart <- renderPlot({
        ggplot(modelAccuracyDT[ method %in% input$modelMethods, ], aes(method, weight = accuracy)) +
          geom_bar(fill = "#FF6666") +
          xlab("Selected Modelling Methods") +
          ylab("Accuracy in Percentage") +
          labs(title = "Accuracy visual comparsion between selected methods")
      })
    })

    output$masterModelResultDT <- renderDataTable({
      masterModelResultDT[ method %in% input$modelMethods, ]
      ## TBD  - to change var to input?
    })

    output$modelAccuracyTable <- renderDataTable({
      modelAccuracyDT[ method %in% input$modelMethods, ]
      ## TBD  - to change var to input?
    })
    
    output$modelConfMatROSE <- renderPrint({modelResultConfMatROSE})
    output$modelConfMatWeighted <- renderPrint({modelResultConfMatWeighted})
    output$modelConfMatRecipe <- renderPrint({modelResultConfMatRecipe})
    output$modelSimpleTable <- renderPrint({modelTabSimple})
    
    output$modelResultBarchart <- renderPlot({
      ggplot(modelAccuracyDT[ method %in% input$modelMethods, ], aes(method, weight = accuracy)) +
        geom_bar(fill = "#FF6666") +
        xlab("Selected Modelling Methods") +
        ylab("Accuracy in Percentage") +
        labs(title = "Accuracy visual comparsion between selected methods")
    })

    
    ################## *******             CLASSIFICATION *******              ##################
    output$treeRPart <- renderPlot({
      plotRPartTree()
    })
    
    
    ################## *******             PROJECT END *******              ##################
    

})