#


# source("global.R")
# library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ###########             Explore and EDA Tab              ###########    
    output$edaTextAboutThisProject <- renderText({
    paste("Though this is an assignment, but because of its openness, I really wanted to make it 
          a fun project instead."
          , "Test this"
          , sep = "\n"
          )})
    
    output$edaTextAboutTheData <- renderText({paste(
        "This dataset is retrieved from:"
        , "with"
        , sep = "\n"
    )})
    output$edaTextFirstImpression <- renderText({paste("Looking at the first few rows of the data, my first impression was: 'Interesting!'. 
This dataset collects students' horoscope information.", "Test this", sep = "\n")})
    
    output$edaTextFindFromSummary <- renderText({
        paste("There are few things I have noticed from the summary:"
              , "1. SpendTime1, SpendTime2, Self1, Superpower, these variables have values that can be conlidated."
              , "For example, in Superpower, 'fly' vs 'flying'."
              , sep = "\n"
              )
    })
    
    output$edaTextCleanseData <- renderText({
        paste("Conver all string to lower case")
    })
    
    
    
    output$summarySourceDT <- renderPrint(
        print(dfSummary(sourceDT, graph.magnif = 0.8), 
              method = "render",
              headings = TRUE,
              bootstrap.css = FALSE)
        )
    
    output$summaryFormattedDT <- renderPrint(
        print(dfSummary(formattedDT, graph.magnif = 0.8), 
              method = "render",
              headings = TRUE,
              bootstrap.css = FALSE)
    )
    
    ###########             WHAT Tab              ###########    
    
    # output$pieChartsFactors <- renderUI({
    #     plot_output_list <- lapply(unique(factorsData[, input$choosevar])
    #                                , function(i) {
    #                                    plotName <- paste0("plot", i)
    #                                    htmlOutput(plotName)
    #                                })
    #     tagList(plot_output_list)
    # })
    # 
    # for (i in 1:) {
    #     local({
    #         my_i <- i
    #         plotname <- paste0("plot", my_i)
    #         
    #         output[[plotname]] <- renderGvis({
    #             data <- mtcars[mtcars[,input$choosevar]==my_i,]
    #             if(dim(data)[1]>0){
    #                 gvisColumnChart(    
    #                     data, xvar='hp', yvar='mpg' 
    #                 )}
    #             else NULL
    #         })  
    #     })
    # }
    
    # output$distPlot <- renderPlot({
    # 
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # 
    # })

})
