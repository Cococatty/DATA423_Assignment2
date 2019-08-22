#


# source("global.R")
# library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ###########             Explore and EDA Tab              ###########    
    output$edaTextAboutProject <- renderText({
        paste("https://vincentarelbundock.github.io/Rdatasets/doc/ggplot2/diamonds.html"
              , "background info etc"
              , sep = "\n")
    })
    output$edaTextFirstImpression <- renderText({
    paste("Though this is an assignment, but because of its openness, I really wanted to make it 
          a fun project instead."
          , "Test this"
          , sep = "\n"
          )})
    
    output$summarySourceDT <- renderPrint(
        print(dfSummary(diamondsData, graph.magnif = 0.8), 
              method = "render",
              headings = TRUE,
              bootstrap.css = FALSE)
        )
    
    output$edaBoxplot <- renderPlot({
        standarizedData <- scale(diamondsData[ ,numericCols, drop = FALSE], center = input$edaPlotCenter, scale = input$edaPlotScale)
        keyValues <- tidyr::gather(as.data.frame(standarizedData))
        keyValuesDT <- data.table(keyValues)
        
        ggplot(mapping = aes(x = keyValuesDT$key, y = keyValuesDT$value, fill = keyValuesDT$key)) +
            geom_boxplot(coef = multiplier, outlier.colour = "red") +
            labs(title = paste("Boxplots at IQR multiplier of", input$edaPlotMultiplier),
                 x = "Standardised variable value", y = "Std Value") +
            coord_flip()
    })
  
   
    output$edaBarchart <- renderPlot({
        
    })
    ###########             Definition Tab              ###########    
    output$defTextCoreQuestion <- renderText({ 
    paste("The second milestone is to setup the CORE question: what do I want to solve in this project?"
          , "Here, "                                       
          , sep = "\n"
    )})
    
    # output$summaryFormattedDT <- renderPrint(
    #     print(dfSummary(formattedDT, graph.magnif = 0.8), 
    #           method = "render",
    #           headings = TRUE,
    #           bootstrap.css = FALSE)
    # )
    
})
