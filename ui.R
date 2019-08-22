# This file contains ui function
# ui - ui function on Shiny application, contains all components to be shown on the application.

library(shiny)
library(shinydashboard)
# library(shinythemes)
# source("server.R")

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(skin = "yellow",
        dashboardHeader(title = "Assignment 2 - Yongyan (Carina) Zheng, 85424581)", titleWidth = 500),
        dashboardSidebar(
            sidebarMenu(
                        menuItem("Explore to understand the data", tabName = "eda", icon = icon("eye"))
                        , menuItem("Setting up the project", tabName = "definition", icon = icon("users-cog"))
                        , menuItem("Implement Multi-Level Classification", tabName = "classification", icon = icon("studiovinari"))
        )),
        dashboardBody(
            fluidRow(
                tabItems(
                    ###########             EDA Tab              ###########    
                    tabItem(tabName = "eda"
                            , tabsetPanel(
                                tabPanel("About this project"
                                         , textOutput("edaTextAboutProject"))
                                , tabPanel("The first Impression"
                                         , textOutput("edaTextFirstImpression")
                                         # , textOutput("edaTextFirstImpression")
                                         # , textOutput("edaTextFindFromSummary")
                                         )
                                , tabPanel("Original data"
                                               # , mainPanel( 
                                                   , htmlOutput("summarySourceDT")
                                                   # )
                                           )
                                           # )
                                           
                                , tabPanel("Source Data Visualization"
                                           , sidebarLayout(
                                               sidebarPanel(
                                                   # input$edaPlotCenter, scale = input$edaPlotScale        
                                                   checkboxInput("edaPlotCenter", "Center Data", value = FALSE)
                                                   , checkboxInput("edaPlotScale", "Scale Data", value = FALSE)
                                                   , sliderInput("edaPlotMultiplier", "Multiplier"
                                                                 , min = 1.5, max = 5, value = 1.5)
                                               )
                                               , mainPanel(
                                                   h3("Boxplot of existing numeric variables")
                                                   , plotOutput("edaBoxplot")
                                                   , h3("Barchart of existing factor variables")
                                                   , plotOutput("edaBarchart")
                                                   )
                                           )
                                )
                                
                                # , tabPanel("Processed data", htmlOutput("summaryFormattedDT"))
                               
                                # , tabPanel("Define the question", textOutput("edaFirstImpression"))
                                )
                    ## Closure - eda tabItem
                    )        
                    
                    ###########             Definition Tab             ###########
                    , tabItem(tabName = "definition"
                              , tabsetPanel(
                                  tabPanel(tabName = "Define the core question"
                                           , textOutput("defTextCoreQuestion"))
                              ))
                    ###########             Classification Tab             ###########
                    , tabItem(tabName = "classification"
                              , tabsetPanel(
                                  tabPanel( tabName = "Implement Classification"
                                            , h3("Select Sensor Data to view additional boxplot")
                                  )
                      ## Closure - classification Tab
                          ))
                    ## Closure - tabItems
                )
            )
            ## dashboardBody End
            )
        ## END OF shinyUI and dashboardPage
        ))