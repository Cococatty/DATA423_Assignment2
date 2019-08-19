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
                        menuItem("Explore to understand the data", tabName = "eda", icon = icon("eye")),
                        # menuItem("Understand the data", tabName = "eda", icon = icon("user")),
                        menuItem("Classification", tabName = "classification", icon = icon("studiovinari"))
        )),
        dashboardBody(
            fluidRow(
                tabItems(
                    ###########             Explore and EDA Tab              ###########    
                    tabItem(tabName = "eda"
                            , tabsetPanel(
                                tabPanel("The first Impression"
                                         , textOutput("edaTextAboutThisProject")
                                         , textOutput("edaTextFirstImpression")
                                         , textOutput("edaTextFindFromSummary")
                                         )
                                , tabPanel("Original data", htmlOutput("summarySourceDT"))
                                , tabPanel("Processed data", htmlOutput("summaryFormattedDT"))
                                # , tabPanel("Data Info",
                                #          fluidRow(
                                #              infoBoxOutput("infoBoxNrow")
                                #          )
                                # )
                                # , tabPanel("Data Overview - Before Formatting", htmlOutput("summarySourceDT"))
                                
                                
                                # , tabPanel("Define the question", textOutput("edaFirstImpression"))
                                )
                            
                    )        
                    ###########             classification Tab             ###########
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