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
                        menuItem("Explore the data", tabName = "eda", icon = icon("eye")),
                        menuItem("Understand the data", tabName = "classification", icon = icon("user"))
        )),
        dashboardBody(
            fluidRow(
                tabItems(
                    ###########             EDA Tab              ###########    
                    tabItem(tabName = "eda"
                            , tabsetPanel(
                                tabPanel("Data Structure and Types"
                                         , h3("Data ")
                                )
                                , tabPanel("High-level Data Overview", htmlOutput("summaryDT")
                                ))
                            
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