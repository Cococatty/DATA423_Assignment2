# This file contains ui function
# ui - ui function on Shiny application, contains all components to be shown on the application.

library(shiny)
library(shinydashboard)
library(shinythemes)
source("server.R")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(skin = "yellow",
    dashboardHeader(title = "Assignment 2 - Yongyan (Carina) Zheng, 85424581)"
                    , titleWidth = 500),
    dashboardSidebar(
        sidebarMenu(
            menuItem("EDA", tabName = "eda", icon = icon("eye"))
        )
    ),
    dashboardBody(
        ###########             MAIN PANEL             ###########
        fluidRow(
            tabItems(
                ###########             EDA Tab              ###########    
                tabItem(tabName = "eda"
                        , tabsetPanel(
                            tabPanel("Basic Data Overview"
                                     , htmlOutput("summaryDT")
                            )
                            , tabPanel("Look at the data with theory"
                                       , h3("Data Role")
                            )
                        ))
            )
        )
    )
    ## dashboardBody End
)
)

