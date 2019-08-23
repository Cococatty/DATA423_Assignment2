# This file contains ui function
# ui - ui function on Shiny application, contains all components to be shown on the application.

library(shiny)
library(shinydashboard)
# library(shinythemes)
# source("server.R")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(skin = "yellow"
    , dashboardHeader(title = "Assignment 2 - Yongyan (Carina) Zheng, 85424581)", titleWidth = 500)
    , dashboardSidebar(width = 300, sidebarMenu(
        menuItem("Project Background", tabName = "aboutProject",icon = icon("eye"))
        , menuItem("Explore source data", tabName = "edaSource",icon = icon("studiovinari"))
        , menuItem("Explore cleansed data", tabName = "edaCleansed",icon = icon("telegram-plane"))
        , menuItem("Setting up the project", tabName = "definition", icon = icon("users-cog"))
        , menuItem("Implement Multi-Level Classification", tabName = "classification", icon = icon("tree"))
        , menuItem("Ending Project", tabName = "projectEnd", icon = icon("globe"))
    ))
    , dashboardBody(fluidRow(tabItems(
        ###########             aboutProject Tab              ###########
        tabItem(
            tabName = "aboutProject"
            , tabsetPanel(
                tabPanel("About this project"
                         , textOutput("edaTextAboutProject"))
                , tabPanel("The first Impression"
                         , textOutput("edaTextFirstImpression")
                         # , textOutput("edaTextFirstImpression")
                         # , textOutput("edaTextFindFromSummary")
                         )
                )
            ## Closure - aboutProject
            )
            ###########             Source Data Tab              ###########
            , tabItem(tabName = "edaSource"
                      , sidebarLayout(
                          sidebarPanel(width = 2
                                        , checkboxInput("edaSourcePlotCenter", "Center Data", value = FALSE)
                                        , checkboxInput("edaSourcePlotScale", "Scale Data", value = FALSE)
                                        , sliderInput("edaSourcePlotMultiplier", "Multiplier", min = 1.5, max = 5,value = 1.5)
                        ## Closure - sidebarPanel
                        )
                        , mainPanel(tabsetPanel(
                         tabPanel("Source Data Summary"
                                  , htmlOutput("edaSourceSummary")
                         )
                         ###########             Source Data Visualization Tab              ###########
                         , tabPanel("Source Data Visualization"
                            , h3("Boxplot of source numeric variables")
                            , plotOutput("edaSourceBoxplot")
                            , h3("Barchart of source factor variables")
                            , plotOutput("edaSourceBarchart")
                            )
                         ###########             Source Data Plan Tab              ###########
                         , tabPanel("What shall I do?"
                                    , textOutput("edaSourcePlan")
                                    )
                        ## Closure - Source Data mainPanel
                        )
                        ## Closure - Source Data Tab tabsetPanel
                        )
                      ## Closure - Source Data Tab sidebarLayout
                        )
                    ## Closure - Source Data Tab
                    )
        
            ###########             Cleansed Data Tab              ###########
            , tabItem(tabName = "edaCleansed"
                      , sidebarLayout(
                          sidebarPanel(width = 2
                                        , checkboxInput("edaCleansedPlotCenter", "Center Data", value = FALSE)
                                        , checkboxInput("edaCleansedPlotScale", "Scale Data", value = FALSE)
                                        , sliderInput("edaCleansedPlotMultiplier", "Multiplier", min = 1.5, max = 5,value = 1.5)
                        ## Closure - sidebarPanel
                        )
                        , mainPanel(tabsetPanel(
                         tabPanel("Cleansed Data Summary"
                                  , htmlOutput("edaCleansedSummary")
                         )
                         ###########             Cleansed Data Visualization Tab              ###########
                         , tabPanel("Cleansed Data Visualization"
                            , h3("Boxplot of Cleansed numeric variables")
                            , plotOutput("edaCleansedBoxplot")
                            , h3("Barchart of Cleansed factor variables")
                            , plotOutput("edaCleansedBarchart")
                            )
                        ## Closure - Cleansed Data mainPanel
                        )
                       ## Closure - Cleansed Data Tab tabsetPanel
                        )
                       ## Closure - Cleansed Data Tab sidebarLayout
                        )
                    ## Closure - Cleansed Data Tab
                    )
                    
        
            ###########             Definition Tab             ###########
            , tabItem(tabName = "definition"
                    , tabsetPanel(
                        tabPanel(tabName = "Define the core question"
                                 , textOutput("defTextCoreQuestion")
                                 )
                        ## Closure - Definition tabsetPanel
                        )
            ## Closure - Definition Tab
                    )
    
            ###########             Classification Tab             ###########
            , tabItem(tabName = "classification"
                    , sidebarLayout(
                          sidebarPanel(width = 2
                                        , checkboxInput("class", "Center Data", value = FALSE)
                        ## Closure - sidebarPanel
                        )
                        , mainPanel(tabsetPanel(
                            ###########             Cleansed Data Visualization Tab              ###########
                             tabPanel("Class Data Summary"
                                      , h2("test")
                                      # , htmlOutput("edaCleansedSummary")
                             )
                        ## Closure - Classification mainPanel
                        )
                       ## Closure - Classification Tab tabsetPanel
                        )
                       ## Closure - Classification Tab sidebarLayout
                        )
                    ## Closure - Classification Tab
                    )
       
            ###########             Project End Tab             ###########
            , tabItem(tabName = "projectEnd"
                   , tabsetPanel(
                        ###########             Project End Tab              ###########
                         tabPanel("Remaining Work"
                                  , textOutput("endMoreWork")
                         )
                        , tabPanel("Potential Extension"
                                   , textOutput("endPotentialExtension")
                                   )
                       ## Closure - Project End Tab tabsetPanel
                        )
                        ## Closure - Project End Tab
                        )
            ## ------------------------------------------------------------------------------------------------------------------------------ ##
                    ## Closure - tabItems
                )
            )
            ## Closure - fluidRow
            )
            ## Closure - dashboardBody
            )
            ## Closure - shinyUI and dashboardPage
        )
# )
        
        
        # , sidebarLayout(
        #     sidebarPanel(
        #         # input$edaPlotCenter, scale = input$edaPlotScale
        #         checkboxInput("edaPlotCenter", "Center Data", value = FALSE)
        #         , checkboxInput("edaPlotScale", "Scale Data", value = FALSE)
        #         , sliderInput("edaPlotMultiplier", "Multiplier"
        #                       , min = 1.5, max = 5, value = 1.5)
        #     )
        #     , mainPanel(