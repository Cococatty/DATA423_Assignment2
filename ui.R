# This file contains ui function
# ui - ui function on Shiny application, contains all components to be shown on the application.

library(shiny)
library(shinydashboard)
# library(shinythemes)
# source("server.R")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(skin = "yellow"
    , dashboardHeader(title = "Assignment 2 - Yongyan (Carina) Zheng, 85424581)", titleWidth = 500)
    ################## *******             MENU *******              ##################
    , dashboardSidebar(width = 300, sidebarMenu(
        menuItem("Project Background", tabName = "aboutProject",icon = icon("eye"))
        , menuItem("Explore data", tabName = "eda",icon = icon("studiovinari"))
        , menuItem("Imputation", tabName = "imputation", icon = icon("puzzle-piece"))
        , menuItem("Build models", tabName = "modelling",icon = icon("telegram-plane"))
        # , menuItem("Multi-Level Classification", tabName = "classification", icon = icon("tree"))
        , menuItem("Ending Project", tabName = "projectEnd", icon = icon("globe"))
    ))
    , dashboardBody(fluidRow(tabItems(
        ################## *******             ABOUT PROJECT *******              ##################
        tabItem(tabName = "aboutProject", tabsetPanel(
            tabPanel("About this project"
                     , textOutput("edaTextAboutProject")
                     , h1("Amazon is ON FIRE!")
                     , img(src = "Amazon-rainforest-fire-1168299.jpg")
                     , div(
                         p("Amazon has bee on fire for more than 3 weeks now. This is a huge concern to the world because
                         Amazon is the lungs of the world. \"The Amazon represents over half of the planet\'s remaining 
                         rainforests and comprises the largest and most biodiverse tract of tropical rainforest in the world, 
                         with an estimated 390 billion individual trees divided into 16,000 species.\"[1]. This relates
                           to one of the topics that I am very interested in - environmentally friendly.")
                         , p("Though this is an assignment, but given its openness, I really wanted to make it a fun 
                             project as well.")
                         , p("[1] Amazon rainforest, Wikipedia, retrieved from https://en.wikipedia.org/wiki/Amazon_rainforest on 
                             18th August 2019")
                     )
                     )
            , tabPanel("Information Gathering"
                       , div(
                           p("I did some information search regarding to forest fire in the world, unfortunately, there are
                             very few datasets meeting both my and the assignment requirements.")
                           , p("Therefore, I changed my project topic to forestry, which is still relevant to both environmentally friendly.
                           And then I found an good dataset from Te Rākau Rakau, Forestry New Zealand, regarding to New Zealand's forests [2].")
                           , p("[2] New Zealand\'s forests, Te Uru Rākau, Forestry New Zealand, retrieved from https://www.teururakau.govt.nz/news-and-resources/open-data-and-forecasting/forestry/new-zealands-forests/ on 
                             20th August 2019")
                       )
                     )
            )
            ## Closure - aboutProject
            )
        ################## *******             EDA *******              ##################
        , tabItem(tabName = "eda", sidebarLayout(
            sidebarPanel(width = 2
                            , checkboxInput("edaPlotCenter", "Center Data", value = FALSE)
                            , checkboxInput("edaPlotScale", "Scale Data", value = FALSE)
                            , sliderInput("edaPlotMultiplier", "Multiplier", min = 1.5, max = 5,value = 1.5)
            ## Closure - sidebarPanel
            )
            , mainPanel(
             tabsetPanel(
            ###########             Source Data Summary Tab              ###########
             tabPanel("Source Data Summary"
                      , htmlOutput("edaSourceSummary")
                      , div(
                          p("As summary shows, there exists missing data in pruning variable. Given this 
                            factor variable already has two levels of value and the number of missing 
                            data is 50, I will set the NA to be the 
                            third level - \"Not Available\"  ")
                      )
             )
             ###########             Source Data Visualization Tab              ###########
             , tabPanel("Source Data Visualization"
                , h3("Boxplot of source numeric variables")
                , plotOutput("edaSourceBoxplot")
                ###########             edaSourceBoxDesc              ###########
                , div("Unpruned.thinnings.m3.ha., Thinnings.m3.ha., Pulplog.thinnings.m3.ha. variables have 0 or 1 level of value.
                      Without Centering or Scaling data, variables Unpruned.logs.m3.ha., TRV.m3.ha., Pulplogs.m3.ha., Pruned.logs.m3.ha., Age.years. have very different value ranges.
                      With Centering and Scaling enabled, these variables are more normally distributed, except for Pruned.logs.m3.ha..
                      Pruned.logs.m3.ha. has a lot of values outside of maximum boundary. We shall have a further check to confirm if they are true outliers.
                      At the same time, first quartile and median of Pruned.logs.m3.ha. minimum value are very close")
                # , textOutput("edaSourceBoxDesc")
                , h3("Barchart of source factor variables")
                , h4("Hover on the bar to see full details!")
                , htmlOutput("edaSourceBarchart")
                ###########             edaSourceBarDesc              ###########
                , div(p("Wood.Supply.Region and Thinning variables have only one level of value.")
                ,p("Therefore, I believe they can be excluded in further analysis.")
                ,p("Severe level of Class imbalance exsits in all factor variables: Species, Pruning, Planting.coverage, Owner.size.
                   This would affect the model to be built in this project. Hence, I will watch out for class imbalance effect and
                   optimistic prediction accuracy when I am building the models.")
                # multinominal classification
                # , textOutput("edaSourceBarDesc")
                )
                )
            ###########             Source Missing Data Tab              ###########
            , tabPanel("Missing Data Plot"
                       , plotOutput("edaSourceMissingData")
                       , plotOutput("edaSourceMissDataPattern")
                       )
             ###########             Cleansed Data Summary Tab              ###########
             , tabPanel("Cleansed Data Summary"
                      , htmlOutput("edaCleansedSummary")
             )
             ###########             Cleansed Data Visualization Tab              ###########
             , tabPanel("Cleansed Data Visualization"
                        , h3("Boxplot of Cleansed numeric variables")
                        , plotOutput("edaCleansedBoxplot")
                        , h3("Frequency Barchart of Cleansed factor variables")
                        , h4("Hover on the bar to see full details!")
                        , htmlOutput("edaCleansedBarchart")
                        , textOutput("edaCleansedBarDesc")
             )
            ###########             Cleansed Missing Data Tab              ###########
            , tabPanel("Missing Data Check"
                       , plotOutput("edaCleansedMissingData")
                       , dataTableOutput("edaCleansedMissDT")
                       , div(
                           p("As the plot above shown, there exists missing data in pruning variable.")
                       )
            )
            ###########             Define Question Tab              ###########
            , tabPanel("Define the core question"
                        , div(
                            p("Now the first milestone - Basic Exploratory Data Analysis, has been reached.
                              We have obtained basic understanding to this data.")
                            , p("The second milestone is to setup the CORE question: what do I want to solve via completing this project?
                                As mentioned in the project background, this project was inspired from the Amazon fire, which relates
                                to one of the topics taht I am very interested in - environmental friendly.")
                            , p("Who are blah")
                        )
             )
             ###########             EDA Plan Tab              ###########
             # , tabPanel("What next?"
             #            , textOutput("edaPlan")
             #            )
            ## Closure - EDA mainPanel
            )
            ## Closure - EDA Tab tabsetPanel
            )
            ## Closure - EDA Tab sidebarLayout
            )
            ## Closure - EDA Tab
            )

        ################## *******             IMPUTATION *******              ##################
        , tabItem(tabName = "imputation"
                  , sidebarLayout(
                      sidebarPanel(width = 2
                                   , sliderInput("imputeTrainRatio", "Set the proportion of train data in Imputation"
                                                 , min = 60, max = 95, value = 75
                                                 , step = 5, post = "%"
                                   )
                                   , selectizeInput("imputeMethods", "Select to show corresponding imputation result"
                                                    , choices = c("kNN", "rPart", "MICE", "Recipe"), multiple = TRUE
                                                    , selected = c("kNN", "rPart", "MICE", "Recipe")
                                                    )
                                   ## Closure - sidebarPanel
                      )
                      , mainPanel(tabsetPanel(
                          ###########             Imputation Methods and Results Tab              ###########
                          tabPanel("Imputation Methods and Results"
                                   , h3("See below for the imputation result for selected methods")
                                   , h4("It may take a moment (approximately 20 seconds) to load and update result (when imputation training ratio is changed) - working on it!")
                                   , dataTableOutput("imputationAccuracyTable")
                                   , dataTableOutput("imputationResultTable")
                          )
                          ###########             Imputation Decision Tab              ###########
                          , tabPanel("Imputation Decision"
                                     # , h3("")
                                     , plotOutput("imputeResultBarchart")
                                     , h3("Decision")
                                     , p("My decision here is to use Recipe to impute missing values in Pruning variable given it has
                                         high accuarcy across all methods and its flexibility.")
                          )
                      )
                      ## Closure - Cleansed Data Tab tabsetPanel
                      )
                      ## Closure - Cleansed Data Tab sidebarLayout
                  )
                  ## Closure - Cleansed Data Tab
        )
        ################## *******             MODELLING *******              ##################
        , tabItem(tabName = "modelling"
                  , sidebarLayout(
                      sidebarPanel(width = 2
                                   , sliderInput("modelTrainRatio", "Set the proportion of train data"
                                                 , min = 60, max = 95, value = 75
                                                 , step = 5, post = "%"
                                                 )
                                   , selectizeInput("modelMethods", "Select to show corresponding imputation result"
                                                    , choices = c("Simple", "ROSE", "Weighted",  "Recipe"), multiple = TRUE
                                                    , selected = c("Simple", "ROSE", "Weighted", "Recipe")
                                                    # "SMOTE",
                                   )
                                   # , selectInput("modelMethods", "Select to show corresponding imputation result"
                                   #                  , choices = c("Simple", "ROSE")#, multiple = TRUE , "Weights", "SMOTE", "Recipe"
                                   #                  , selected = c("Simple", "ROSE")
                                   # )
                                   # modelWithWeights()
                                   # modelWithRecipe()
                                   # modelWithROSE()
                                   # modelWithSMOTE()
                        ## Closure - sidebarPanel
                      )
                      , mainPanel(tabsetPanel(
                          ###########             Modelling Methods and Results Tab              ###########
                          tabPanel("Modelling Methods and Results"
                                   , div(
                                       p("Methods tested are: Class Weighting (Weighted), Random Over 
                                      Sampling Examples (ROSE), Synthetic Minority Over-sampling Technique (SMOTE), ")
                                       , p("Unfortunately, SMOTE is not available in this application because of heavy 
                                       RAM consumption.")
                                       , p("However, relevant development code can be find in files.")
                                   )
                                   , h3("See below for the modelling result for selected methods")
                                   , h4("It may take a moment (approximately 20 seconds) to load and update result 
                                        (when modelling training ratio is changed) - working on it!")
                                   , dataTableOutput("modelAccuracyTable")
                                   , dataTableOutput("masterModelResultDT")
                                   
                          )
                          ###########             Simple Modelling Tab              ###########
                          , tabPanel("Simple Model"
                                     , verbatimTextOutput("modelSimpleTable")
                          )
                          ###########             Weighted Modelling Tab              ###########
                          , tabPanel("Weighted Model"
                                     , verbatimTextOutput("modelConfMatWeighted")
                          )
                          ###########             ROSE Modelling Tab              ###########
                          , tabPanel("ROSE Model"
                                     , verbatimTextOutput("modelConfMatROSE")
                          )
                          ###########             Recipe Modelling Tab              ###########
                          , tabPanel("Recipe Model"
                                     , verbatimTextOutput("modelConfMatRecipe")
                          ) 
                          ###########             Modelling Decision Tab              ###########
                          , tabPanel("Conclusion"
                                     , p("My decision here is to use Recipe to impute missing values in Pruning variable given it has
                                         high accuarcy across all methods and its flexibility.")
                          )
                      ## Closure - Modelling Tab tabsetPanel
                      )
                      ## Closure - Modelling Tab mainPanel
                      )
                      ## Closure - Modelling Tab sidebarLayout
                  )
                  ## Closure - Modelling Tab
        )
        ################## *******             CLASSIFICATION *******              ##################
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
                          )
                          ## Closure - Classification mainPanel
                      )
                      ## Closure - Classification Tab tabsetPanel
                      )
                      ## Closure - Classification Tab sidebarLayout
                  )
                  ## Closure - Classification Tab
        )
        
        ################## *******             PROJECT END *******              ##################
        , tabItem(tabName = "projectEnd", tabsetPanel(
            ###########             Remaining Work Tab              ###########
            tabPanel("What I have learnt"
                   , h2("Statistics")
                   , h3("Different imputation methods IN PRACTICE")
                   , p("I learnt a lot about imputation methods and their implementation via this project!")
                   , h3("Various modelling methods IN PRACTICE")
                   , p("I learnt a lot about imputation methods and their implementation via this project!")
                   , h3("A more appropriate Data Science project design")
                   , div(
                       p("blah")
                   )
                   , h2("Shiny")
                   , h3("Dynamic number of plots x googleVis Package")
                   , div(
                       p("I used googleVis package before but not in a very dynmaic manner. In this project, 
                         the googleVis plots are generated dynamically!")
                       , p("This means if the dataset has one extra factor variable, there will be one extra barchart
                           generated AUTOMATICALLY!")
                   )
                   , h3("More flexibilities in Shiny")
                   , div(
                       p("Previously, I write report text in server.R, then use the output function in ui.R for display purpose.
                         However, I found out I can write the text in ui.R with HTML tags! This is great because it has reduced
                         unreuqired code and efficient has improved, at the same time, it has further extended my HTML skill!")
                   )
                   , h2("Keep in mind of what you are doing!")
                   , div(
                       p("During the development, my modelling methods were 'not' working when I tested
                       in in Shiny application, thought they were working fine on local (with R session restarted).")
                       , p("I almost gave up on these modellings though it took me a while to write. But then I noticed
                           I used the wrong outcome variable to these methods!")
                       , p("Next time I will write down the question on a piece of paper to remind myself what
                           I am trying to solve so that I would not be lost because of heavy and intensive development.")
                   )
            )
            , tabPanel("Remaining Work"
                       , h2("What can be improved")
                       , div(
                           p("UI")
                           , p("Table column names")
                           , p("Efficient and run time of some components. Store objects that are less likely to change
                               locall?")
            ))
            , tabPanel("Potential Extension"
                     , div(
                         p("Working with companies within these categories to improve sustainability in New Zealand.")
                     )
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

        # , sidebarLayout(
        #     sidebarPanel(
        #         # input$edaPlotCenter, scale = input$edaPlotScale
        #         checkboxInput("edaPlotCenter", "Center Data", value = FALSE)         
        #     )
        #     , mainPanel(
        #        
###########             Tab Template              ###########
# , tabPanel("With"
#          , h3("blah")
# )
## Closure - Cleansed Data mainPanel