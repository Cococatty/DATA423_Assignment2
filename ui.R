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
        , menuItem("Classification Visualization", tabName = "classification", icon = icon("tree"))
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
                      , h2("It may take a few seconds to load depending on the RAM capacity.")
                      , h3("Efficient solution shall be developed.")
                      , htmlOutput("edaSourceSummary")
                      , div(
                          p("As summary shows, there exists missing data in pruning variable. Given this 
                            factor variable already has two levels of value and the number of missing 
                            data is 50, I will try to impute these rows by trying various imputation methods.")
                      )
             )
             ###########             Source Data Visualization Tab              ###########
             , tabPanel("Source Data Visualization"
                        , h2("It may take a few seconds to load depending on the RAM capacity.")
                        , h3("Efficient solution shall be developed.")
                , h3("Boxplot of source numeric variables")
                ###########             edaSourceBoxDesc              ###########
                , div("Unpruned.thinnings.m3.ha., Thinnings.m3.ha., Pulplog.thinnings.m3.ha. variables have 0 or 1 level of value.
                      Without Centering or Scaling data, variables Unpruned.logs.m3.ha., TRV.m3.ha., Pulplogs.m3.ha., Pruned.logs.m3.ha., Age.years. have very different value ranges.
                      With Centering and Scaling enabled, these variables are more normally distributed, except for Pruned.logs.m3.ha..
                      Pruned.logs.m3.ha. has a lot of values outside of maximum boundary. We shall have a further check to confirm if they are true outliers.
                      At the same time, first quartile and median of Pruned.logs.m3.ha. minimum value are very close")
                
                , plotOutput("edaSourceBoxplot")
                ###########             edaSourceBarDesc              ###########
                , h3("Barchart of source factor variables")
                , h4("Hover on the bar to see full details!")
                , div(p("Wood.Supply.Region and Thinning variables have only one level of value.")
                      ,p("Therefore, I believe they can be excluded in further analysis.")
                      ,p("Severe level of Class imbalance exsits in all factor variables: Species, Pruning, Planting.coverage, Owner.size.
                   This would affect the model to be built in this project. Hence, I will watch out for class imbalance effect and
                   optimistic prediction accuracy when I am building the models.")
                , htmlOutput("edaSourceBarchart")
                )
                )
            ###########             Source Missing Data Tab              ###########
            , tabPanel("Missing Data Plot"
                       , p("As discussed previously, there exists missing data and two variables only have one or zero
                           levels of values. Here I can confirm Unpruned.thinning.m3.ha and Pulplog.thinning.m3.ha variables
                           have no values. These variables can be excluded in analysis.")
                       , plotOutput("edaSourceMissingData")
                       , p("Plotting missing data pattern to confirm there is no relationship between the three variables that
                           have missing data. Unpruned.thinning.m3.ha and Pulplog.thinning.m3.ha variables missing data is Missing
                           Not at Random. Further reading data note, these variables are 'Otago/Southland only.'. This confirms my
                           understanding. Pruning variable is Missing at Random, hence, I will impute it.")
                       , plotOutput("edaSourceMissDataPattern")
                       )
             ###########             Cleansed Data Summary Tab              ###########
             , tabPanel("Cleansed Data Summary"
                        , p("With basic cleansing, the dataset is more efficient to analyse. The missing 
                            data is not fixed here just yet. I will try different models in the next section.")
                        , p("Class imbalance once again catches my attention due to it occurs in various variables.")
                      , htmlOutput("edaCleansedSummary")
             )
             ###########             Cleansed Data Visualization Tab              ###########
             , tabPanel("Cleansed Data Visualization"
                        , p("Through visualization, class imbalance is very obvious.")
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
                              I have obtained basic understanding to this data.")
                            , p("The second milestone is to setup the CORE question: what do I want to solve via completing this project?
                                As mentioned in the project background, this project was inspired from the Amazon fire, which relates
                                to one of the topics taht I am very interested in - environmental friendly.")
                            , p("Therefore, the core question in this project is: What trees are yielding the most?")
                            , p("From the core question, I define my outcome variable to be 'Planting coverage' variable, all other variables
                                that have more than one level of values to be predictors.")
                        )
             )
             
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
                                   , p("Imputation has train and test split ratio default to 0.75. However, users can change
                                       the ratio via slidebar on the left.")
                                   , dataTableOutput("imputationAccuracyTable")
                                   , p("To check the comparison between predicted and actual values, please see table below.")
                                   , p("From various imputation tests, I can see in general, kNN has the lowest accuracy, while Recipe maintains
                                       the highest accuracy.")
                                   , dataTableOutput("imputationResultTable")
                          )
                          ###########             Imputation Decision Tab              ###########
                          , tabPanel("Imputation Decision"
                                     , h3("The barchart below shows the accuracy comparsion via visualization. It would be updated
                                          when ratio is changed in the left-hand menu.")
                                     , p("My decision here is to use Recipe to impute missing values in Pruning variable given it has
                                         high accuarcy across all methods in multiple tests, and its flexibility and stability.")
                                     , plotOutput("imputeResultBarchart")
                                     , h3("Decision")
                                     
                      )
                      ## Closure - Cleansed Data Tab tabsetPanel
                      )
                      ## Closure - Cleansed Data Tab sidebarLayout
                  )
                  ## Closure - Cleansed Data Tab
        ))
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
                          tabPanel("Modelling Class Imbalanced Classification Data and Results"
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
                                   , div(
                                       p("Looking at the result, I tend to use Recipe modelling method because it
                                         has the highest accuracy among all, followed by Weighted, then ROSE and Simple.")
                                       , p("More details can be found within in tab.")
                                   )
                                   , dataTableOutput("modelAccuracyTable")
                                   , dataTableOutput("masterModelResultDT")
                                   
                          )
                          ###########             Simple Modelling Tab              ###########
                          , tabPanel("Simple Model"
                                     , p("As shown in the first tab, Simple multi-class classification does not have very 
                                         good accuracy.")
                                     , verbatimTextOutput("modelSimpleTable")
                          )
                          ###########             Weighted Modelling Tab              ###########
                          , tabPanel("Weighted Model"
                                     , p("Weighted model has better accuracy rate than Simple, but its accuracy rate is only at 0.64
                                         (2 decimal places) which is not good either.")
                                     , verbatimTextOutput("modelConfMatWeighted")
                          )
                          ###########             ROSE Modelling Tab              ###########
                          , tabPanel("ROSE Model"
                                     , p("ROSE model has similar accuracy rate as Simple, its Overall Statistics indicates
                                     its accuracy rate is approximately 0.55 (2 decimal places) which is not good.")
                                     , verbatimTextOutput("modelConfMatROSE")
                          )
                          ###########             Recipe Modelling Tab              ###########
                          , tabPanel("Recipe Model"
                                     , p("Recipe model has the best accuracy rate among all. Its Overall Statistics indicates
                                     its accuracy rate is approximately 0.73 (2 decimal places) which is reasonably good.")
                                     , verbatimTextOutput("modelConfMatRecipe")
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
                  # , tabsetPanel(
                      ###########             Cleansed Data Visualization Tab              ###########
                      , tabPanel("rPart Tree"
                               , plotOutput("treeRPart")
                               , div(
                                   h2("Terminology explained:")
                                   , div(
                                       p("Age of stands: For each crop type and regime, contributors were asked to 
                                           provide separate yield tables based on when the stand was planted.")
                                       , p("Radiata pine has 4 tending regimes: Pruned without production thinning, Unpruned without production thinning,
                                           Pruned with production thinning, Unpruned with production thinning.")
                                       , p("Douglas-fir forest has 2 tending regimes: Without production thinning, With production thinning.")
                                       , p("Log type specifications: Pruned, Unpruned, Pulp.")
                                   )
                                   , h2("Plot Reading")
                                   , p("If the tree is planted per-1990 and Pruned, then it has 33% of probability of being in all stands category.")
                                   , p("There are approximately 47% of the trees in this survey are planted pre-1990. This is concluded from the bottom layer of the tree.")
                                   , p("Trees planted all stand has the second largest percentage in this analysis, approximately 33%.")
                                   , p("Trees planted post-1989 has the least number in this survey. The approximate percentage is 21%.")
                                   , p("In this model, the key figures of numeric variables are 170 (Pulplogs.m3.ha), 421 (Unpruned.log.m3.ha), 468 (TRV.m3.ha),
                                       267 (Unpruned.logs.m3.ha), 65 (Pluplogs.m3.ha). These figures are used to determined the directions a decision is following towards.")
                               )
                               ## Closure - rPart Tree Tab
                               )
                          
                      # )
                      ## Closure - Classification Tab tabsetPanel
                      # )
                  ## Closure - Classification Tab
        )
        
        ################## *******             PROJECT END *******              ##################
        , tabItem(tabName = "projectEnd", tabsetPanel(
        ###########             What I have learnt Tab              ###########
        tabPanel("What I have learnt"
               , h2("Statistics")
               , h3("Different imputation methods IN PRACTICE")
               , p("I learnt a lot about imputation methods and their implementation via this project!")
               , h3("Various modelling methods IN PRACTICE")
               , p("I learnt a lot about imputation methods and their implementation via this project!")
               , h3("A more appropriate Data Science project design")
               , div(
                   p("I knew Data Science involves Analysis (Statistical analysis) and programming (Computer Science).
                   But I did not pay much attention to the beginning and the end sections.")
                   , p("After completing this project, now I know at the beginning of a project, you MUST understand 
                   the project background, propose the core question, and then find relevant data. EDA is not a simple 
                   concept either. It turns out to be very important because it shows the hidden characteristic 
                   (if right methods are applied!), it would guide you what to do next (missing data? class imbalance?).
                   EDA is a skilled techniques because it requires attention to details, so that you can use the right 
                   forms to convey obvious messages to yourself.")
                   , p("I had been underestimated the power of EDA, thought it is a generic process to ALL projects. 
                   But in this journey, I found EDA is similar to DNA - a pair might share exactly the same content, 
                   but the chance is pretty low (good for confidence interval estimation thou!).")
                   , p("I'm happy that I no longer using the same EDA techniques over my different projects, but 
                       start tailoring and thinking over it for EACH project!")
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
               , h2("Useful Sites")
               , div(
                   p("I found some very useful sites during the development.")
                   , p("https://topepo.github.io/caret/train-models-by-tag.html#Robust_Model")
                   , p("This site contains A LOT of methods that I can try in the future in terms of modelling.")
               )
        )
        ###########             Remaining Work Tab              ###########
        , tabPanel("Remaining Work"
                   , h2("What can be improved")
                   , div(
                       p("UI - this is not very good at the moment.")
                       , p("Table column names - make them more sensible.")
                       , p("Efficient and run time of some components. Store objects that are less likely to change
                           locall?")
                       )
                   )
        ###########             Potential Extension Tab              ###########
        , tabPanel("Potential Extension"
                 , div(
                     p("Working with companies within these categories to improve sustainability in New Zealand.")
                 )
        )
        ###########             The End and The Beginning Tab              ###########
        , tabPanel("The End and The Beginning"
                   , div(
                       p("This could be the end of this assignment project, but it would not be the end to this topic
                         and project!")
                       , p("I am hoping to extend this project during my personal time to further practice my skills
                           and contribute to something I'm passionate about - environmentally friendly and Data Science!")
                       , p("Thank you Nick for organizing this assignment, I love it! Though it was hard at the beginning,
                           but I feel the goodness within it as I carried on. :) ")
                   )
            )
        # )
                  ## Closure - tabsetPanel
        )
            ## ------------------------------------------------------------------------------------------------------------------------------ ##
             ## Closure - Project End Tab
            )
        ## Closure - tabItems
        )
        ## Closure - fluidRow
        )
        ## Closure - dashboardBody
    )
    ## Closure - dashboardPage
    )
    ## Closure - shinyUI
)