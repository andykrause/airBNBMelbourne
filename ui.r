################################################################################
#                                                                              #
#   User Interface for Zesimate MD Shiny App                                   #                                               #
#                                                                              #
################################################################################

# Set library(s)

  library(shiny)

  shinyUI(fluidPage(

    # Application title
    titlePanel("Airbnb In Melbourne:  Analysis of Returns"),

    # Sidebar with controls to select the model and the variable to display

    sidebarLayout(
      sidebarPanel(
        
    ## Recalculate Button
        
        actionButton('plot', "Plot/Re-Plot Data", icon("paper-plane"), 
                     style="color: #fff; background-color: #FF5A5F; border-color: #FF5A5F"),
        hr(),
        
    ## Select county to view --------------------------------------------------
        
        selectInput('county', 'Select a Market to Analyze',
          c('Choose a market' = 'none', 
            'All properites' = 'all',
            'Houses only' = 'houses',
            'Apartments only' = 'apts',
            'City-core' = 'city-core',
            'City' = 'city',
            'Suburban' = 'suburban',
            'Rural' = 'rural',
            'Beach' = 'beach')
        ),
        hr(),
        hr()#,
    #     
    # ## Select which options to alter ------------------------------------------
    #     
    #     selectInput(
    #       'opt', 'Select Options to Update',
    #       c('Choose a option' = 'none',
    #         'Graphical Aesthetics' = 'aeth',
    #         'Data Specifics' = 'spec')
    #     ),
    #     hr(),
    #     hr(),
    #     
    #  ## If 'aeth' option chosen ----------------------------------------------
    # 
    #     conditionalPanel(
    #      condition = "input.opt == 'aeth'",
    #   
    #    ## Select Y value
    #      
    #       selectInput('yVar', "Select Y Variable",
    #         c('Prediction Error' = "raw.error")
    #       ), # Ends Select Y Value
    #      
    #    ## Select X variable
    #       
    #      selectInput("xVar", "Select X Variable",
    #         c('Data/Model/Estimator' = 'dme',
    #           'Prediction Month' = 'pred.time',
    #           'Forecast Distance' = 'time.diff')
    #         ), # Ends Select X Variable
    #    
    #    ## Select Color variable
    #     selectInput('col', "Select Color Variable",
    #                  c('None' = 'none',
    #                    'Data/Model/Estimator' = 'dme',
    #                    'Hierarchical Blend' = 'hrb')
    #    ), # Ends Color colour
    #    
    #    # Select Facet Variable
    #    selectInput("f1Var", "Select Facet Variable",
    #                c('none' = 'none',
    #                  'Market Level' = 'market',
    #                  'Model' = 'model',
    #                  'Estimator' = 'est')
    #    ) # Ends Select Facet Variable
    #    
    #   ), # Ends conditionPanel('aeth'
    #   
    # ## if 'spec' option chosen ---------------------------------------------
    # 
    #   conditionalPanel(condition = "input.opt == 'spec'",
    #   
    #     sliderInput("timeRange", "Time Range in Months (Jan 06 = 1)",
    #                  min = 1, max = 121, 
    #                  value = c(1, 121), step=1),    
    #     
    #     # Select Prediction Types to consider
    #     selectInput("predType", "Select Prediction Type",
    #                 c('Forecast' = 'forecast',
    #                   'Holdout (80/20)' = 'holdout',
    #                   'Both' = 'both')
    #     ),
    #                    
    #     # Use Absolute Value of error or not
    #     checkboxInput('abserr', "Use Absolute Value of Errors", TRUE),
    #     
    #     # Consider Hierarchical Blended Models
    #     checkboxInput('hrb', "Include Hierarchical Blend", TRUE),
    #     
    #     # Consider Specific Model Types
    #     checkboxInput('useMTB', "Include Model Blending", TRUE),
    #     checkboxInput('useAVR', "Include Assessed Value Models", TRUE),
    #     checkboxInput('useRME', "Include Relative Match Models", TRUE),
    #     checkboxInput('useSRS', "Include Repeat Sale Models", TRUE),
    #     checkboxInput('useHED', "Include Hedonic Models", TRUE),
    #     checkboxInput('useBAS', "Include Base Estimators", TRUE),
    #     checkboxInput('useROB', "Include Robust Estimators", TRUE),
    #     checkboxInput('useWGT', "Include Weighted Estimators", TRUE)
    #     
    #   ) # Ends conditional panel
    ), # Ends sidebarPanel(
    
## Show main panel -----------------------------------------------------  
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      tabPanel("Plot", plotOutput("compPlot"))
    ) # Ends MainPanel(
   ) # Ends sidebarLayout(
  ) # Ends FluidPage(
 ) # Ends ShinyUI(
  