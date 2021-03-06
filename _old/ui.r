################################################################################
#                                                                              #
#   User Interface for Should I Airbnb My Property                             #                                               #
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
     
    ## Select Input Types  
           
    selectInput(
      'opt', 'Select Options to Update',
      c('Analysis Parameters' = 'anys',
        'Data Parameters' = 'data'
        )
    ),

    # Data Panel
    conditionalPanel(
      condition = "input.opt == 'data'",
      
      ## Select Data to Include
      h3("Select those to Include in Analysis:"),
      hr(),
      h4("Property Type"),
      checkboxInput('apt', "Apartments", TRUE),
      checkboxInput('house', "House", TRUE),
      h4("Geographic Submarket"),
      checkboxInput('core', "City Core", TRUE),
      checkboxInput('city', "City", TRUE),
      checkboxInput('suburban', "Suburban", TRUE),
      checkboxInput('rural', "Rural", TRUE),
      checkboxInput('beach', "Beach", TRUE),
      h4("Product Type"),
      checkboxInput('bb11', "1 Bed / 1 Bath", TRUE),
      checkboxInput('bb21', "2 Bed / 1 Bath", TRUE),
      checkboxInput('bb22', "2 Bed / 2 Bath", TRUE),
      checkboxInput('bb31', "3 Bed / 1 Bath", TRUE),
      checkboxInput('bb32', "3 Bed / 2 Bath", TRUE),
      checkboxInput('bb42', "4 Bed / 2 Bath", TRUE),
      h4("Host Category"),
      checkboxInput('hostps', "Hosts - Profit Seekers", TRUE),
      checkboxInput('hostos', "Hosts - Opportunistic Sharers", TRUE),
      checkboxInput('hostmu', "Hosts - Multiplatform Users", TRUE),
      checkboxInput('hostun', "Host - Unknowns", TRUE)
      
    ), # Ends Conditional Panel
    
    # Analysis Panel
    conditionalPanel(
      
      condition = "input.opt == 'anys'", 
      
      selectInput('rev.type', 'Select Revenue Type to Use',
                  c('Actual' = 'act', 
                    'Potential' = 'pot')
      ),
      
      selectInput('transform', 'Use Percentiles?',
                  c('No' = 'Raw', 
                    'Yes' = 'Pcntl')
      ),
      
      selectInput('facet.var', 'Choose Comparison Variable',
                  c('None'= 'none',
                    'Property Type' = 'type', 
                    'Geographic Submarket' = 'geo.mrkt',
                    'Product (Bed/Bath)' = 'bedbath',
                    'Host Type' = 'host.type'),
                  selected='none'
      ),
      
      checkboxInput('svm', "Use SVM Heat Map", TRUE)
      
    ) # Ends conditional panel
   ), # Ends sidebarPanel(
    
## Show main panel -----------------------------------------------------  
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
     tabsetPanel(
      tabPanel("Preference Summary", tableOutput("prefTable")),
      tabPanel("Occupancy Plot", plotOutput("occplot")),
      tabPanel("Heat map", plotOutput("hmplot")),
      tabPanel("Location map", plotOutput("locplot")),
      tabPanel("Logistic Model", tableOutput("coefTable")),
      tabPanel("Revenue Plot", plotOutput("rdplot"))
      
     ) # Ends Tab Set Panel  
    ) # Ends MainPanel(
   ) # Ends sidebarLayout(
  ) # Ends FluidPage(
 ) # Ends ShinyUI(
  